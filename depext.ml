(* misc functions *)

let lines_of_channel ic =
  let rec aux acc =
    match input_line ic with
    | s -> aux (s::acc)
    | exception End_of_file -> acc
  in
  List.rev (aux [])

let lines_of_command c =
  let ic = Unix.open_process_in c in
  let lines = lines_of_channel ic in
  close_in ic;
  lines

let command_output c =
  match lines_of_command c with
  | [s] -> s
  | _ -> failwith (Printf.sprintf "Command %S failed" c)

let string_split char str =
  let rec aux pos =
    try
      let i = String.index_from str pos char in
      String.sub str pos (i - pos) :: aux (succ i)
    with Not_found | Invalid_argument _ ->
        let l = String.length str in
        [ String.sub str pos (l - pos) ]
  in
  aux 0

let has_command c =
  let cmd = Printf.sprintf "/bin/sh -c command -v %s" c in
  try Sys.command cmd = 0 with Sys_error _ -> false

(* system detection *)

let arch =
  match command_output "uname -m" with
  | "x86_64" -> `X86_64
  | "x86" | "i386" | "i586" | "i686" -> `X86
  | "armv7l" -> `Arm7
  | "PPC" | "PowerPC" -> `PPC
  | s -> `Other s

let os = match Sys.os_type with
  | "Unix" ->
    (match command_output "uname -s" with
     | "Darwin"    -> `Darwin
     | "Linux"     -> `Linux
     | "FreeBSD"   -> `FreeBSD
     | "OpenBSD"   -> `OpenBSD
     | "NetBSD"    -> `NetBSD
     | "DragonFly" -> `DragonFly
     | _           -> `Unix)
  | "Win32"  -> `Win32
  | "Cygwin" -> `Cygwin
  | s        -> `Other s

let distribution = match os with
  | `Darwin -> Some `Homebrew (* Check first, macports ? *)
  | `Linux ->
    (match command_output "lsb_release -i -s" with
     | "Debian" -> Some `Debian
     | "Ubuntu" -> Some `Ubuntu
     | "Centos" -> Some `Centos
     | "Fedora" -> Some `Fedora
     | "Mageia" -> Some `Mageia
     | s -> Some (`Other s))
  | _ -> None

(* OPAM depexts flags *)

let archflags =
  match arch with
  | `X86_64 -> ["x86_64"]
  | `X86 -> ["x86"]
  | `Arm7 -> ["arm";"armv7"]
  | `PPC -> ["ppc"]
  | `Other s -> [String.lowercase s]

let osflags =
  match os with
  | `Darwin -> ["osx"]
  | `Linux -> ["linux"]
  | `Unix -> ["unix"]
  | `FreeBSD -> ["bsd";"freebsd"]
  | `OpenBSD -> ["bsd";"openbsd"]
  | `NetBSD -> ["bsd";"netbsd"]
  | `DragonFly -> ["bsd";"dragonfly"]
  | `Win32 -> ["mswindows";"win32"]
  | `Cygwin -> ["mswindows";"cygwin"]
  | `Other s -> [String.lowercase s]

let distrflags =
  match distribution with
  | Some `Homebrew -> ["homebrew"]
  | Some `Macports -> ["macports"]
  | Some `Debian -> ["debian"]
  | Some `Ubuntu -> ["ubuntu"]
  | Some `Centos -> ["centos"]
  | Some `Fedora -> ["fedora"]
  | Some `Mageia -> ["mageia"]
  | Some (`Other s) -> [String.lowercase s]
  | None -> []

(* current OPAM intf doesn't allow to filter depexts that have a given flag
   (only depexts having any subset of the given flags) so we'll need to diff
   since the "source" flag indicates a different format... *)
let sourceflags = ["source"]

(* processing *)

let depexts flags opam_packages =
  let c =
    (* Two options here. list is lighter and doesn't require a lock, which might
       be best if this is run as root *)
    (* {[ Printf.sprintf "opam install --external=%s %s"
           (String.concat "," flags)
           (String.concat "," opam_packages) ]} *)
    Printf.sprintf "opam list --safe --recursive --external=%s --required-by=%s"
      (String.concat "," flags)
      (String.concat "," opam_packages)
  in
  let s = lines_of_command c in
  let lines = List.filter (fun s -> String.length s > 0 && s.[0] <> '#') s in
  List.flatten (List.map (string_split ' ') lines)

let install_packages_command packages =
  match distribution with
  | Some (`Debian | `Ubuntu) ->
    "apt-get"::"install"::"-qq"::"-yy"::packages
  | Some (`Centos | `Fedora | `Mageia) ->
    "yum"::"install"::"-y"::packages
  | Some `Homebrew ->
    "brew"::"install"::packages (* needs check *)
  | _ -> failwith "Sorry, don't know how to install packages on your system"

let () =
  let opam_packages = List.tl (Array.to_list Sys.argv) in
  let flags = archflags @ osflags @ distrflags in
  if Array.length Sys.argv < 2 then (
    Printf.printf "# depexts flags detected on this system:\n%s\n%!"
      (String.concat "\n" flags);
    exit 0
  );
  Printf.printf "Detecting depexts using flags: %s\n%!"
    (String.concat " " flags);
  let os_packages = depexts flags opam_packages in
  let source_urls =
    List.filter (fun s -> not (List.mem s os_packages))
      (depexts (sourceflags @ flags) opam_packages)
  in
  if os_packages <> [] then
    Printf.printf "The following system packages are needed:\n  - %s\n%!"
      (String.concat "\n  - " os_packages);
  if source_urls <> [] then
    Printf.printf "The following scripts need to be run:\n  - %s\n%!"
      (String.concat "\n  - " source_urls);
  if os_packages = [] && source_urls = [] then
    Printf.printf "No extra OS packages requirements found.\n%!";
  if os_packages <> [] then (
    let cmd = install_packages_command os_packages in
    let cmd = match os with
      | `Linux | `Unix | `FreeBSD | `OpenBSD | `NetBSD | `Dragonfly ->
        (* not sure about this list. Does OSX do sudo ? *)
        if Unix.getuid () <> 0 then (
          Printf.printf "System installation will be done through \"sudo\"\n%!";
          "sudo"::cmd
        )else cmd
      | _ -> cmd
    in
    match Unix.system (String.concat " " cmd) with
    | Unix.WEXITED 0 -> Printf.printf "OS packages installation done\n%!"
    | _ -> failwith "OS package installation failed"
  );
  if source_urls <> [] then (
    let commands =
      (* OPAM supports (and requires) either, by doing it too we ensure that we
         don't need extra depexts *)
      if has_command "curl" then
        (* This still feels a bit frightening, said this way. *)
        List.map (Printf.sprintf "curl -L \"%s\" | bash -ex -") source_urls
      else
        List.map (Printf.sprintf "wget -O - \"%s\" | bash -ex -") source_urls
    in
    List.iter (fun cmd ->
        match Unix.system cmd with
        | Unix.WEXITED 0 -> ()
        | _ -> failwith (Printf.sprintf "Command %S failed" cmd))
      commands;
    Printf.printf "Source installation scripts run successfully\n%!"
  )
