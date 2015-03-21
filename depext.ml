(* misc functions *)

let debug = ref false

let lines_of_channel ic =
  let rec aux acc =
    let line = try Some (input_line ic) with End_of_file -> None in
    match line with
    | Some s -> aux (s::acc)
    | None -> acc
  in
  List.rev (aux [])

let lines_of_command c =
  if !debug then Printf.eprintf "+ %s\n%!" c;
  let ic = Unix.open_process_in c in
  let lines = lines_of_channel ic in
  close_in ic;
  lines

let lines_of_file f =
  let ic = open_in f in
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
  let cmd = Printf.sprintf "command -v %s" c in
  try Sys.command cmd = 0 with Sys_error _ -> false

let run_command c =
  let c = String.concat " " (List.map (Printf.sprintf "%s") c) in
  if !debug then Printf.eprintf "+ %s\n%!" c;
  Unix.system c

(* system detection *)

let arch () =
  match command_output "uname -m" with
  | "x86_64" -> `X86_64
  | "x86" | "i386" | "i586" | "i686" -> `X86
  | "armv7l" -> `Arm7
  | "PPC" | "PowerPC" -> `PPC
  | s -> `Other s

let os () = match Sys.os_type with
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

let distribution = function
  | `Darwin ->
    if has_command "brew" then Some `Homebrew
    else if has_command "port" then Some `Macports
    else None
  | `Linux ->
    (try
       let name =
         if has_command "lsb_release" then
           command_output "lsb_release -i -s"
         else
         let release_file =
           List.find Sys.file_exists
             ["/etc/redhat-release"; "/etc/centos-release"; "/etc/issue"]
         in
         List.hd (string_split ' ' (List.hd (lines_of_file release_file)))
       in
       match String.lowercase name with
       | "debian" -> Some `Debian
       | "ubuntu" -> Some `Ubuntu
       | "centos" -> Some `Centos
       | "fedora" -> Some `Fedora
       | "mageia" -> Some `Mageia
       | s -> Some (`Other s)
     with Not_found | Failure _ -> None)
  | _ -> None

(* generate OPAM depexts flags *)

let archflags = function
  | `X86_64 -> ["x86_64"]
  | `X86 -> ["x86"]
  | `Arm7 -> ["arm";"armv7"]
  | `PPC -> ["ppc"]
  | `Other s -> [String.lowercase s]

let osflags = function
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

let distrflags = function
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

let install_packages_command distribution packages =
  match distribution with
  | Some `Homebrew ->
    "brew"::"install"::packages
  | Some `Macports ->
    "port"::"install"::packages
  | Some (`Debian | `Ubuntu) ->
    "apt-get"::"install"::"-qq"::"-yy"::packages
  | Some (`Centos | `Fedora | `Mageia) ->
    "yum"::"install"::"-y"::packages
  | Some `FreeBSD ->
    "pkg"::"install"::packages
  | Some (`OpenBSD | `NetBSD) ->
    "pkg_add"::packages
  | Some (`Other d) ->
    failwith ("Sorry, don't know how to install packages on your " ^ d ^ " system")
  | None ->
    failwith "Sorry, don't know how to install packages on your system"

let sudo os distribution cmd = match os, distribution with
  | (`Linux | `Unix | `FreeBSD | `OpenBSD | `NetBSD | `Dragonfly), _
  | `Darwin, Some `Macports ->
    (* not sure about this list *)
    if Unix.getuid () <> 0 then (
      Printf.printf "Not running as root, \
                     system installation will be done through \"sudo\"\n%!";
      "sudo"::cmd
    ) else cmd
  | _ -> cmd

let install os distribution = function
  | [] -> ()
  | os_packages ->
    let cmd = install_packages_command distribution os_packages in
    let cmd = sudo os distribution cmd in
    match run_command cmd with
    | Unix.WEXITED 0 ->
      Printf.printf "# OS packages installation successful\n%!"
    | _ -> failwith "OS package installation failed"

let run_source_scripts = function
  | [] -> ()
  | source_urls ->
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
        match run_command [cmd] with
        | Unix.WEXITED 0 -> ()
        | _ -> failwith (Printf.sprintf "Command %S failed" cmd))
      commands;
    Printf.printf "Source installation scripts run successfully\n%!"


(* Command-line handling *)

let main print_flags list short no_sources debug_arg install_arg opam_packages =
  if debug_arg then debug := true;
  let arch = arch () in
  let os = os () in
  let distribution = distribution os in
  let flags = archflags arch @ osflags os @ distrflags distribution in
  if print_flags then
    (if short then List.iter print_endline flags else
       Printf.printf "# Depexts flags detected on this system: %s\n"
         (String.concat " " flags);
     exit 0);
  if not short then
    Printf.printf "# Detecting depexts using flags: %s\n%!"
      (String.concat " " flags);
  let os_packages = depexts flags opam_packages in
  if short then List.iter print_endline os_packages
  else if os_packages <> [] then
    Printf.printf "# The following system packages are needed:\n#  - %s\n%!"
      (String.concat "\n#  - " os_packages)
  else if list then print_endline "# No required system packages found";
  if list then exit 0;
  let source_urls =
    if no_sources then [] else
      List.filter (fun s -> not (List.mem s os_packages))
        (depexts (sourceflags @ flags) opam_packages)
  in
  if source_urls <> [] && not short then
    Printf.printf "# The following scripts need to be run:\n#  - %s\n%!"
      (String.concat "\n#  - " source_urls);
  if os_packages = [] && source_urls = [] then
    Printf.printf "# No extra OS packages requirements found.\n%!";
  install os distribution os_packages;
  run_source_scripts source_urls;
  if install_arg && opam_packages <> [] then
    (Printf.printf "# Now letting OPAM install the packages";
     Unix.execvp "opam" (Array.of_list ("opam"::"install"::opam_packages)))

open Cmdliner

let packages_arg =
  Arg.(value & pos_all string [] &
       info ~docv:"PACKAGES"
         ~doc:"OPAM packages to install external dependencies for. \
               All installed packages if omitted" [])

let print_flags_arg =
  Arg.(value & flag &
       info ~doc:"Only display the inferred \"depexts\" flags" ["flags"])

let list_arg =
  Arg.(value & flag &
       info ~doc:"Only list the system packages needed" ["l";"list"])

let short_arg =
  Arg.(value & flag &
       info ~doc:"Only output the raw item lists" ["s";"short"])

let no_sources_arg =
  Arg.(value & flag &
       info ~doc:"Don't handle remote source scripts" ["no-sources"])

let debug_arg =
  Arg.(value & flag &
       info ~doc:"Print commands that are run by the program" ["d";"debug"])

let install_arg =
  Arg.(value & flag &
       info ~doc:"Install the packages through \"opam install\" after \
                  installing external dependencies" ["i";"install"])

let command =
  let man = [
    `S "DESCRIPTION";
    `P "opam-depext is a simple program intended to facilitate the interaction \
        between OPAM packages and the host package management system. It can \
        perform OS and distribution detection, query OPAM for the right \
        external dependencies on a set of packages, and call the OS's package \
        manager in the appropriate way to install then.";
    `S "COPYRIGHT";
    `P "opam-depext is written by Louis Gesbert <louis.gesbert@ocamlpro.com>, \
        copyright OCamlPro 2014-2015, \
        distributed under the terms of the LGPL v3 with linking exception. \
        Full source available at $(i,https://github.com/AltGr/opam-depext)";
    `S "BUGS";
    `P "Bugs are tracked at $(i,https://github.com/AltGr/opam-depext/issues).";
  ] in
  let doc = "Query and install external dependencies of OPAM packages" in
  Term.(pure main $ print_flags_arg $ list_arg $ short_arg $
        no_sources_arg $ debug_arg $ install_arg $
        packages_arg),
  Term.info "opam-depext" ~version:"0.3" ~doc ~man

let () =
  match Term.eval command with
  | `Ok () | `Version | `Help -> exit 0
  | `Error (`Parse | `Term) -> exit 2
  | `Error `Exn -> exit 1
