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
  let cmd = Printf.sprintf "command -v %s >/dev/null" c in
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
         let os_release_files = ["/etc/os-release"; "/usr/lib/os-release"] in
         if List.exists Sys.file_exists os_release_files then
           let file = List.find Sys.file_exists os_release_files in
           let cmd = Printf.sprintf ". %s && echo $ID" file in
           match command_output cmd with
           | "" -> raise (Failure ("Parsing " ^ file))
           | id -> id
         else if has_command "lsb_release" then
           command_output "lsb_release -i -s"
         else let release_file = List.find Sys.file_exists
                                           ["/etc/redhat-release"; "/etc/centos-release";
                                            "/etc/gentoo-release"; "/etc/issue"]
              in (List.hd (string_split ' ' (List.hd (lines_of_file release_file))))
       in
       match String.lowercase name with
       | "debian" -> Some `Debian
       | "ubuntu" -> Some `Ubuntu
       | "centos" -> Some `Centos
       | "fedora" -> Some `Fedora
       | "mageia" -> Some `Mageia
       | "gentoo" -> Some `Gentoo
       | "alpine" -> Some `Alpine
       | "arch" -> Some `Archlinux
       | "rhel" -> Some `RHEL
       | s -> Some (`Other s)
     with Not_found | Failure _ -> None)
  | `OpenBSD -> Some `OpenBSD
  | `FreeBSD -> Some `FreeBSD
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
  | Some `RHEL -> ["rhel"]
  | Some `Mageia -> ["mageia"]
  | Some `Alpine -> ["alpine"]
  | Some `Archlinux -> ["archlinux"]
  | Some `Gentoo -> ["gentoo"]
  | Some `OpenBSD -> ["openbsd"]
  | Some `FreeBSD -> ["freebsd"]
  | Some (`Other s) -> [String.lowercase s]
  | None -> []

(* current OPAM intf doesn't allow to filter depexts that have a given flag
   (only depexts having any subset of the given flags) so we'll need to diff
   since the "source" flag indicates a different format... *)
let sourceflags = ["source"]

(* processing *)

let opam_version = lazy (
  command_output "opam --version"
)

let depexts flags opam_packages =
  let c =
    if String.sub (Lazy.force opam_version) 0 4 = "1.1." then
      (* backwards-compatible command *)
      Printf.sprintf "opam install --external=%s %s"
        (String.concat "," flags)
        (String.concat "," opam_packages)
    else
    (* this is lighter, more general and doesn't require a lock. But only on
       newer opams *)
    Printf.sprintf "opam list --safe --recursive --external=%s --required-by=%s"
      (String.concat "," flags)
      (String.concat "," opam_packages)
  in
  let s = lines_of_command c in
  let lines = List.filter (fun s -> String.length s > 0 && s.[0] <> '#') s in
  List.flatten (List.map (string_split ' ') lines)

let install_packages_commands ~interactive distribution packages =
  let yes opt r =
    if not interactive then opt @ r else r
  in
  match distribution with
  | Some `Homebrew ->
    ["brew"::"install"::packages]
  | Some `Macports ->
    ["port"::"install"::packages]
  | Some (`Debian | `Ubuntu) ->
    ["apt-get"::"install"::yes ["-qq"; "-yy"] packages]
  | Some (`Centos | `Fedora | `Mageia | `RHEL) ->
    ["yum"::"install"::yes ["-y"] packages;
     "rpm"::"-q"::packages]
  | Some `FreeBSD ->
    ["pkg"::"install"::packages]
  | Some (`OpenBSD | `NetBSD) ->
    ["pkg_add"::packages]
  | Some `Archlinux ->
    ["pacman"::"-S"::packages]
  | Some `Gentoo ->
    ["emerge"::packages]
  | Some `Alpine ->
    ["apk"::"add"::packages]
  | Some (`Other d) ->
    failwith ("Sorry, don't know how to install packages on your " ^ d ^ " system")
  | None ->
    failwith "Sorry, don't know how to install packages on your system"

let update_command = function
  | Some (`Debian | `Ubuntu) ->
     ["apt-get";"update"]
  | Some `Homebrew ->
     ["brew"; "update"]
  | Some (`Centos | `Fedora | `Mageia | `RHEL) ->
     ["yum"; "-y"; "update"]
  | Some `Archlinux ->
     ["pacman"; "-S"]
  | Some `Gentoo ->
     ["emerge"; "-u"]
  | Some `Alpine ->
     ["apk"; "update"]
  | _ -> ["echo"; "Skipping system update on this platform."]

exception Signaled_or_stopped of string * Unix.process_status

(* filter 'packages' to retain only the installed ones *)
let get_installed_packages distribution (packages: string list): string list =
  match distribution with
  | Some `Homebrew ->
    let lines = try lines_of_command "brew list" with _ -> [] in
    let installed = List.flatten (List.map (string_split ' ') lines) in
    List.filter (fun p -> List.mem p packages) installed
  | Some (`Debian | `Ubuntu) ->
    let cmd =
      (* ${db:Status-Status} would give only the column we're interested in, but
         it's quite new in dpkg-query. *)
      String.concat " "
        ("dpkg-query -W -f '${Package} ${Status}\\n'" :: packages
         @ ["2>/dev/null"])
    in
    let lines = try lines_of_command cmd with _ -> [] in
    List.fold_left
      (fun acc l -> match string_split ' ' l with
         | [pkg;_;_;"installed"] -> pkg :: acc
         | _ -> acc)
      [] lines
  | Some (`Centos | `Fedora | `Mageia | `Archlinux| `Gentoo | `Alpine | `RHEL) ->
    let query_command_prefix = match distribution with
      | Some (`Centos | `Fedora | `Mageia | `RHEL) -> "rpm -qi "
      | Some `Archlinux -> "pacman -Q "
      | Some `Gentoo -> "equery list "
      | Some `Alpine -> "apk info -e "
      | _ -> assert(false)
    in
    List.filter
      (fun pkg_name ->
         let cmd = query_command_prefix ^ pkg_name ^ " 2>/dev/null" in
         match Unix.system cmd with
         | Unix.WEXITED 0 -> true (* installed *)
         | Unix.WEXITED 1 -> false (* not installed *)
         | exit_status -> raise (Signaled_or_stopped (cmd, exit_status))
      ) packages
  | Some `FreeBSD ->
    let installed = try lines_of_command "pkg query %n" with _ -> [] in
    List.filter (fun p -> List.mem p packages) installed
  (* todo *)
  | Some `Macports -> []
  | Some (`OpenBSD | `NetBSD) -> []
  | Some (`Other _) | None -> []

let sudo os distribution cmd = match os, distribution with
  | (`Linux | `Unix | `FreeBSD | `OpenBSD | `NetBSD | `Dragonfly), _
  | `Darwin, Some `Macports ->
    (* not sure about this list *)
    if Unix.getuid () <> 0 then (
      Printf.printf "Not running as root, \
                     the following command will be run through \"sudo\":\n\
                    \    %s\n%!"
        (String.concat " " cmd);
      "sudo"::cmd
    ) else cmd
  | _ -> cmd

let update os distribution =
  let cmd = update_command distribution in
  let cmd = sudo os distribution cmd in
  match run_command cmd with
  | Unix.WEXITED 0 ->
    Printf.printf "# OS package update successful\n%!"
  | _ -> failwith "OS package update failed"

let install ~interactive os distribution = function
  | [] -> ()
  | os_packages ->
    let cmds =
      install_packages_commands ~interactive distribution os_packages
    in
    let cmds = List.map (sudo os distribution) cmds in
    let is_success r = (r = Unix.WEXITED 0) in
    let ok =
      List.fold_left (fun ok cmd ->
          if ok then is_success (run_command cmd) else false)
        true cmds
    in
    if ok then Printf.printf "# OS packages installation successful\n%!"
    else failwith "OS package installation failed"

let run_source_scripts = function
  | [] -> ()
  | source_urls ->
    let commands =
      (* OPAM supports (and requires) either, by doing it too we ensure that we
         don't need extra depexts *)
      if has_command "curl" then
        (* This still feels a bit frightening, said this way. *)
        List.map (Printf.sprintf "curl -L \"%s\" | sh -ex -") source_urls
      else
        List.map (Printf.sprintf "wget -O - \"%s\" | sh -ex -") source_urls
    in
    List.iter (fun cmd ->
        match run_command [cmd] with
        | Unix.WEXITED 0 -> ()
        | _ -> failwith (Printf.sprintf "Command %S failed" cmd))
      commands;
    Printf.printf "Source installation scripts run successfully\n%!"


(* Command-line handling *)

let main print_flags list short no_sources
    debug_arg install_arg update_arg dryrun_arg
    jobs_arg interactive_arg verbose_arg yes_arg opam_packages =
  if debug_arg then debug := true;
  let opam_flags =
    (match jobs_arg with
    | Some j -> ["-j";string_of_int j]
    | None -> []) @
    (match verbose_arg with true -> ["-v"] | false -> []) @
    (match yes_arg with true -> ["-y"] | false -> [])
  in
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
  if os_packages <> [] && not short then
    Printf.printf "# The following system packages are needed:\n#  - %s\n%!"
      (String.concat "\n#  - " os_packages)
  else if list && not short then
    print_endline "# No required system packages found";
  if list then exit 0;
  let source_urls =
    if no_sources then [] else
      List.filter (fun s -> not (List.mem s os_packages))
        (depexts (sourceflags @ flags) opam_packages)
  in
  if source_urls <> [] && not short then
    Printf.printf "# The following scripts need to be run:\n#  - %s\n%!"
      (String.concat "\n#  - " source_urls);
  if os_packages = [] && source_urls = [] && not short then
    Printf.printf "# No extra OS packages requirements found.\n%!";
  let installed = get_installed_packages distribution os_packages in
  let os_packages =
    List.filter (fun p -> not (List.mem p installed)) os_packages
  in
  if short then
    (List.iter print_endline os_packages;
     List.iter print_endline source_urls)
  else if installed <> [] then
    if os_packages <> [] then
      Printf.printf
        "# The following new OS packages need to be installed: %s\n%!"
        (String.concat " " os_packages)
    else
      Printf.printf
        "# All required OS packages found.\n%!";
  if dryrun_arg then exit (if os_packages = [] then 0 else 1);
  let interactive = match interactive_arg with
    | Some i -> i
    | None -> Unix.isatty Unix.stdin
  in
  if os_packages <> [] && update_arg then update os distribution;
  install ~interactive os distribution os_packages;
  run_source_scripts source_urls;
  let opam_cmdline = "opam"::"install":: opam_flags @ opam_packages in
  if install_arg && opam_packages <> [] then begin
    (if not short then Printf.printf "# Now letting OPAM install the packages\n%!");
    (if !debug then Printf.eprintf "+ %s\n%!" (String.concat " " opam_cmdline));
    Unix.execvp "opam" (Array.of_list opam_cmdline)
  end

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

let update_arg =
  Arg.(value & flag &
       info ~doc:"Update the OS package sets before installation" ["u";"update"])

let install_arg =
  Arg.(value & flag &
       info ~doc:"Install the packages through \"opam install\" after \
                  installing external dependencies" ["i";"install"])

let interactive_arg =
  Arg.(value & vflag None [
      Some true, info
        ~doc:"Run the system package manager interactively (default if run \
              from a tty)"
        ["interactive";"I"];
      Some false, info
        ~doc:"Run the system package manager non-interactively \
              (default when not running from a tty)"
        ["noninteractive"];
    ])

let mk_opt ?section ?vopt flags value doc conv default =
  let doc = Arg.info ?docs:section ~docv:value ~doc flags in
  Arg.(value & opt ?vopt conv default & doc)

let jobs_arg =
  let positive : int Arg.converter =
    let (parser, printer) = Arg.int in
    let parser s =
      match parser s with
      | `Error _ -> `Error "expected a positive integer"
      | `Ok n as r -> if n <= 0
        then `Error "expected a positive integer"
        else r in
    (parser, printer) in
  mk_opt ["j";"jobs"] "JOBS"
    "Set the maximal number of concurrent jobs to use when installing packages.
     You can also set it using the $(b,\\$OPAMJOBS) environment variable.
     This setting only applies when the $(i,-i) flag is also passed."
    Arg.(some positive) None

let verbose_arg = 
  Arg.(value & flag &
       info ~doc:"Display the build output of source packages as they are
     being compiled. You can set it using the $(b,\\$OPAMVERBOSE) environment
     variable.  This setting only applies when the $(i,-i) flag is also passed."
     ["v";"verbose"])

let yes_arg =
  Arg.(value & flag &
       info ~doc:"Disable interactive mode and answer yes to all questions that
     would otherwise be asked to the user. This is equivalent to setting
     \\$OPAMYES to \"true\".  This setting only applies when the $(i,-i) flag is
     also passed." ["y";"yes"])

let dryrun_arg =
  Arg.(value & flag &
       info ~doc:"Only list the new system packages (and source scripts) that \
                  would need to be installed, don't try to install them. Exits \
                  with 0 if all required system packages are already installed, \
                  1 otherwise."
         ["n";"dry-run"])

let command =
  let man = [
    `S "DESCRIPTION";
    `P "$(b,opam-depext) is a simple program intended to facilitate the interaction \
        between OPAM packages and the host package management system. It can \
        perform OS and distribution detection, query OPAM for the right \
        external dependencies on a set of packages, and call the OS package \
        manager in the appropriate way to install then.";
    `S "COPYRIGHT";
    `P "$(b,opam-depext) is written by Louis Gesbert <louis.gesbert@ocamlpro.com>, \
        copyright OCamlPro 2014-2015 with contributions from Anil Madhavapeddy, \
        distributed under the terms of the LGPL v3 with linking exception. \
        Full source available at $(i,https://github.com/ocaml/opam-depext)";
    `S "BUGS";
    `P "Bugs are tracked at $(i,https://github.com/ocaml/opam-depext/issues) \
        or can be reported to $(i,<opam-devel@lists.ocaml.org>).";
  ] in
  let doc = "Query and install external dependencies of OPAM packages" in
  Term.(pure main $ print_flags_arg $ list_arg $ short_arg $
        no_sources_arg $ debug_arg $ install_arg $ update_arg $ dryrun_arg $
        jobs_arg $ interactive_arg $ verbose_arg $ yes_arg $ packages_arg),
  Term.info "opam-depext" ~version:"1.0.0" ~doc ~man

let () =
  match Term.eval command with
  | `Ok () | `Version | `Help -> exit 0
  | `Error (`Parse | `Term) -> exit 2
  | `Error `Exn -> exit 1
