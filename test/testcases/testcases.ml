open Core

let raise_failure prog args =
  let cmd_line =
    List.map ~f:Filename.quote (prog :: args)
    |> String.concat ~sep:" "
  in
  let open Shexp_process in
  function
   | Exit_status.Exited code ->
     Printf.ksprintf failwith "Command exited with code %d: %s"
       code cmd_line
   | Exit_status.Signaled signal ->
     Printf.ksprintf failwith "Command got signal %s: %s"
       (signal |> Signal.of_caml_int |> Signal.to_string) cmd_line


let run_with_buffered_stdout prog args pred =
  let open Shexp_process in
  let open Shexp_process.Infix in
  capture [Std_io.Stdout] (run_exit_status prog args)
  >>= fun (es, out) ->
    if pred es
    then return ()
    else echo ~where:Std_io.Stderr out >>= (fun _ -> raise_failure prog args es |> return)

let run_with_buffered_stdout_if_failed prog args =
  let open Shexp_process in
  run_with_buffered_stdout prog args (function
    | Exit_status.Exited 0 -> true
    | _ -> false
  )

module type EparImpl = sig
  val read_archive : basedir:string -> string list -> Epar.Prim.archive
  val write_archive : basedir:string -> Epar.Prim.archive -> unit
  val version : string
end

module EparTestcase (EparImpl : EparImpl) : sig
  val roundtrip_test : string -> string -> OUnitTest.test list
end = struct
  let run_preparation_script basedir =
    let open Shexp_process in
    let open Shexp_process.Infix in
    cwd_logical >>= (fun cwd ->
      let preparation_script_file =
        FilePath.concat basedir "prepare.sh"
        |> FilePath.make_absolute cwd
      in
      if FileUtil.(test Is_exec preparation_script_file)
      then chdir (FilePath.dirname preparation_script_file) (run preparation_script_file [EparImpl.version])
      else return ()
    )

  let archive_filename basedir = FilePath.concat basedir (Printf.sprintf "archive-%s.epar" EparImpl.version)

  let roundtrip_test_sub basedir = begin
    let archive_list_file = FilePath.concat basedir "files.txt" in
    let archive_file = archive_filename basedir in
    let contents_dir = FilePath.concat basedir "contents" in
    let extracted_dir = FilePath.concat basedir (Printf.sprintf "contents-%s.gen" EparImpl.version) in
    let archived_file = FilePath.concat basedir (Printf.sprintf "archive-%s.gen.epar" EparImpl.version) in

    FileUtil.(rm ~recurse:true [extracted_dir; archived_file;]);

    let archive_list =
      In_channel.read_all archive_list_file
      |> String.split_lines
    in

    let archive_of_dir =
      let filenames = archive_list in
      EparImpl.read_archive ~basedir:contents_dir filenames in

    let archive_of_file =
      In_channel.read_all archive_file |> Epar.StringConversion.of_string_exn in

    let () =
      EparImpl.write_archive ~basedir:extracted_dir archive_of_file in

    let () =
      let serialized_archive = Epar.StringConversion.to_string archive_of_dir in
      Out_channel.write_all ~data:serialized_archive archived_file in

    let open Shexp_process.Infix in
    run_with_buffered_stdout_if_failed "diff" ["-u"; archive_file; archived_file]
    >> run_with_buffered_stdout_if_failed "diff" ["-u"; "-r"; contents_dir; extracted_dir]
  end

  let roundtrip_test testname basedir =
    let cmd =
      let open Shexp_process.Infix in
      run_preparation_script basedir >> roundtrip_test_sub basedir
    in
    if FileUtil.(test Is_file (archive_filename basedir))
    then
      let open OUnit2 in
      [ testname >::
        fun _test_ctxt -> Shexp_process.eval cmd
      ]
    else []
end

module Epar0_1 = EparTestcase(Epar.Epar_0_1)

let () = print_endline (FileUtil.pwd ())

let testcases_basedir = "."

let ounit_suite =
  let testcase testname =
    let basedir = FilePath.concat testcases_basedir testname in
      match String.split ~on:'-' testname with
      | "roundtrip" :: _ -> List.concat [Epar0_1.roundtrip_test testname basedir]
      | _ when String.is_prefix ~prefix:"." testname -> []
      | _ ->
        failwithf "testcase: unknown test case type %s\n" testname ()
  in
  let tests =
    FileUtil.ls testcases_basedir
    |> FileUtil.(filter Is_dir)
    |> List.map ~f:FilePath.basename
    |> List.concat_map ~f:testcase
  in
  let open OUnit2 in
  "testcases" >::: tests

let () =
  let open OUnit2 in
  run_test_tt_main ounit_suite
