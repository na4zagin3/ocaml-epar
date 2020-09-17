open Core

open Prim

let write_section ~basedir ~defaults (section : section) : unit =
  let path = Filename.concat basedir section.filename in
  let parent_dir = Filename.dirname path in
  let linebreak_default = get_linebreak defaults ~default:"\n"  in
  let linebreak = Result.(linebreak_default >>= (fun default -> get_linebreak section.metadata ~default)) |> Result.ok_or_failwith in
  let linebreaksatend_default = get_linebreaksatend defaults ~default:1  in
  let linebreaksatend = Result.(linebreaksatend_default >>= (fun default -> get_linebreaksatend section.metadata ~default)) |> Result.ok_or_failwith in
  begin if FileUtil.(test Is_dir) parent_dir |> not
  then FileUtil.mkdir ~parent:true parent_dir
  end;
  Out_channel.with_file ~binary:true path ~f:(fun och ->
    begin match section.content with
      | [] -> ()
      | [line] -> Out_channel.output_string och line
      | line :: lines ->
        Out_channel.output_string och line;
        List.iter lines ~f:(fun line -> Out_channel.output_string och linebreak; Out_channel.output_string och line) end;
    for _ = 0 to linebreaksatend - 1 do Out_channel.output_string och linebreak done
  )

(* TODO Rewrite to handle big files *)
let read_section ~basedir ~defaults filename : section =
  let path = Filename.concat basedir filename in
  let linebreak_default = get_linebreak defaults ~default:"\n" |> Result.ok_or_failwith in
  let linebreaksatend_default = get_linebreaksatend defaults ~default:1 |> Result.ok_or_failwith in
  let str = In_channel.with_file ~binary:true path ~f:(fun ich ->
    In_channel.input_all ich
  ) in
  let first_linebreak_index = String.lfindi ~pos:0 str ~f:(fun _ c -> Char.equal c '\r' || Char.equal c  '\n') in
  let linebreak =
    match first_linebreak_index with
      | None -> linebreak_default
      | Some first_linebreak_index ->
        match String.slice str first_linebreak_index (first_linebreak_index + 2) with
          | "\r\n" as lb -> lb
          | lb2 -> begin match String.prefix lb2 1 with
            | ("\r" | "\n") as lb -> lb
            | _ -> linebreak_default end in
  let trailing_lines, content_rev = String.split_lines str |> List.rev |> List.split_while ~f:(String.is_empty) in
  let content = List.rev content_rev in
  let linebreaksatend = List.length trailing_lines + if String.is_suffix ~suffix:linebreak str then 1 else 0 in
  let metadata =
    [ Option.some_if (String.equal linebreak linebreak_default |> not) (section_metadata_linebreak_key, `String (linebreak_symbol linebreak));
      Option.some_if (linebreaksatend <> linebreaksatend_default) (section_metadata_linebreaksatend_key, `Float (Int.to_float linebreaksatend));
    ] |> List.map ~f:Option.to_list |> List.concat |> StringMap.of_alist_exn in
  { location = None; filename; content; metadata; }

let write_archive ~basedir (archive : archive) : unit =
  let defaults = get_defaults archive.metadata |> Result.ok_or_failwith in
  List.iter archive.sections ~f:(write_section ~basedir ~defaults)

let delimiter_allowed sections delimiter =
  if String.is_prefix ~prefix:" " delimiter
  then false (* conservative *)
  else not (List.exists sections ~f:(fun section -> List.fold_until section.content ~init:false ~finish:(const false) ~f:(fun following_empty_line -> function
    | "" -> Continue_or_stop.Continue true
    | s -> if following_empty_line && String.is_prefix ~prefix:delimiter s
      then Continue_or_stop.Stop true
      else Continue_or_stop.Continue false)))

let read_archive ~basedir filenames : archive =
  let defaults = StringMap.empty in
  let sections = List.map filenames ~f:(fun fn -> read_section ~basedir ~defaults fn) in
  let delimiter_candidates =
    let rand4 () = Random.int 0xffff in
    [ "---"; "-#-#-";
      Printf.sprintf "-#%x#-" (rand4 ());
      Printf.sprintf "-#%x%x#-" (rand4 ()) (rand4 ());
      Printf.sprintf "-#%x%x%x%x#-" (rand4 ()) (rand4 ()) (rand4 ()) (rand4 ());
    ] in
  let delimiter = Option.value_exn ~message:"Cannot found a valid delimiter" (List.find ~f:(delimiter_allowed sections) delimiter_candidates) in
  (* TODO find an appropriate delimiter *)
  let metadata =
    [ Option.some_if (String.equal delimiter default_delimiter |> not) (header_metadata_delimiter_key, `String delimiter);
    ] |> List.map ~f:Option.to_list |> List.concat |> StringMap.of_alist_exn in
  { metadata; sections; }
