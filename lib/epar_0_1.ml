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

let split_lines_rev ~linebreak str =
  let str_length = String.length str in
  let linebreak_length = String.length linebreak in
  let linebreaks = String.substr_index_all ~pattern:linebreak ~may_overlap:false str in
  let rec sub start acc = function
    | [] ->
      String.slice str start str_length :: acc
    | 0 :: es ->
      sub linebreak_length ("" :: acc) es
    | e :: es ->
      sub (e + linebreak_length) (String.slice str start e :: acc) es
  in
  sub 0 [] linebreaks

(* TODO Rewrite to handle big files *)
let read_section_from_string ~defaults ~filename str : section =
  let linebreak_default = get_linebreak defaults ~default:"\n" |> Result.ok_or_failwith in
  let linebreaksatend_default = get_linebreaksatend defaults ~default:1 |> Result.ok_or_failwith in
  let first_linebreak_cr_index = String.index str '\r' in
  let first_linebreak_lf_index = String.index str '\n' in
  let linebreak =
    match first_linebreak_cr_index, first_linebreak_lf_index with
      | None, None -> linebreak_default
      | Some _, None -> "\r"
      | None, Some _ -> "\n"
      | Some n, Some m when n + 1 = m -> "\r\n"
      | Some n, Some m when n < m -> "\r"
      | Some n, Some m when n > m -> "\n"
      | Some _, Some _ ->
        failwithf "%s: BUG: read_section_from_string" filename ()
  in
  let trailing_lines, content_rev = split_lines_rev ~linebreak str |> List.split_while ~f:(String.is_empty) in
  let content = List.rev content_rev in
  let linebreaksatend =
    let trailing_empty_lines = List.length trailing_lines in
    if List.is_empty content
    then trailing_empty_lines - 1
    else trailing_empty_lines
  in
  let metadata =
    [ Option.some_if (String.equal linebreak linebreak_default |> not) (section_metadata_linebreak_key, `String (linebreak_symbol linebreak));
      Option.some_if (linebreaksatend <> linebreaksatend_default) (section_metadata_linebreaksatend_key, `Float (Int.to_float linebreaksatend));
    ] |> List.map ~f:Option.to_list |> List.concat |> StringMap.of_alist_exn in
  { location = None; filename; content; metadata; }

let read_section ~basedir ~defaults filename : section =
  let path = Filename.concat basedir filename in
  let str = In_channel.with_file ~binary:true path ~f:(fun ich ->
    In_channel.input_all ich
  ) in
  read_section_from_string ~defaults ~filename str

let%expect_test "read_section: ok: empty file" =
  let defaults = StringMap.empty in
  ""
  |> read_section_from_string ~defaults ~filename:"test"
  |> [%sexp_of: section]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    ((location ()) (filename test) (metadata ((line-breaks-at-end (Float 0))))
     (content ())) |}]

let%expect_test "read_section: ok: empty file with lf" =
  let defaults = StringMap.empty in
  "\n"
  |> read_section_from_string ~defaults ~filename:"test"
  |> [%sexp_of: section]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    ((location ()) (filename test) (metadata ()) (content ())) |}]

let%expect_test "read_section: ok: empty file with two lfs" =
  let defaults = StringMap.empty in
  "\n\n"
  |> read_section_from_string ~defaults ~filename:"test"
  |> [%sexp_of: section]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    ((location ()) (filename test) (metadata ((line-breaks-at-end (Float 2))))
     (content ())) |}]

let%expect_test "read_section: ok: empty file with cr" =
  let defaults = StringMap.empty in
  "\r"
  |> read_section_from_string ~defaults ~filename:"test"
  |> [%sexp_of: section]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    ((location ()) (filename test) (metadata ((line-break (String cr))))
     (content ())) |}]

let%expect_test "read_section: ok: empty file with crlf" =
  let defaults = StringMap.empty in
  "\r\n"
  |> read_section_from_string ~defaults ~filename:"test"
  |> [%sexp_of: section]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    ((location ()) (filename test) (metadata ((line-break (String crlf))))
     (content ())) |}]

let%expect_test "read_section: ok: empty file with two crlfs" =
  let defaults = StringMap.empty in
  "\r\n\r\n"
  |> read_section_from_string ~defaults ~filename:"test"
  |> [%sexp_of: section]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    ((location ()) (filename test)
     (metadata ((line-break (String crlf)) (line-breaks-at-end (Float 2))))
     (content ())) |}]

let%expect_test "read_section: ok: text file without linebreaks" =
  let defaults = StringMap.empty in
  "abcdef"
  |> read_section_from_string ~defaults ~filename:"test"
  |> [%sexp_of: section]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    ((location ()) (filename test) (metadata ((line-breaks-at-end (Float 0))))
     (content (abcdef))) |}]

let%expect_test "read_section: ok: text file with a trailing lf" =
  let defaults = StringMap.empty in
  "abc\ndef\n"
  |> read_section_from_string ~defaults ~filename:"test"
  |> [%sexp_of: section]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    ((location ()) (filename test) (metadata ()) (content (abc def))) |}]

let%expect_test "read_section: ok: text file without a trailing lf" =
  let defaults = StringMap.empty in
  "abc\ndef\nghi"
  |> read_section_from_string ~defaults ~filename:"test"
  |> [%sexp_of: section]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    ((location ()) (filename test) (metadata ((line-breaks-at-end (Float 0))))
     (content (abc def ghi))) |}]

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
