open Core

open Prim

let map_to_yaml map =
  `O (Map.to_alist map)

(* Handle trailing line breaks *)
let filename_to_string filename : string =
  "\"" ^ escape_filename filename ^ "\""

let%expect_test "filename_to_string: encode file names" =
  print_endline (filename_to_string "\"abc\"\nde");
  [%expect{|
    "\"abc\"\nde"
  |}]

let section_to_string delimiter section : string =
  let metadata = if Map.is_empty (section : section).metadata
    then ""
    else Yaml.to_string_exn (map_to_yaml section.metadata) ^ "...\n" in
  delimiter ^ " " ^ filename_to_string section.filename ^ "\n"
  ^ metadata
  ^ "\n"
  ^ String.concat ~sep:"\n" section.content

let example_section1 = {
  filename = "abc.txt";
  metadata = StringMap.empty;
  content = ["abc"; "def"; "ghi"]
}

let example_section2 = {
  filename = "abc.txt";
  metadata = StringMap.of_alist_exn ["line-break", `String "LF"];
  content = ["abc"; "def"; "ghi"]
}

let%expect_test "section_to_string: example without a metadata" =
  let delimiter = "---" in
  print_endline (section_to_string delimiter example_section1);
  [%expect{|
    --- "abc.txt"

    abc
    def
    ghi
  |}]

let%expect_test "section_to_string: example with a metadata" =
  let delimiter = "---" in
  print_endline (section_to_string delimiter example_section2);
  [%expect{|
    --- "abc.txt"
    line-break: LF
    ...

    abc
    def
    ghi
  |}]

let to_string archive : string =
  let delimiter = match Map.find archive.metadata header_metadata_delimiter_key with
    | Some (`String str) -> str
    | _ -> "---" in
  let metadata = if Map.is_empty archive.metadata
    then ""
    else Yaml.to_string_exn (map_to_yaml archive.metadata) ^ "...\n" in
  "#EPAR: 0.1\n"
  ^ metadata
  ^ "\n"
  ^ String.concat ~sep:"\n\n" (List.map ~f:(section_to_string delimiter) archive.sections)

let%expect_test "to_string: example without a header metadata" =
  let archive = {
    metadata = StringMap.empty;
    sections = [example_section1; example_section2];
  } in
  print_endline (to_string archive);
  [%expect{|
    #EPAR: 0.1

    --- "abc.txt"

    abc
    def
    ghi

    --- "abc.txt"
    line-break: LF
    ...

    abc
    def
    ghi
  |}]

let%expect_test "to_string: example with a header metadata" =
  let archive = {
    metadata = StringMap.of_alist_exn ["delimiter", `String "---"];
    sections = [example_section1; example_section2];
  } in
  print_endline (to_string archive);
  [%expect{|
    #EPAR: 0.1
    delimiter: ! '---'
    ...

    --- "abc.txt"

    abc
    def
    ghi

    --- "abc.txt"
    line-break: LF
    ...

    abc
    def
    ghi
  |}]

type 'a result = ('a * (int * string list)) Or_error.t [@@deriving sexp]

let parse_header pos : string list -> unit result = function
  | "#EPAR: 0.1" :: rest -> Or_error.return ((), (pos + 1, rest))
  | str :: _ -> Or_error.error_string (Format.sprintf "line %d: Invalid header %S" pos str)
  | [] -> Or_error.error_string (Format.sprintf "line %d: Empty" pos)

let%expect_test "parse_header: ok" =
  [ "#EPAR: 0.1";
    "";
    "--- \"abc.txt\"";
  ]
  |> parse_header 0
  |> [%sexp_of: unit result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{| (Ok (() (1 ("" "--- \"abc.txt\"")))) |}]

let%expect_test "parse_header: error: empty" =
  [ "abc";
    "";
    "--- \"abc.txt\"";
  ]
  |> parse_header 0
  |> [%sexp_of: unit result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{| (Error "line 0: Invalid header \"abc\"") |}]

let%expect_test "parse_header: error: empty" =
  []
  |> parse_header 0
  |> [%sexp_of: unit result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{| (Error "line 0: Empty") |}]

let parse_yaml_metadata part pos : string list -> Yaml.value StringMap.t result = function
  | "" :: rest -> Or_error.return (StringMap.empty, (pos + 1, rest))
  | lines ->
    match List.split_while lines ~f:(fun x -> not (String.equal "..." x)) with
      | _, [] -> Or_error.error_string (Format.sprintf "line %d: Section metadata must be terminated with \"...\"" pos)
      | metadata, "..." :: rest ->
      (* TODO Handle error *)
      begin match Yaml.of_string_exn (String.concat ~sep:"\n" metadata) with
        | `O (alist) -> Or_error.(StringMap.of_alist_or_error alist >>| fun m -> (m, (pos + List.length metadata, rest)))
        | `Null -> Or_error.return (StringMap.empty, (pos + List.length metadata, rest))
        | y -> Or_error.error_string (Format.sprintf !"line %d: %s must be an object, but got %{sexp:Yaml.value}" pos part y) end
      | metadata, rest -> Or_error.error_string (Format.sprintf !"BUG: line %d: This must not happen: metadata: %{sexp: string list}, rest:%{sexp: string list}" pos metadata rest)

let parse_header_metadata = parse_yaml_metadata "Header Metadata"

let%expect_test "parse_header_metadata: ok: beginning with an empty line" =
  [ "";
    "--- def: 2";
    "def: 2";
    "...";
  ]
  |> parse_header_metadata 0
  |> [%sexp_of: Yaml.value StringMap.t result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{| (Ok (() (1 ("--- def: 2" "def: 2" ...)))) |}]

let%expect_test "parse_header_metadata: ok: with an object" =
  [ "abc: 1";
    "def: 2";
    "...";
    "rest";
  ]
  |> parse_header_metadata 0
  |> [%sexp_of: Yaml.value StringMap.t result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{| (Ok (((abc (Float 1)) (def (Float 2))) (2 (rest)))) |}]

let%expect_test "parse_header_metadata: error: not terminated" =
  [ "abc: 1"
  ]
  |> parse_header_metadata 0
  |> [%sexp_of: Yaml.value StringMap.t result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{| (Error "line 0: Section metadata must be terminated with \"...\"") |}]

let%expect_test "parse_header_metadata: error: not an object" =
  [ "- abc";
    "- def";
    "...";
  ]
  |> parse_header_metadata 0
  |> [%sexp_of: Yaml.value StringMap.t result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    (Error
     "line 0: Header Metadata must be an object, but got (A ((String abc) (String def)))") |}]

let rec parse_section_header delimiter pos : string list -> string result = function
  | [] -> Or_error.error_string (Format.sprintf "BUG: line %d: File section Header does not exist" pos)
  | "" :: rest -> parse_section_header delimiter (pos + 1) rest
  | fn_line :: rest -> match String.chop_prefix ~prefix:(delimiter ^ " ") fn_line with
    | Some fn ->
      let fn = String.lstrip fn ~drop:(Char.is_whitespace) in
      begin match String.chop_prefix fn ~prefix:"\"" with
        | Some fn ->
          begin match String.Escaping.lsplit2 fn ~on:'"' ~escape_char:'\\' with
                  | Some (unescaped, "") -> Or_error.return (unescaped, (pos + 1, rest))
                  | Some (_, remaining_fields) -> Or_error.error_string (Format.sprintf "line %d: File section header must contain only a filename but got: %S" pos remaining_fields)
                  | None -> Or_error.error_string (Format.sprintf "line %d: Quoted file name does not terminate: %S" pos fn_line) end
        | None -> match String.split fn ~on:' ' with
          | [fn] -> Or_error.return (fn, (pos + 1, rest))
          | [] -> Or_error.error_string (Format.sprintf "line %d: File section Header does not have a filename: %S" pos fn_line)
          | _ -> Or_error.error_string (Format.sprintf !"line %d: A file name with spaces must be quoted: %S" pos fn) end
    | None -> Or_error.error_string (Format.sprintf "line %d: File section Header is invalid: %S" pos fn_line)

let%expect_test "parse_section_header: ok: bare filename" =
  [ "--- abc.txt";
    "";
    "...";
  ]
  |> parse_section_header "---" 0
  |> [%sexp_of: string result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{| (Ok (abc.txt (1 ("" ...)))) |}]

let%expect_test "parse_section_header: ok: bare filename with heading spaces" =
  [ "---    abc.txt";
    "";
    "...";
  ]
  |> parse_section_header "---" 0
  |> [%sexp_of: string result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{| (Ok (abc.txt (1 ("" ...)))) |}]

let%expect_test "parse_section_header: ok: quoted filename" =
  [ "--- \"abc def ghi\nxyz.txt\"";
    "";
    "...";
  ]
  |> parse_section_header "---" 0
  |> [%sexp_of: string result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    (Ok ( "abc def ghi\
         \nxyz.txt" (1 ("" ...)))) |}]

let%expect_test "parse_section_header: ok: quoted filename with trailing spaces" =
  [ "---    \"abc def ghi\nxyz.txt\"";
    "";
    "...";
  ]
  |> parse_section_header "---" 0
  |> [%sexp_of: string result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    (Ok ( "abc def ghi\
         \nxyz.txt" (1 ("" ...)))) |}]

let%expect_test "parse_section_header: error: end of stream" =
  []
  |> parse_section_header "---" 0
  |> [%sexp_of: string result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{| (Error "BUG: line 0: File section Header does not exist") |}]

let%expect_test "parse_section_header: error: end of stream with empty lines" =
  [ "";
    "";
    "";
  ]
  |> parse_section_header "---" 0
  |> [%sexp_of: string result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{| (Error "BUG: line 3: File section Header does not exist") |}]

let%expect_test "parse_section_header: error: bare filename followed by another info" =
  [ "--- abc.txt abc";
    "";
    "...";
  ]
  |> parse_section_header "---" 0
  |> [%sexp_of: string result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    (Error "line 0: A file name with spaces must be quoted: \"abc.txt abc\"") |}]

let%expect_test "parse_section_header: error: quoted filename followed by another info" =
  [ "--- \"abc def.txt\" abc";
    "";
    "...";
  ]
  |> parse_section_header "---" 0
  |> [%sexp_of: string result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    (Error
     "line 0: File section header must contain only a filename but got: \" abc\"") |}]

let%expect_test "parse_section_header: error: lacking a space after the delimiter" =
  [ "---\"abc.txt\"";
    "";
    "...";
  ]
  |> parse_section_header "---" 0
  |> [%sexp_of: string result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    (Error "line 0: File section Header is invalid: \"---\\\"abc.txt\\\"\"") |}]

let parse_section_metadata = parse_yaml_metadata "Section Metadata"

let parse_section_content delimiter pos lines : string list result =
  let process_acc x = List.drop_while x ~f:(String.is_empty) |> List.rev in
  let rec sub acc pos = function
    | [] -> Or_error.return (process_acc acc, (pos, []))
    | "" :: (fn_line :: _ as rest) -> if String.is_prefix ~prefix:(delimiter ^ " ") fn_line
      then Or_error.return (process_acc acc, (pos + 1, rest))
      else sub ("" :: acc) (pos + 1) rest
    | line :: rest -> sub (line :: acc) (pos + 1) rest in
  sub [] pos lines

let%expect_test "parse_section_content: ok: eos" =
  []
  |> parse_section_content "---" 0
  |> [%sexp_of: string list result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{| (Ok (() (0 ()))) |}]

let%expect_test "parse_section_content: ok: empty lines before eos" =
  [ "";
    "";
    "";
  ]
  |> parse_section_content "---" 0
  |> [%sexp_of: string list result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{| (Ok (() (3 ()))) |}]

let%expect_test "parse_section_content: ok: last section" =
  [ "abc";
    "def";
  ]
  |> parse_section_content "---" 0
  |> [%sexp_of: string list result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    (Ok ((abc def) (2 ()))) |}]

let%expect_test "parse_section_content: ok: last section with trailing empty lines" =
  [ "abc";
    "def";
    "";
  ]
  |> parse_section_content "---" 0
  |> [%sexp_of: string list result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    (Ok ((abc def) (3 ()))) |}]

let%expect_test "parse_section_content: ok: non-last section" =
  [ "abc";
    "def";
    "";
    "--- def.txt";
  ]
  |> parse_section_content "---" 0
  |> [%sexp_of: string list result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    (Ok ((abc def) (3 ("--- def.txt")))) |}]

let%expect_test "parse_section_content: ok: non-last section with trailing empty lines" =
  [ "abc";
    "def";
    "";
    "";
    "--- def.txt";
  ]
  |> parse_section_content "---" 0
  |> [%sexp_of: string list result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    (Ok ((abc def) (4 ("--- def.txt")))) |}]

let%expect_test "parse_section_content: ok: non-last section with confusing lines" =
  [ "abc";
    "def";
    "";
    "---! def.txt";
    "abc";
    "---";
    "--- ghi.txt";
    "";
    "--- xyz.txt";
  ]
  |> parse_section_content "---" 0
  |> [%sexp_of: string list result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    (Ok ((abc def "" "---! def.txt" abc --- "--- ghi.txt") (8 ("--- xyz.txt")))) |}]

let parse_section delimiter pos lines : section result =
  Or_error.bind (parse_section_header delimiter pos lines) ~f:(fun (filename, (pos, lines)) ->
    Or_error.bind (parse_section_metadata pos lines) ~f:(fun (metadata, (pos, lines)) ->
      Or_error.bind (parse_section_content delimiter pos lines) ~f:(fun (content, (pos, lines)) ->
        let section = {filename; metadata; content;} in
        Or_error.return (section, (pos, lines))
      )
    )
  )

let%expect_test "parse_section: ok: non-last section" =
  [ "--- def.txt";
    "";
    "abc";
    "def";
    "";
    "--- def.txt";
  ]
  |> parse_section "---" 0
  |> [%sexp_of: section result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    (Ok
     (((filename def.txt) (metadata ()) (content (abc def))) (5 ("--- def.txt")))) |}]

let%expect_test "parse_section: ok: last section" =
  [ "--- def.txt";
    "";
    "abc";
    "def";
  ]
  |> parse_section "---" 0
  |> [%sexp_of: section result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    (Ok (((filename def.txt) (metadata ()) (content (abc def))) (4 ()))) |}]

let%expect_test "parse_section: ok: last section with trailing spaces" =
  [ "--- def.txt";
    "";
    "abc";
    "def";
    "";
  ]
  |> parse_section "---" 0
  |> [%sexp_of: section result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    (Ok (((filename def.txt) (metadata ()) (content (abc def))) (5 ()))) |}]

let%expect_test "parse_section: ok: non-last section with metadata" =
  [ "--- def.txt";
    "line-break: CR";
    "line-breaks-at-end: 2";
    "...";
    "";
    "abc";
    "def";
    "";
    "--- def.txt";
  ]
  |> parse_section "---" 0
  |> [%sexp_of: section result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    (Ok
     (((filename def.txt)
       (metadata ((line-break (String CR)) (line-breaks-at-end (Float 2))))
       (content ("" abc def)))
      (7 ("--- def.txt")))) |}]

let parse_sections delimiter pos lines : section list result =
  let rec sub acc pos : string list -> section list result = function
    | [] -> Or_error.return (List.rev acc, (pos, []))
    | lines ->
    Or_error.bind (parse_section delimiter pos lines) ~f:(fun (section, (pos, lines)) ->
      sub (section :: acc) pos lines
    ) in
  sub [] pos lines

let%expect_test "parse_sections: ok: various sections" =
  [ "--- abc.txt";
    "";
    "abc";
    "def";
    "";
    "--- def.txt";
    "line-break: CR";
    "line-breaks-at-end: 2";
    "...";
    "";
    "abc";
    "def";
    "";
  ]
  |> parse_sections "---" 0
  |> [%sexp_of: section list result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    (Ok
     ((((filename abc.txt) (metadata ()) (content (abc def)))
       ((filename def.txt)
        (metadata ((line-break (String CR)) (line-breaks-at-end (Float 2))))
        (content ("" abc def))))
      (12 ()))) |}]

let lift_std_result pos = function
  | Ok v -> Or_error.return v
  | Error msg -> Or_error.errorf "line %d: %s" pos msg

let parse_archive pos lines : archive result =
  Or_error.bind (parse_header pos lines) ~f:(fun ((), (pos, lines)) ->
    Or_error.bind (parse_header_metadata pos lines) ~f:(fun (metadata, (pos, lines)) ->
      Or_error.bind (get_delimiter metadata |> lift_std_result pos) ~f:(fun delimiter ->
        Or_error.map (parse_sections delimiter pos lines) ~f:(fun (sections, rest) ->
          { metadata; sections }, rest
        )
      )
    )
  )

let%expect_test "parse_sections: ok: without header metadata" =
  [ "#EPAR: 0.1";
    "";
    "--- abc.txt";
    "";
    "abc";
    "def";
    "";
    "--- def.txt";
    "line-break: CR";
    "line-breaks-at-end: 2";
    "...";
    "";
    "abc";
    "def";
    "";
  ]
  |> parse_archive 0
  |> [%sexp_of: archive result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    (Ok
     (((metadata ())
       (sections
        (((filename abc.txt) (metadata ()) (content (abc def)))
         ((filename def.txt)
          (metadata ((line-break (String CR)) (line-breaks-at-end (Float 2))))
          (content ("" abc def))))))
      (14 ()))) |}]

let%expect_test "parse_sections: ok: specifying a delimiter" =
  [ "#EPAR: 0.1";
    "delimiter: '#'";
    "defaults:";
    "    line-breaks-at-end: 1";
    "...";
    "";
    "# abc.txt";
    "";
    "abc";
    "def";
    "";
    "# def.txt";
    "line-break: CR";
    "line-breaks-at-end: 2";
    "...";
    "";
    "abc";
    "def";
    "";
  ]
  |> parse_archive 0
  |> [%sexp_of: archive result]
  |> Sexp.pp_hum Format.std_formatter;
  [%expect{|
    (Ok
     (((metadata
        ((defaults (O ((line-breaks-at-end (Float 1))))) (delimiter (String #))))
       (sections
        (((filename abc.txt) (metadata ()) (content (abc def)))
         ((filename def.txt)
          (metadata ((line-break (String CR)) (line-breaks-at-end (Float 2))))
          (content ("" abc def))))))
      (17 ()))) |}]

let of_string_or_error str : archive Or_error.t =
  let lines = String.split_lines str in
  Or_error.map (parse_archive 0 lines) ~f:fst

let of_string_exn str : archive =
  Or_error.ok_exn (of_string_or_error str)
