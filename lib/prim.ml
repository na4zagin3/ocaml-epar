open Core

module StringMap = Map.Make(String)
type section = {
  filename: string;
  metadata: Yaml.value StringMap.t;
  content: string list;
} [@@deriving sexp]

type archive = {
  metadata: Yaml.value StringMap.t;
  sections: section list;
} [@@deriving sexp]

let header_metadata_delimiter_key = "delimiter"
let section_metadata_linebreak_key = "line-break"

let default_delimiter = "---"
let get_delimiter header_metadata = match Map.find header_metadata header_metadata_delimiter_key with
  | Some (`String "") -> Error "delimiter should not be an empty string"
  | Some (`String delimiter) -> Ok delimiter
  | None -> Ok default_delimiter
  | Some _-> Error "delimiter value must be an non-empty string"

let get_linebreak metadata ~default = match Map.find metadata section_metadata_linebreak_key with
  | Some (`String "CR") -> Ok "\r"
  | Some (`String "LF") -> Ok "\n"
  | Some (`String "CRLF") -> Ok "\r\n"
  | None -> Ok default
  | Some _-> Error "line-break value must be one of LF, CR, or CRLF"

(* TODO Fix escape map *)
let escapeworthy_map =['"', '"'; '\n', 'n']

let escape_filename =
  let escape_char = '\\' in
  String.Escaping.escape_gen ~escapeworthy_map ~escape_char
  |> Or_error.ok_exn

let unescape_filename =
  let escape_char = '\\' in
  String.Escaping.unescape_gen ~escapeworthy_map ~escape_char
  |> Or_error.ok_exn

