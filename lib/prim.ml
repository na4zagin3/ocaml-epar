open Core

module StringMap = Map.Make(String)

module Yaml = struct
  include Yaml
  (* TODO Needs efficient implementation*)
  let compare_value a b = Sexp.compare (Yaml.sexp_of_value a) (Yaml.sexp_of_value b)
end

type version = Epar_0_1
[@@deriving compare]

let string_of_version = function
  | Epar_0_1 -> "0.1"

let version_of_string = function
  | "0.1" -> Epar_0_1
  | x -> failwithf "%S is not a valid EPAR version" x ()

let sexp_of_version x =
  string_of_version x
  |> [%sexp_of: string]

let version_of_sexp x =
  [%of_sexp: string] x
  |> version_of_string

type location = {
  line: int
} [@@deriving compare, sexp]

type section = {
  location: location option; [@compare.ignore]
  filename: string;
  metadata: Yaml.value StringMap.t;
  content: string list;
} [@@deriving compare, sexp]

type archive = {
  version: version;
  metadata: Yaml.value StringMap.t;
  sections: section list;
} [@@deriving compare, sexp]

module type EparImpl = sig
  type t
  val version : version

  val create_archive : basedir:string -> string list -> t
  val extract_archive : basedir:string -> t -> unit

  val read_file_exn : string -> t
  val write_file : string -> t -> unit
  val of_string_exn : string -> t
  val to_string : t -> string
end

let header_metadata_defaults_key = "defaults"
let header_metadata_delimiter_key = "delimiter"
let section_metadata_linebreak_key = "line-break"
let section_metadata_linebreaksatend_key = "line-breaks-at-end"

let default_delimiter = "---"
let get_delimiter header_metadata = match Map.find header_metadata header_metadata_delimiter_key with
  | Some (`String "") -> Error "delimiter should not be an empty string"
  | Some (`String delimiter) -> Ok delimiter
  | None -> Ok default_delimiter
  | Some _-> Error "delimiter value must be an non-empty string"

let get_defaults header_metadata = match Map.find header_metadata header_metadata_defaults_key with
  | Some (`O o) -> Ok (StringMap.of_alist_exn o)
  | None -> Ok StringMap.empty
  | Some _-> Error "defaults value must be an object"

let get_linebreak metadata ~default = match Map.find metadata section_metadata_linebreak_key with
  | Some (`String "cr") -> Ok "\r"
  | Some (`String "lf") -> Ok "\n"
  | Some (`String "crlf") -> Ok "\r\n"
  | None -> Ok default
  | Some _-> Error "line-break value must be one of LF, CR, or CRLF"

let linebreak_symbol = function
  | "\r" -> "cr"
  | "\n" -> "lf"
  | "\r\n" -> "crlf"
  | c -> failwithf "Line break must be one of %S, %S, or %S, but got %S" "\n" "\r" "\r\n" c ()

let get_linebreaksatend metadata ~default = match Map.find metadata section_metadata_linebreaksatend_key with
  | Some (`Float f) -> Ok (Float.iround_down_exn f)
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

