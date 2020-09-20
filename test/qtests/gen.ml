open Core
open Epar.Prim

let char_utf8_n =
  let open QCheck.Gen in
  char_range '\x80' '\xbf'

let string_char_utf8_1 =
  let open QCheck.Gen in
  map Char.to_string (char_range '\x00' '\x7f')

let string_char_utf8_2 =
  let open QCheck.Gen in
  map2 (fun a b -> String.of_char_list [a; b]) (char_range '\xc2' '\xdf') char_utf8_n

let string_char_utf8_3 =
  let open QCheck.Gen in
  frequency
    [  0x2, map2 (fun   b c -> String.of_char_list ['\xe0'; b; c]) (char_range '\xa0' '\xbf') char_utf8_n;
      0x30, map3 (fun a b c -> String.of_char_list [a; b; c]) (char_range '\xe1' '\xec') char_utf8_n char_utf8_n;
       0x2, map2 (fun   b c -> String.of_char_list ['\xed'; b; c]) (char_range '\x80' '\x9f') char_utf8_n;
       0x4, map2 (fun   b c -> String.of_char_list ['\xef'; b; c]) char_utf8_n char_utf8_n;
    ]

let string_char_utf8_4 =
  let open QCheck.Gen in
  let map4 f x y z w = x >>= (fun a -> map3 (f a) y z w) in
  frequency
    [  0x3, map3 (fun   b c d -> String.of_char_list ['\xf0'; b; c; d]) (char_range '\x90' '\xbf') char_utf8_n char_utf8_n;
       0xc, map4 (fun a b c d -> String.of_char_list [a; b; c; d]) (char_range '\xf1' '\xf3') char_utf8_n char_utf8_n char_utf8_n;
       0x1, map3 (fun   b c d -> String.of_char_list ['\xf4'; b; c; d]) (char_range '\x80' '\x8f') char_utf8_n char_utf8_n;
    ]

let string_char_utf8_normal =
  let open QCheck.Gen in
  frequency
    [  16, string_char_utf8_1;
       4, string_char_utf8_2;
       2, string_char_utf8_3;
       1, string_char_utf8_4;
    ]

let string_utf8_repeat n : string QCheck.Gen.t =
  let open QCheck.Gen in
  map String.concat (list_repeat n string_char_utf8_normal)

let string_utf8_gen =
  let open QCheck.Gen in
  small_nat >>= string_utf8_repeat

let shrink_string_utf8 s yield =
  for i = 0 to String.length s-1 do
    let skip = match s.[i] with
      | '\x00' .. '\x7f' -> Some 1
      | '\x80' .. '\xbf' -> None
      | '\xc0' .. '\xdf' -> Some 2
      | '\xe0' .. '\xef' -> Some 3
      | '\xf0' .. '\xf4' -> Some 4
      | x -> failwithf "Invalid utf-8 prefix: %x" (Char.to_int x) ()
    in
    match skip with
      | None -> ()
      | Some skip ->
        let s' = Bytes.init (String.length s - skip)
          ~f:(fun j -> if j < i then s.[j] else s.[j + skip])
        in
        yield (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:s')
  done

let string_utf8 =
  QCheck.make ~shrink:shrink_string_utf8 ~small:String.length
    ~print:(sprintf "%S") string_utf8_gen

let string_utf8_with_unified_linebreak linebreak =
  let is_linebreak = function
    | '\r' | '\n' -> true
    | _ -> false
  in
  let gen =
    let open QCheck.Gen in
    list (map (String.filter ~f:is_linebreak) string_utf8_gen)
    |> map (String.concat ~sep:linebreak)
  in
  QCheck.make ~shrink:shrink_string_utf8 ~small:String.length
    ~print:(sprintf "%S") gen


let linebreak_gen =
  let open QCheck.Gen in
  oneofa [|"\r"; "\n"; "\r\n"|]

let linebreak =
  QCheck.make ~print:(sprintf "%S") string_utf8_gen


(* TODO Use string_utf8 instead *)
let yaml_string_gen = QCheck.Gen.(sized @@ fun n -> string_utf8_repeat (min n 10))
let yaml_string =
  QCheck.make ~shrink:shrink_string_utf8 ~small:String.length
    ~print:(sprintf "%S") yaml_string_gen

let yaml =
  let open QCheck.Iter in
  let yaml_gen_limited max_size : Yaml.value QCheck.Gen.t = QCheck.Gen.(sized @@ fix (fun self n -> match n with
    | 0 -> map (fun x -> `String x) yaml_string_gen
    | n -> sized @@ (fun size ->
      (* TODO Limit sample sized to avoid buffer size limitation *)
      let n = min n max_size in
      let size = min size max_size in
      frequency
        [ 10, self 0;
          1, let keys = list_repeat n string_utf8.gen in
             let values = list_repeat n (self (size / n)) in
             map (fun x -> `O x) (map2 List.zip_exn keys values)
        ]
    )
  )) in
  let yaml_gen : Yaml.value QCheck.Gen.t = yaml_gen_limited 3 in
  (* let print_yaml v = Yaml.to_string_exn ~len:0x1000000 v in *)
  let print_yaml v = Yaml.sexp_of_value v |> Sexp.to_string_hum in
  let rec shrink_yaml : Yaml.value QCheck.Shrink.t = function
    | `Null -> QCheck.Shrink.(map (fun () -> `Null) (unit ()))
    | `Bool b -> QCheck.Shrink.(map (fun () -> `Bool b) (unit ()))
    | `Float f -> QCheck.Shrink.(map (fun () -> `Float f) (unit ()))
    | `String str -> shrink_string_utf8 str >|= (fun s -> `String s)
    | `O alist ->
      let open QCheck.Shrink in
      list ~shrink:(fun (k, v) -> shrink_yaml v |> map (fun v -> (k, v))) alist
      |> map (fun alist -> `O alist)
    | `A vs ->
      let open QCheck.Shrink in
      list ~shrink:shrink_yaml vs
      |> map (fun vs -> `A vs)
  in
  QCheck.make yaml_gen ?print:(Some print_yaml) ?shrink:(Some shrink_yaml)

module StringMap = Map.Make(String)

let string_map arb_val : 't StringMap.t QCheck.arbitrary =
  let open QCheck in
  list (pair string_utf8 arb_val)
  |> map ~rev:(StringMap.to_alist) (StringMap.of_alist_reduce ~f:const)

let location_gen : location QCheck.Gen.t = QCheck.Gen.(map (fun x -> {line = x }) int)
let print_location v = Format.sprintf !"%{sexp:location}" v
let shrink_location : location QCheck.Shrink.t = function
  | { line = l } ->
    let open QCheck.Iter in
    QCheck.Shrink.int l >|= (fun l -> { line = l })
let location : location QCheck.arbitrary =
  QCheck.make location_gen ?print:(Some print_location) ?shrink:(Some shrink_location)

let section_gen : section QCheck.Gen.t = QCheck.Gen.(opt location.gen >>= (fun location ->
  map3 (fun filename metadata content -> { location; filename; metadata; content; })
    string_utf8.gen
    (string_map yaml).gen
    (list string)
))
let print_section v = Format.sprintf !"%{sexp:section}" v
let shrink_section : section QCheck.Shrink.t = function
  | { location; filename; metadata; content; } ->
    let open QCheck.Shrink in
    let shrink_quad = quad
        (option shrink_location)
        shrink_string_utf8
        (Option.value_exn (string_map yaml).shrink)
        (list ~shrink:string) in
    let open QCheck.Iter in
      shrink_quad (location, filename, metadata, content)
        >|= (fun (location, filename, metadata, content) -> { location; filename; metadata; content; })
let section : section QCheck.arbitrary =
  QCheck.make section_gen ?print:(Some print_section) ?shrink:(Some shrink_section)

let archive : archive QCheck.arbitrary =
  let version = Epar_0_1 in
  let archive_gen : archive QCheck.Gen.t = QCheck.Gen.(
    map2 (fun metadata sections -> { version; metadata; sections; })
      (string_map yaml).gen
      (list section_gen)
  ) in
  let print_archive v = Format.sprintf !"%{sexp:archive}" v in
  let shrink_archive : archive QCheck.Shrink.t = function
    | { version; metadata; sections; } ->
      let open QCheck.Shrink in
      let shrink_pair = pair
          (Option.value_exn (string_map yaml).shrink)
          (list ~shrink:shrink_section) in
      let open QCheck.Iter in
        shrink_pair (metadata, sections)
          >|= (fun (metadata, sections) -> { version; metadata; sections; })
  in
  QCheck.make archive_gen ?print:(Some print_archive) ?shrink:(Some shrink_archive)
