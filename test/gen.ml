open Core
open Epar.Prim

let yaml =
  let open QCheck.Iter in
  let yaml_gen : Yaml.value QCheck.Gen.t = QCheck.Gen.(sized @@ fix (fun self n -> match n with
    | 0 -> map (fun x -> `String x) string
    | n -> sized @@ (fun size ->
      frequency
        [ 10, self 0;
          1, let keys = list_repeat n string in
             let values = list_repeat n (self (size / n)) in
             map (fun x -> `O x) (map2 List.zip_exn keys values)
        ]
    )
  )) in
  let print_yaml v = Yaml.to_string_exn v in
  let rec shrink_yaml : Yaml.value QCheck.Shrink.t = function
    | `Null -> QCheck.Shrink.(map (fun () -> `Null) (unit ()))
    | `Bool b -> QCheck.Shrink.(map (fun () -> `Bool b) (unit ()))
    | `Float f -> QCheck.Shrink.(map (fun () -> `Float f) (unit ()))
    | `String str -> QCheck.Shrink.string str >|= (fun s -> `String s)
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
  list (pair string arb_val)
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
    string
    (string_map yaml).gen
    (list string)
))
let print_section v = Format.sprintf !"%{sexp:section}" v
let shrink_section : section QCheck.Shrink.t = function
  | { location; filename; metadata; content; } ->
    let open QCheck.Shrink in
    let shrink_quad = quad
        (option shrink_location)
        string
        (Option.value_exn (string_map yaml).shrink)
        (list ~shrink:string) in
    let open QCheck.Iter in
      shrink_quad (location, filename, metadata, content)
        >|= (fun (location, filename, metadata, content) -> { location; filename; metadata; content; })
let section : section QCheck.arbitrary =
  QCheck.make section_gen ?print:(Some print_section) ?shrink:(Some shrink_section)

let archive : archive QCheck.arbitrary =
  let archive_gen : archive QCheck.Gen.t = QCheck.Gen.(
    map2 (fun metadata sections -> { metadata; sections; })
      (string_map yaml).gen
      (list section_gen)
  ) in
  let print_archive v = Format.sprintf !"%{sexp:archive}" v in
  let shrink_archive : archive QCheck.Shrink.t = function
    | { metadata; sections; } ->
      let open QCheck.Shrink in
      let shrink_pair = pair
          (Option.value_exn (string_map yaml).shrink)
          (list ~shrink:shrink_section) in
      let open QCheck.Iter in
        shrink_pair (metadata, sections)
          >|= (fun (metadata, sections) -> { metadata; sections; })
  in
  QCheck.make archive_gen ?print:(Some print_archive) ?shrink:(Some shrink_archive)
