open Core
open Gen

let test_yaml_to_string =
  QCheck.Test.make ~count:1000 ~name:"Yaml.to_string_is_involutive"
   yaml
   (fun a -> Yaml.equal a (Yaml.of_string_exn (Yaml.to_string_exn a)));;

let test_to_string =
  QCheck.Test.make ~count:1000 ~name:"StringConversion.to_string_is_involutive"
   archive
   (fun a -> Or_error.compare Epar.Prim.compare_archive (Or_error.return a) (Epar.StringConversion.of_string_or_error (Epar.StringConversion.to_string a)) = 0);;

let test_epar_1_0_write_section_sub ~linebreak ~label =
  let defaults = Map.empty (module String) in
  let filename = "test" in
  QCheck.Test.make ~count:1000 ~name:("Epar.Epar_0_1.write_section_to_string_is_a_section:" ^ label)
    (string_utf8_with_unified_linebreak linebreak)
    (fun str ->
      String.equal
        str
        (Epar.Epar_0_1.(write_section_to_string ~defaults (read_section_from_string ~defaults ~filename str))));;

let test_epar_1_0_write_section_lf =
  test_epar_1_0_write_section_sub ~linebreak:"\n" ~label:"lf";;

let test_epar_1_0_write_section_cr =
  test_epar_1_0_write_section_sub ~linebreak:"\r" ~label:"cr";;

let test_epar_1_0_write_section_crlf =
  test_epar_1_0_write_section_sub ~linebreak:"\r\n" ~label:"crlf";;

(* we can check right now the property... *)
QCheck_runner.run_tests ~verbose:true ~colors:true [
    (* test_to_string; *)
    test_epar_1_0_write_section_lf;
    test_epar_1_0_write_section_cr;
    test_epar_1_0_write_section_crlf;
  ];;
