open Core
open Gen

let test_to_string =
  QCheck.Test.make ~count:1000 ~name:"StringConversion.to_string_is_involutive"
   archive
   (fun a -> Or_error.compare Epar.Prim.compare_archive (Or_error.return a) (Epar.StringConversion.of_string_or_error (Epar.StringConversion.to_string a)) = 0);;

(* we can check right now the property... *)
QCheck_runner.run_tests ~verbose:true ~colors:true [test];;
