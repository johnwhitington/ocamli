(*print_string (string_of_int [%compiletime List.map (fun x -> x + 1) [1; 2; * 3]])*)

print_string (string_of_int [%compiletimestr "List.map (fun x -> x + 1) [1; 2; 3]"])
