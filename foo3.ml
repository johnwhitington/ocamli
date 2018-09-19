let r = ref 3

let () = for x = (r := 1; 2) to !r do () done

