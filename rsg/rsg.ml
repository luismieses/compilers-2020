let () =
  Random.self_init ();

  let students = Csv.load "students.csv" in
  let numrows = Csv.lines students in
  let tab = Csv.to_array students in
  let rownum = Random.int (numrows - 1) in
  print_endline ( tab.(1 + rownum).(0) ^ " " ^ tab.(1 + rownum).(1) )
;;
