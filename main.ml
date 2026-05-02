let interpret s =
  let tape = Array.make 30000 0 in
  let ptr = ref 0 in

  let skip_forward s i =
    let depth = ref 1 in 
    let j = ref (i+1) in
    while !depth > 0 do
      (match s.[!j] with 
      | '[' -> incr depth
      | ']' -> decr depth
      | _ -> ());
    incr j
    done;
    !j
  in

  let skip_backward s i = 
    let depth = ref 1 in 
    let j = ref (i-1) in 
    while !depth > 0 do 
      (match s.[!j] with 
      | '[' -> decr depth 
      | ']' -> incr depth 
      | _ -> ()); 
    if !depth > 0 then decr j
    done;
    !j
  in
  
  let rec run i = 
    if i >= String.length s then ()
    else match s.[i] with
    | '+' -> tape.(!ptr) <- tape.(!ptr) + 1 land 255; run (i+1)
    | '-' -> tape.(!ptr) <- tape.(!ptr) - 1 land 255; run (i+1)
    | '>' -> incr ptr; run (i+1)
    | '<' -> decr ptr; run (i+1)
    | '.' -> print_char (Char.chr tape.(!ptr)); run (i+1)
    | ',' -> (
      print_endline "The program is requesting a character input.";
      match In_channel.input_char In_channel.stdin with 
      | None -> run (i+1)
      | Some c -> tape.(!ptr) <- Char.code c
      ); run (i+1)
    | '[' -> (
      if tape.(!ptr) = 0 then run (skip_forward s i)
      else run (i+1)
    )
    | ']' -> (
      if tape.(!ptr) = 0 then run (i+1)
      else run (skip_backward s i)
    )
    | _ -> run (i+1)
  in
  run 0;
  print_newline ();;

let main () = 
  let ic = open_in "source.bf" in
  let s = In_channel.input_all ic in 
  print_endline "Program output:";
  interpret s;;

main ()