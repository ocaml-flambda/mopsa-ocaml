let uid_counter = ref 0
    
let timers : (int, float) Hashtbl.t = Hashtbl.create 10;;

let start () =
  incr uid_counter;
  Hashtbl.replace timers !uid_counter (Sys.time());
  !uid_counter
    
let stop uid =
  let t = Sys.time() in
  let t' = Hashtbl.find timers uid in
  Hashtbl.remove timers uid;
  t -. t'
