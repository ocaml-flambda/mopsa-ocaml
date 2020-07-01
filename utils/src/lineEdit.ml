(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

(**
   Simple line-editing for interactive sessions in the terminal.
   Assumes an UTF-8 console with standard ANSI escape codes.
*)



(** {2 Entering raw mode} *)
(** ********************* *)
   
let org_stdin_attr =
  try Some Unix.(tcgetattr stdin)
  with Unix.Unix_error _ -> None (* if Unix module is not supported *)

(** Whether terminal control is available *)                          
let tc_available () =
  org_stdin_attr <> None                          
                          
(** Enter raw mode before starting line edition. *)
let enter_raw_mode () =
  match org_stdin_attr with
  | Some attr ->
     let open Unix in
     let tt =
       { attr with
         c_isig=false; c_ixon=false; c_icanon=false; c_echo=false; 
         c_icrnl=false; c_inlcr=false;
         c_vmin=0; c_vtime=1;
       }
     in
     tcsetattr stdin TCSADRAIN tt
  | None -> failwith "Raw terminal mode not supported (tcsetattr not available)"
     
(** Exit raw mode after finishing line edition. 
    Do not call print to the console when in raw mode.
 *)
let exit_raw_mode () =
  match org_stdin_attr with
  | None -> ()
  | Some attr -> Unix.(tcsetattr stdin TCSADRAIN attr)

(** Ensure that we restore the terminal when quitting. *)
let _ = at_exit exit_raw_mode



(** {2 FIFO character buffer} *)
(** ************************* *)

module FIFO = struct

  type t = {
      mutable cin: char list;  (* chars are accumulated here *)
      mutable cout: char list; (* chars are read from here *)
    }

  let create () =
    { cin = []; cout = []; }

  let is_empty (b:t) : bool =
    b.cin = [] && b.cout = []

  let length (b:t) : int =
    List.length b.cin + List.length b.cout
    
  let add (b:t) (c:char) : unit =
    b.cin <- c::b.cin

  let get (b:t) : char =
    match b.cout with
    | a::r -> b.cout <- r; a
    | [] ->
       (* time to get more chars from the accumulated buffer *)
       match List.rev (b.cin) with
       | a::r -> b.cin <- []; b.cout <- r; a
       | [] -> invalid_arg "Empty FIFO"

  let get_opt (b:t) : char option =
    if is_empty b then None else Some (get b)
             
  let clear (b:t) : unit =
    b.cin <- []; b.cout <- []
    
end
                           
                           
      
(** {2 Character input} *)
(** ******************* *)


(** Non blocking read_char. *)
let read_char_unbuf_opt () : char option =
  flush stdout;
  let open Unix in
  let b = Bytes.make 1 ' ' in
  let r = read stdin b 0 1 in
  if r  = 0 then None
  else if r = 1 then Some (Bytes.get b 0)
  else failwith "Read error"

(** Block until a char is available and return it. *)  
let rec read_char_unbuf () : char =
  match read_char_unbuf_opt () with
  | Some x -> x
  | None -> read_char_unbuf ()


(** We may need to scan the input to get the answer for a 
    get_position call.
    Hence, we keep a FIFO of inputs read while scanning.
 *)
let rabuf = FIFO.create ()

(** Put (back) in FIFO. *)          
let unread_char (c:char) =
  FIFO.add rabuf c

let unread_char_list (c:char list) =
  List.iter unread_char c
  
(** Non blocking read_char. 
    Fetch from the read-ahead buffer first. 
*)
let read_char_opt () : char option =
  if FIFO.is_empty rabuf then read_char_unbuf_opt ()
  else Some (FIFO.get rabuf)

(** Block until a char is available and return it. 
    Fetch from the read-ahead buffer first. 
*)  
let read_char () : char  =
  if FIFO.is_empty rabuf then read_char_unbuf ()
  else FIFO.get rabuf

(** Put all chars possible in the read-ahead buffer. *)
let rec read_flush () : unit =
  match read_char_unbuf_opt () with
  | Some x -> unread_char x; read_flush ()
  | None -> ()
  
  
          
  

(** {2 Cursor manipulation} *)
(** *********************** *)
          
let pf = Printf.printf
let ps = print_string

(** Moves the cursor up, down, left, or right n positions. *)      
let cursor_up    n = pf "\027[%iA" n
let cursor_down  n = pf "\027[%iB" n
let cursor_right n = pf "\027[%iC" n
let cursor_left  n = pf "\027[%iD" n

(** Go to the begining of the n-th next / previous line. *)
let next_line n = pf "\027[%iE" n
let perv_line n = pf "\027[%iF" n

(** Move to line l, column c (starting at 1). *)
let set_column   c = pf "\027[%iG" c
let set_position l c = pf "\027[%i;%iH" l c

(** Clear the whole screen / line. *)                      
let clear_screen () = ps "\027[2J"
let clear_line   () = ps "\027[2K"

(** Clear the screen / line from the cursor position until the end. *)
let clear_end_screen () = ps "\027[0J"
let clear_end_line   () = ps "\027[0K"

(** Clear the screen / line up to the cursor position from the begining. *)
let clear_start_screen () = ps "\027[1J"
let clear_start_line   () = ps "\027[1K"

(** Scroll some number of lines up / down. *)
let scroll_up   n = pf "\027[%iS" n
let scroll_down n = pf "\027[%iT" n

(** Save / restore the cursor position. *)                     
let save_position    () = ps "\027[s"
let restore_position () = ps "\027[u"


(** Return the cursur line and column (starting at 1).
    Return (0,0) in case of an error.
 *)
let get_position () : int * int =
  (*read_flush ();*)
  (* write ESC[6n *)
  print_string "\027[6n";
  (* read back ESC[<line>;<col>R *)
  let rec wait () =
    match read_char_unbuf () with
    | '\027' -> wait2 ()
    | x -> unread_char x; wait ()
  and wait2 () =
    match read_char_unbuf () with
    | '[' -> ()
    | '\027' -> unread_char '\027'; wait2 ()
    | x -> unread_char_list ['\027'; x]; wait ()
  in
  let rec updt acc =
    let c = read_char_unbuf () in
    if c >= '0' && c <= '9' then updt (acc * 10 + Char.code c - Char.code '0')
    else c, acc
  in
  wait ();
  let r1, l = updt 0 in
  if r1 <> ';' then 0,0
  else 
    let r2, c = updt 0 in
    if r2 <> 'R' then 0,0
    else l, c

    
(** Return the height and width of the window. *)
let get_window_size () : int * int =
  (* save cursor position *)
  let l,c = get_position () in
  (* go bottom and right as far as we can *)
  cursor_right 999;
  cursor_down  999;
  (* cursor position indicates the size *)
  let h,w = get_position () in
  (* restore cursor position *)
  set_position l c;
  h, w
    

  
(** {2 UTF-8 buffers} *)
(** ***************** *)


(** Byte-size of the utf-8 char starting with code c. *)  
let sizeof_uchar (c:char) : int =
  let i = Char.code c in
  if i land 0x80 = 0 then 1
  else if i land 0xe0 = 0xc0 then 2
  else if i land 0xf0 = 0xe0 then 3
  else if i land 0xf8 = 0xf0 then 4
  else if i land 0xfb = 0xf8 then 5
  else if i land 0xfe = 0xfb then 6
  else 1 (* error *)
  

(** Buffers backed by byte sequences that grow automatically.
    Support insertion and deletion at any position within the buffer.
    Include some support for UTF-8 encoding.
 *)  
module UBuf = struct

  type t = {
      mutable buf: Bytes.t; (* buffer *)
      mutable len: int; (* nb of bytes actually used *)
    }


  (* internal function to ensure that there is room for nb more bytes *)                
  let ensure (b:t) (nb:int) =
    if b.len + nb > Bytes.length b.buf then (
      let buf = Bytes.create (2 * (b.len + nb)) in
      Bytes.blit b.buf 0 buf 0 b.len;
      b.buf <- buf
    )
      

  (** Create an empty buffer. *)
  let create () = {
      buf = Bytes.create 2;
      len = 0;
    }

  (** Creates a copy of a buffer. *)
  let copy (b:t) : t = {
      buf = Bytes.copy b.buf;
      len = b.len;
    }

  (** Buffer contents, as a string. *)
  let contents (b:t) : string =
    Bytes.sub_string b.buf 0 b.len

  (** Buffer size in bytes. *)  
  let byte_length (b:t) : int =
    b.len

  (** Buffer size in utf-8 chars. *)  
  let uchar_length (b:t) : int =
    let i, len = ref 0, ref 0 in
    while !i < b.len do
      i := !i + sizeof_uchar (Bytes.get b.buf !i);
      incr len
    done;
    !len
    
  (** Byte index of the nb-th utf-8 char. *)  
  let index_of_uchar (b:t) (nb:int) : int =
    let idx = ref 0 in
    for _ = 0 to nb-1 do
      if !idx >= b.len then invalid_arg "index_of_uchar";
      idx := !idx + sizeof_uchar (Bytes.get b.buf !idx)
    done;
    !idx

  (** Get the byte at the specified byte index. *)
  let nth (b:t) (i:int) : char =
    if i < 0 || i >= b.len then invalid_arg "nth";
    Bytes.get b.buf i

  (** Get the substring from the specified byte index. *)    
  let sub (b:t) (i:int) (len:int) : string =
    if i < 0 || i + len > b.len then invalid_arg "sub";
    Bytes.sub_string b.buf i len
    
  (** Append a byte at the end of the buffer. *)
  let add_char (b:t) (c:char) : unit =
    ensure b 1;
    Bytes.set b.buf b.len c;
    b.len <- b.len + 1
    
  (** Append a part of a string at the end of the buffer. *)
  let add_substring (b:t) (s:string) (off:int) (len:int) : unit =
    if off < 0 || len < 0 || off+len > String.length s
    then invalid_arg "add_substring";
    if len > 0 then (
      ensure b len;
      Bytes.blit_string s off b.buf b.len len;
      b.len <- b.len + len
    )
    
  (** Append a string at the end of the buffer. *)
  let add_string (b:t) (s:string) : unit =
    add_substring b s 0 (String.length s)

  (** Insert a byte at the specified byte index. *)
  let insert_char (b:t) (i:int) (c:char) : unit =
    if i < 0 || i > b.len then invalid_arg "insert_char";
    ensure b 1;
    if b.len > i then Bytes.blit b.buf i b.buf (i+1) (b.len-i);
    Bytes.set b.buf i c;
    b.len <- b.len + 1
    
  (** Insert a substring at the specified byte index. *)
  let insert_substring (b:t) (dst:int) (s:string) (src:int) (len:int) : unit =
    if src < 0 || len < 0 || src+len > String.length s
     || dst < 0 || dst > b.len
    then invalid_arg "insert_substring";
    ensure b len;
    if b.len > dst then Bytes.blit b.buf dst b.buf (dst+len) (b.len-dst);
    Bytes.blit_string s src b.buf dst len;
    b.len <- b.len + len
    
  (** Insert a string at the specified byte index. *)
  let insert_string (b:t) (dst:int) (s:string) : unit =
    if dst < 0 || dst > b.len then invalid_arg "insert_string";
    let len = String.length s in
    ensure b len;
    if b.len > dst then Bytes.blit b.buf dst b.buf (dst+len) (b.len-dst);
    Bytes.blit_string s 0 b.buf dst len;
    b.len <- b.len + len

  (** Delete some bytes at the specified byte index. *)
  let delete (b:t) (i:int) (len:int) =
    if i < 0 || len < 0 || i+len > b.len then invalid_arg "delete";
    if b.len > i + len then Bytes.blit b.buf (i+len) b.buf i (b.len-i-len);
    b.len <- b.len - len

  (** Reset the buffer to 0 length. *)
  let clear (b:t) =
    b.len <- 0;
    b.buf <- Bytes.create 2

  (** Print to buffer. *)
  let output (ch:out_channel) (b:t) =
    output_string ch (contents b)
    
end
            
  
  
(** {2 Line editing} *)
(** **************** *)


(** Line edition context, maintained between calls to read_line. *)
type ctx = {
    mutable ctx_history: UBuf.t list;
  }

let create_ctx () =
  { ctx_history = []; }


(* \\ followed by end-of-line *)
let backslash_eol =
  Str.regexp "\\\\\\(\n\\|\r\\|\r\n\\)"
  
  
(** Main line editing function, with support for cursor movement and history.
    You can print a prompt before calling this function.
    Return the string that was read.
    Throw Exit when Ctrl+C or Ctrl+D is typed.
 *)  
let read_line_tc ctx =
  ctx.ctx_history <- (UBuf.create ())::ctx.ctx_history;
  (* current buffer *)
  let buf = ref (List.hd ctx.ctx_history) in
  (* position in history *)
  let hpos = ref 0 in
  (* know when to quit *)
  let continue = ref true in
  try
    (* setup terminal parameters *)
    enter_raw_mode ();
    let h, w = get_window_size () in
    let org_l, org_c = get_position () in (* start of line *)
    let org_l = ref org_l in (* mutable, in case of scrolling *)
    let cur = ref 0 in (* cursor position, in characters within buf *)

    (* line, column where the n-th character is displayed *)
    let cur_pos n =
      let rec doit l c i n =
        if n = 0 || i >= UBuf.byte_length !buf then (l,c) else
          let x = UBuf.nth !buf i in
          if x = '\n' || x = '\r' then doit (l + 1) 1 (i + 1) (n - 1)
          else doit (l + c / w) (c mod w + 1) (i + sizeof_uchar x) (n - 1)
      in
      doit !org_l org_c 0 n
    in
    
    (* cursor position corresponding to the given line, column *)
    let pos_cur dstl dstc =
      let rec doit l c i n =
        if (l >= dstl && c >= dstc) || i >= UBuf.byte_length !buf then n else
          let x = UBuf.nth !buf i in
          if x = '\n' || x = '\r' then
            if l = dstl then n (* don't go past dstl if line is shorter than dstc *)
            else doit (l + 1) 1 (i + 1) (n + 1)
          else doit (l + c / w) (c mod w + 1) (i + sizeof_uchar x) (n + 1)
      in
      doit !org_l org_c 0 0
    in
    
    (* edition loop *)
    while !continue do
      
      (* show line *)
      set_position !org_l org_c;
      clear_end_screen ();
      UBuf.output stdout !buf;
      print_string " ";

      (* detect scrolling *)
      let end_l, _ = cur_pos (-1) in
      if end_l > h then org_l := !org_l + h - end_l;
      
      (* show cursor at the correct position *)
      cur := max 0 (min !cur (UBuf.uchar_length !buf));
      let cur_l, cur_c = cur_pos !cur in
      set_position cur_l cur_c;
      flush stdout;

      (* handle input *)
      match read_char () with

       (* return *)
      | '\r' | '\n' ->
         if !cur > 0 && UBuf.nth !buf (UBuf.index_of_uchar !buf (!cur - 1)) = '\\'
         then (
           (* after a \, insert a new-line *)
           let pos = UBuf.index_of_uchar !buf !cur in
           incr cur;
           UBuf.insert_char !buf pos '\n'
         )
        else
           (* otherwise, finish line input *)
          continue := false

       (* special characters *)
       | '\027' ->
          (match read_char () with
           | '[' ->
              (match read_char() with

               (* up *)
               | 'A' ->
                  if cur_l = !org_l then
                    if !hpos < List.length ctx.ctx_history - 1 then (
                      (* go up in history *)
                      hpos := !hpos + 1;
                      buf := List.nth ctx.ctx_history !hpos;
                      cur := UBuf.uchar_length !buf
                    )
                    else ()
                  else
                    (* go up in the line *)
                    cur := pos_cur (cur_l - 1) cur_c

               (* down *)
               | 'B' ->
                  if cur_l = end_l then
                    if !hpos > 0 then (            
                      (* go down in history *)
                      hpos := !hpos - 1;
                      buf := List.nth ctx.ctx_history !hpos;
                      cur := UBuf.uchar_length !buf
                    )
                    else ()
                  else
                    (* go down in line *)
                    cur := pos_cur (cur_l + 1) cur_c
                 
               (* right *)
               | 'C' ->
                  cur := min (UBuf.uchar_length !buf) (!cur + 1)

               (* left *)
               | 'D' ->
                  cur := max 0 (!cur - 1)

               (* home *)
               | 'H' ->
                  cur := 0
               
               (* end *)
               | 'F' ->
                  cur := UBuf.uchar_length !buf

               (* delete *)
               | '3' ->
                  let _ = read_char() in (* ~ *)
                  if !cur < UBuf.uchar_length !buf then (
                    let pos = UBuf.index_of_uchar !buf !cur in
                    let len = sizeof_uchar (UBuf.nth !buf pos) in
                    UBuf.delete !buf pos len
                  )
                  
               | _ -> ()
              )

           (* alt-enter: insert a new-line *)
           | '\n' | '\r' ->
              UBuf.insert_char !buf (UBuf.index_of_uchar !buf !cur) '\n';
              incr cur
              
           | _ -> ()
          )

       (* backspace *)
       | '\127' ->
          if !cur > 0 then (
            cur := !cur - 1;
            let pos = UBuf.index_of_uchar !buf !cur in
            let len = sizeof_uchar (UBuf.nth !buf pos) in
            UBuf.delete !buf pos len
          )          
          
       (* ctrl+C / ctrl+D: quit *)
       | '\003' | '\004' ->
          raise Exit

       (* visible character *)
       | x when x >= ' ' ->
          let pos = UBuf.index_of_uchar !buf !cur in
          incr cur;
          UBuf.insert_char !buf pos x;
          (* add remaining bytes in case of a multi-byte utf-8 character *)
          let len = sizeof_uchar x in
          for i = 1 to len-1 do
            UBuf.insert_char !buf (pos+i) (read_char())
          done

       (* unhandled control character *)          
       | _ -> ()
      
    done;
    
    (* fix history *)
    if !hpos <> 0 then
      ctx.ctx_history <- (UBuf.copy !buf)::(List.tl ctx.ctx_history);
    
    (* end: fix terminal *)
    print_string "\n";
    flush stdout;
    exit_raw_mode();

    (* return string, removing \ at end of lines *)
    let s = (UBuf.contents !buf)^"\n" in
    Str.global_replace backslash_eol "\n" s

  with x ->
    (* error: fix terminal and re-raise exception *)
    print_string "\n";
    flush stdout;
    exit_raw_mode ();
    raise x

    
(** Line edition, with fall-back to Stdlib.read_line if there is no terminal control. *)
let read_line ctx =
  if tc_available () then read_line_tc ctx
  else Stdlib.read_line ()
  
