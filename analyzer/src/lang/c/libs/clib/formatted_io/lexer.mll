{
  open Parser
  let debug fmt = Debug.debug ~channel:"foo" fmt
}

let num = ['0'-'9'] ['0'-'9']*

rule read =
  parse
  | "%%"        { read lexbuf }
  | "%"         { debug "perc"; PERCENT }
  | "+"         { debug "+";PLUS }
  | "-"         { debug "-";MINUS }
  | "0"         { ZERO }
  | "#"         { SHARP }
  | num         { NUM }
  | "*"         { STAR }
  | "."         { DOT }
  | "hh" | "HH"	{ HH }
  | "h" | "h"   { H }
  | "ll" | "LL" { LL }
  | "l" | "L"   { L }
  | "d"         { debug "d";D }
  | "i"         { debug "i";I }
  | "u"         { debug "u";U }
  | "f" | "F"   { debug "f";F }
  | "g" | "G"   { G }
  | "a" | "A"   { A }
  | "p"         { P }
  | "s"		{ S }
  | "x" | "X"	{ X }
  | "o"	  	{ O }
  | "c" 	{ C }
  | eof         { debug "eof";EOF }
  | _           { read lexbuf }