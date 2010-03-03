type ascii =
    NULL
  | BELL
  | BACKSPACE
  | TAB
  | LF
  | FF
  | CR
  | ESC
  | DEL
  | CNTRL of char
  | CHAR of char
  | EXT of char
module Class :
  sig
    type t = int
    val isIn : int -> int -> bool
    val _PRINTABLE : int
    val _CONTROL : int
    val _EXTENDED : int
    val _ULETTER : int
    val _LLETTER : int
    val _DDIGIT : int
    val _BDIGIT : int
    val _ODIGIT : int
    val _HDIGIT : int
    val _PUNCTUATOR : int
    val _BRACKET : int
    val _LBRACKET : int
    val _RBRACKET : int
    val _ARITHMETIC : int
    val _RELATION : int
    val _LOGIC : int
    val _QUOTE : int
    val _OTHER : int
    val names : string array
    val nClasses : int
    val toString : int -> string
    val table : int array
    val get : char -> int
  end
val toString : ascii -> string
val fromChar : char -> ascii
val toChar : ascii -> char
val asciiStream : char Stream.t -> ascii Stream.t
