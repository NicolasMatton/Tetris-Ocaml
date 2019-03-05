val n : unit
type kind = Barre | Lr | Ll | Square | T | Zr | Zl
val barreI : (int * int) list
val lrI : (int * int) list
val llI : (int * int) list
val squareI : (int * int) list
val tI : (int * int) list
val zrI : (int * int) list
val zlI : (int * int) list
type tetramino = { k : kind; mutable squares : (int * int) list; }
val makeT : kind -> tetramino
val make_random : unit -> tetramino
val down : tetramino -> unit
val left : tetramino -> unit
val right : tetramino -> unit
