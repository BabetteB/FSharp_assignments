module Multi

(* GREEN *)
(* E 4.1 *)
type MultiSet<'a>

val empty   : MultiSet<'a>
val isEmpty : MultiSet<'a> -> bool

val size        : MultiSet<'a> -> uint32
val contains    : 'a -> MultiSet<'a> -> bool
val numItems    : 'a -> MultiSet<'a> -> uint32

val add         : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
val addSingle   : 'a -> MultiSet<'a> -> MultiSet<'a>
val remove      : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
val removeSingle: 'a -> MultiSet<'a> -> MultiSet<'a>

val fold        : ('a -> 'b -> uint32 -> 'a) -> 'a -> MultiSet<'b> -> 'a
val foldBack    : ('a -> uint32 -> 'b -> 'b) -> MultiSet<'b> -> 'b -> 'b

(* YELLOW *)
(* E 4.2 *)
val ofList  : 'a list -> MultiSet<'a>
val toList  : MultiSet<'a> -> 'a list
val map     : ('a -> 'b) -> MultiSet<'a> -> MultiSet<'b>

val union       : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
val sum         : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
val subtract    : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
val intersection: MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
 