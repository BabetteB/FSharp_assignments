module Dic
// Collection of strings (lists, or sets for instance)
type Dict

val emptyDS : unit -> Dict

val insertDS : string -> Dict -> Dict

val lookupDS : string -> Dict -> bool

// Trie
type Trie

val emptyT : unit -> Trie

val insertT : string -> Trie -> Trie

val lookupT : string -> Trie -> bool

val stepT : char -> Trie -> (bool * Trie) option

// Gaddag
type Gaddag

val empty   : unit -> Gaddag
val insert  : string -> Gaddag -> Gaddag

val step    : char -> Gaddag -> (bool * Gaddag) option
val reverse : Gaddag -> (bool * Gaddag) option

val lookup : string -> Gaddag -> bool
