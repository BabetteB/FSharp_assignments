module Dic
// Collection of strings (lists, or sets for instance)
type Dict

val empty : unit -> Dict

val insert : string -> Dict -> Dict

val lookup : string -> Dict -> bool

// Trie
type Trie

val empty : unit -> Trie

val insert : string -> Trie -> Trie

val lookup : string -> Trie -> bool

val step : char -> Trie -> (bool * Trie)

// Gaddag