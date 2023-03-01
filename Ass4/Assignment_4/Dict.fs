module Dic

(* Green *)
type Dict = DS of string list

let empty () : Dict = DS List.empty<string>
let insert (s : string) (DS d) : Dict = DS (d @ [s])

let lookup (s : string) (DS d) : bool =
    List.contains s d
    
(* Yellow *)
type Trie =
    | Leaf of char * bool
    | Node of bool * (Map<char, Trie>)

let empty () = Leaf (System.Char.MinValue, false)
let insert (s : string) (t : Trie) =
    
    let rec insertion (s : string) (tree : Trie) : Trie =
        match tree with
        | Leaf (c, _)           when (s.Length = 1)   -> Leaf (s.[0], true)
        | Node (_, m)      when (s.Length = 1)   -> Node (true, Map.empty )
        
        | Node (b, m)                            ->
            match Map.tryFind s.[0] m with
            | Some v        -> Node(b, (Map.add s.[0] (insertion (s.Remove (0, 1)) v) m))
        | Leaf (c, b)           when (c < 'n')  -> Node ((c, b), insertion (s.Remove (0, 1)) empty)
    insertion s t