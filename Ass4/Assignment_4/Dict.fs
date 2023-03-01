module Dic

(* Green *)
type Dict = DS of string list

let emptyDS () : Dict = DS List.empty<string>
let insertDS (s : string) (DS d) : Dict = DS (d @ [s])

let lookupDS (s : string) (DS d) : bool =
    List.contains s d
    
(* Yellow *)
type Trie =
    | Leaf of char * bool
    | Node of bool * Map<char, Trie>

let emptyT () = Leaf (System.Char.MinValue, false)
let insertT (s : string) (t : Trie) =
    let rec insertion (s : string) (tree : Trie) : Trie =
        match tree with
        | Leaf _           when (s.Length = 1)   -> Leaf (s.[0], true)
        | Node _           when (s.Length = 1)   -> Node (true, Map.empty )
        
        | Leaf (_, b)                            ->
            Node(b, (Map.add s.[0] (insertion (s.Remove (0, 1)) (emptyT ())) Map.empty ) )
        | Node (b, m)                            ->
            match Map.tryFind s.[0] m with
            // if we find a trie with the char s.[0] then we will overwrite the node with a new node, keep the boolean but insert the rest of the word within the submap of that node, including the char
            | Some v        -> Node(b, (Map.add s.[0] (insertion (s.Remove (0, 1)) v) m))
            // if the char does not exist within the node map, we need to add it and continue the subtrie
            | None          -> Node(b, (Map.add s.[0] (insertion (s.Remove (0, 1)) (emptyT ())) m)) 
    insertion s t
    
let lookupT (word : string) (tree : Trie) =
    let rec look (s : string) t =
        match t with
        | Leaf (_, b)       when (s.Length = 1)     -> b // when we at the end of the tree AND at the end of the word - return weather it is a word
        | Leaf _                                    -> false // at the end of the tree, but there is still more letters in the word
        | Node (b, _)       when (s.Length = 1)     -> b // we at end of the word mid-tree, then bool says weather the subtree creates a word
        | Node (_, m)                               ->
            match m.TryFind s.[0] with
            | Some v    -> look (s.Remove (0, 1)) v
            | None      -> false
    look word tree
    
let stepT (character : char) (tree : Trie) =
    match tree with
    | Leaf _        -> None
    | Node (b, m)   ->
        match (m.TryFind character) with
        | Some v        -> Some (b, v)
        | None          -> None
        
        
(* Red *)
type Gaddag =
    | Leaf of char * bool
    | Node of bool * Map<char, Gaddag>
    
let empty () = Leaf (System.Char.MinValue, false)

let insert (word : string) (dictionary : Gaddag) =
    let rec aux (w : string) (d : Gaddag) =
        match d with
        | Leaf _        when (w.Length = 1) -> Leaf (w.[0], true)
        | Leaf (_, b)                            ->
            Node(b, (Map.add w.[0] (aux (w.Remove (0, 1)) (empty ())) Map.empty ) )
        | Node (b, m)                            ->
            match Map.tryFind w.[0] m with
            | Some v        -> Node(b, (Map.add w.[0] (aux (w.Remove (0, 1)) v) m))
            | None          -> Node(b, (Map.add w.[0] (aux (w.Remove (0, 1)) (empty ())) m)) 
    aux word dictionary