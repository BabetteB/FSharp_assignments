module Multi

type MultiSet<'a>  when 'a : comparison = M of Map<'a, uint32>

let empty : MultiSet<'a> = M Map.empty
let isEmpty (M m) : bool = Map.isEmpty m

let size        (M m)           : uint32    = uint32 (Map.count m)
let contains    (a : 'a) (M m)  : bool      = Map.containsKey a m
let numItems    (a : 'a) (M m)  : uint32    =
    let v  = Map.tryFind a m
    match v with
    | Some i    -> i
    | None      -> 0u

let add (a : 'a) (n : uint32) (M m) : MultiSet<'a> =
    let v = numItems a (M m)
    M (Map.add a (v + n) m)
    
let addSingle (a : 'a) (M m) : MultiSet<'a> =
    let v = numItems a (M m)
    M (Map.add a (v + 1u) m)

let remove (a : 'a) (n : uint32) (M m) : MultiSet<'a> =
    let v = numItems a (M m)
    match v with
    | 0u    -> M (Map.add a 0u m)
    | n     -> M (Map.add a (v - n) m)
    
let removeSingle (a : 'a) (M m) : MultiSet<'a> =
    let v = numItems a (M m)
    match v with
    | 0u    -> M (Map.add a 0u m)
    | _     -> M (Map.add a (v - 1u) m)
    
let fold (f : 'a -> 'b -> uint32 -> 'a) (acc : 'a) (M m) : 'a=
    Map.fold f acc m
    
let foldBack (f : 'a -> uint32 -> 'b -> 'b) (M m) (acc : 'a) =
    Map.foldBack f m acc
    
(* YELLOW *)
(* 4.2 *)
