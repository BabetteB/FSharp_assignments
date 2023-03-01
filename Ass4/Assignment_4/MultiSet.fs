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
    | _     -> M (Map.add a (v - n) m)
    
let removeSingle (a : 'a) (M m) : MultiSet<'a> =
    let v = numItems a (M m)
    match v with
    | 0u    -> M (Map.add a 0u m)
    | _     -> M (Map.add a (v - 1u) m)
    
let fold (f : 'a -> 'b -> uint32 -> 'a) (acc : 'a) (M m) : 'a=
    Map.fold f acc m
    
let foldBack (f : 'a -> uint32 -> 'b -> 'b) (M m) acc =
    Map.foldBack f m acc
    
(* YELLOW *)
(* 4.2 *)

let ofList a = M (Map.ofList a)

let toList (M m) = Map.toList m  

let map (f : 'a -> 'b) (M m) = M (Map.map f m)


let union (M a) (M b) =
    let m1 = toList (M a)
    let m2 = toList (M b)
    ofList (m1 @ m2)
let sum (M a) (M b) =
    fold (fun m k v -> add k v m) (M a) (M b)

let subtract (M a) (M b) =
    fold (fun m k v -> remove k v m) (M a) (M b)

let intersection (M a) (M b) =
    let aux m k =
        match (Map.tryFind k m) with
        | Some i    -> i
        | None      -> 0u
    fold (fun m k v -> if ((aux m k) <> 0u) then (Map.add k v m) else (Map.remove k m) ) a (M b)


