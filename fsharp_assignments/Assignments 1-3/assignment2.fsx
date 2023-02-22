(* GREEN *)
(* E 2.1 : Write a function downto1 : int -> int list that given an integer returns the -element list [n; n-1;
...; 1] if and [] otherwise. Usa if-else *)
let downto1 n =
    if (n = 0) then [] else [ n .. -1 .. 1 ];;

let downto2 n =
    match n with
    | 0 -> []
    | _ -> [ n .. -1 .. 1 ];;

(* E 2.2 : Write a function removeOddIdx : 'a list -> 'a list that given a list xs returns a list where all 
        oddindexed elements of xs have been removed*)
let removeOddIdx (xs : 'a list) =
    let rec aux ys =
        match ys with
        | [] -> []
        | y:: [] -> ys
        | y::z::ys -> y :: aux(ys)
    aux(xs) ;;

(* E 2.3 : Write a function combinePair : 'a list -> ('a * 'a) list that given a list xs returns the list with
elements from xs combined into pairs. *)
let combinePair xs =
    let rec aux ys =
        match ys with
        | [] -> []
        | y::[] ->[]
        | y::z::ys -> (y, z) :: aux(ys)
    aux(xs);;


(* E 2.4 : 
    Define a type complex with floating point components
    Define a function mkComplex : float -> float -> complex that given two floating point numbers return
        the corresponding complex number
    Define a function complexToPair : complex -> float * float that given a complex number
        returns the pair (a, b) .*)
type complex = float * float;;

let mkComplex (a : float) (b : float) : complex  = (a,b);;
let complexToPair (a : complex) : float * float = a;;

let (|+|) (a : complex) b = (((fst a) + (fst b)), ((snd a) + (snd b)));;
let (|-|) (a : complex) b = a |+| (-(fst b), -(snd b));;
let (|*|) (a : complex) b = 
    (((fst a) * (fst b) - (snd a) * (snd b)), (fst a) * (snd b) + (snd a)* (fst b) );;

let (|/|) (a : complex) (b : complex) : complex = 
    let (b1, b2) = (fst b, snd b)
    a |*| mkComplex ( b1 / ((b1**2.0) + (b2**2.0)) ) ( ((-b2) / ((b1**2.0) + (b2**2.0))) );;

(* E 2.5 : Write a non-recursive function explode1 : string -> char list that given a string s returns the list of
characters in s .
write a recursive function explode2 : string -> char list that has the same semanics as
explode
*)
let explode1 (s : string) : char list = 
    s.ToCharArray() |> List.ofArray ;;

let explode2 (s : string) : char list =
    let rec aux txt i =
        match txt with 
        | "" -> []
        | _ -> txt.[0] :: aux (txt.Remove(0, 1)) (i-1)
    aux s s.Length;;

(* E 2.6 : Write a function implode : char list -> string that given a list of characters cs 
returns a string with all characters of cs in the same orde *)
let implode (cs : char list) : string =
    List.fold(fun acc lst -> acc + string lst) "" cs

let implodeRev (cs : char list) : string =
    List.foldBack(fun lst acc -> acc + string lst) cs ""

(* E 2.7 : Write a function toUpper : string -> string that given a string s
returns s with all characters in upper case.*)
let toUpper (s : string) : string = 
    let cs = s.ToCharArray()
    List.fold(fun acc lst -> acc + string (System.Char.ToUpper lst)) "" (cs |> Array.toList)

(* E 2.8 : Write the function ack : int * int -> int that given an integer pair (m, n) implements the
Ackermann function using pattern matching*)
let ack (p : int * int) =
    let rec aux k =
        match k with
        | (0, n)                        -> n+1
        | (m, 0)    when m > 0          -> aux((m-1), 1)
        | (m, n)    when m > 0 && n > 0 -> aux((m-1), aux(m, (n-1)))
        | _                             -> failwith "method only evaluates positive numbers"
    aux p;;

(* YELLOW *)
(* E 2.9 : Write a new function timeArg1 : ('a -> 'b) -> 'a -> 'b * TimeSpan
that given a function f and an argument a , times the computation of evaluating the function f with
argument a *)
let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start);;

let timeArg1 f a =
    let start = System.DateTime.Now
    let res = f (a)
    let finish = System.DateTime.Now
    let timeSpendt = finish - start
    (res, timeSpendt);;

(* E 2.10 : Declare a function downto3 : (int -> 'a -> 'a) -> int -> 'a -> 'a such that
if n greater or equal 0 when f 1 ( f 2 (...(f (n-1) (f n e))))
Declare the factorial function fac : int -> int by use of downto3
Use downto3 to declare a function range : (int -> 'a) -> int -> 'a list that given a function g
and an integer n returns the list of [g 1, g 2, ..., g n] if n is positive, and the empty list otherwise
*)
let rec downto3 (f : int -> 'a -> 'a) (n : int ) (e : 'a) =
    match n with
    | i     when i <= 0     -> e
    | i                     -> f i (downto3 f (i-1) e);;

let rec fact n =
    match n with
    | 0 -> 1
    | n -> n * fact(n-1) ;;

let fac (n : int) : int = 
    downto3 (fun i acc -> acc * i ) n 1 ;;

let range (g : int -> 'a) (n : int) : 'a list =
    let rec aux f x =
        match x with
        | y     when n >= y     -> f y :: (aux f (y+1))
        | _                     -> []
    aux g 1;;

(*Scrabble*)
// last week
type tile = (char * int);;

// this week
type word = (char * int) list

(*E 2.11 : Create the value hello of type word that spells the word HELLO*)
let hello : word =
    ('H', 4) :: ('E', 1) :: ('L', 1) :: ('L', 1) :: ('O', 1) :: [];;
let helllo : word =
    ('H', 4) :: ('E', 1) :: ('L', 1) :: ('L', 1) :: ('L', 1) :: ('O', 1) :: [];;

(* E 2.12 : Create the square functions singleLetterScore , doubleLetterScore and tripleLetterScore that all
have the type squareFun*)
type squareFun = word -> int -> int -> int

let singleLetterScore (word : word) (pos : int) (acc : int) : int =
    snd word.[pos] + acc;;

let doubleLetterScore (word : word) (pos : int) (acc : int) : int =
    2* (snd word.[pos]) + acc;;

let tripleLetterScore (word : word) (pos : int) (acc : int) : int =
    3* (snd word.[pos]) + acc;;

(* E 2.13 : Create the square functions doubleWordScore and tripleWordScore that both have the type squareFun*)
let doubleWordScore (word : word) (pos : int) (acc : int) : int =
    2 * acc;;

let tripleWordScore (word : word) (pos : int) (acc : int) : int =
    3 * acc;;

(* E 2.14 : Create a square function oddConsonants of type squareFun that negates the accumulator if there are an
odd number of consonants in the word placed over the square*)
let isConsonant (c : char) =
    let lc = System.Char.ToLower c
    match lc with
    | 'b' | 'c' | 'd' | 'f' | 'g' | 'h' | 'j' | 'k' | 'l' | 'm' | 'n' | 'p'
    | 'q' | 'r' | 's' | 't' | 'v' | 'w' | 'x' | 'z' -> true
    | _ -> false;;

let oddConsonants (word : word) (pos : int) (acc :int) = 
    let cs = List.map (fun x -> fst x) word
    let b = (List.filter isConsonant cs) |> List.length |> (fun x -> x%2=0)
    if b then acc else (-acc)


(* RED *)
(* E 2.15 *)
//     (priority, (word, pos, acc))  <=> (priority, ((char * int) lst), pos, acc))
type square = (int * squareFun) list

let SLS : square = [(0, singleLetterScore)];;
let DLS : square = [(0, doubleLetterScore)];;
let TLS : square = [(0, tripleLetterScore)];;

let DWS : square = SLS @ [(1, doubleWordScore)];;
let TWS : square = SLS @ [(1, tripleWordScore)];;

let calculatePoints (squares : square list) (word : word) : int =
    //((int * (int -> int)) list) list  <=>     (prioritet * (word -> pos) list) list
    let priWordPoslst           = squares               |> List.mapi    (fun i s1 ->  (List.map (fun s -> (fst s), ((snd s) word i)))  s1) 
    let flattenPriWordPoslst    = priWordPoslst         |> List.fold    (@) []
    let sortFlatPriWordPoslst   = flattenPriWordPoslst  |> List.sortBy  (fun ls -> fst ls)
    let wordList                = sortFlatPriWordPoslst |> List.map     (fun xs -> snd xs)
    let fcmsl = wordList     |> List.fold    (>>) id
    fcmsl 0