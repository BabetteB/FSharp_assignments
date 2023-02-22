(* GREEN *)
(*E 1.1 : Write a function sqr : int -> int that given an integer x returns x squared *)
let sqr a = a * a;;

(*E 1.2 : Write a function pow : float -> float -> float that given two floating point numbers x and n
returns x to the power of n *)
let pow x n = System.Math.Pow(x, n);;

(*E 1.3 : Write a recursive function sum : int -> int such that given an integer n such that n 0 returns the
sum of all integers from 0 to n inclusive.*)
let rec sum = function
| 0 -> 0
| n when n > 0 -> n + sum (n-1)
| n when n > 0 -> n + sum (n+1)
| _ -> failwith "cannot summarize argument";;

(*E 1.4 : Fibonacci*)
let rec fib n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | n -> fib(n-1) + fib(n-2);;

(*E 1.5 : Write a function dup : string -> string that given a string s concatenates s with itself. You can either
use + or ˆ to concatenate strings.*)
let dup (s : string) = s + s;;

(* E 1.6 : Write a function dupn : string -> int -> string that given a string s and an integer n concatenates
s with itself n times.*)
let dupn s n = 
    let txt = ""
    let rec aux n =
        match n with
        | 0 -> txt
        | 1 -> txt + s
        | n -> txt + s + aux(n-1)
    aux(n);;

(* E 1.7 : Declare a function bin : int * int -> int that given a pair (n, k) computes  binomial coefficients*)
let bin (n, k) =
    let rec aux (i, j) =
        match (i, j) with
        | (_, 0) -> 1
        | (h, g) when h = g -> 1
        | (h, g) -> aux((h-1), (g-1)) + aux((h-1), g)
    aux(n,k);;



(* YELLOW *)

(* E 1.8 : Write a function timediff : int * int -> int * int->int so that timediff t1 t2 computes the
difference in minutes between t1 and t2 , i.e., t2-t1 *)
let timediff (h1, m1) (h2, m2) =
    let t1 = (h1 * 60) + m1
    let t2 = (h2 * 60) + m2
    t2 - t1;;

(* E 1.9 : Write a function minutes : int * int -> int that computes the number of minutes since midnight *)
let minutes (h, m) = timediff(0, 0) (h, m);;

(* E 1.10 : Write definitions of curry and uncurry*)
// i don't understand the question
let curry (f : 'a * 'b -> 'c) a b = f(a,b);;

let uncurry (f : 'a -> 'b -> 'c) (a, b) = f a b;;


//Scrabble assignments
type tile = (char * int);;

let t1 = ('H', 4);;
let t2 = ('A', 1);;

(* E 1.11 : Create a function empty : char * int -> (int -> char * int)*)
let empty (letter, pointValue) = 
    fun _ -> (letter, pointValue);;

(* E 1.12 : Create a function add : int -> (char * int) -> (int -> char * int) -> (int -> char * int)*)
let add (newPosition : int) (charAndValue : char * int) (word : int -> char * int) =
    fun pos -> if pos = newPosition then charAndValue else word pos;;

(* E 1.13 : create the function hello : int -> char * int that spells HELLO*)
let hello : int -> char * int =
    add 0 ('H', 4) (empty ('H', 4))
    |> add 1 ('E', 1)
    |> add 2('L', 1)
    |> add 3 ('L', 1)
    |> add 4 ('O', 1);;

let singleLetterScore (word : int -> char * int) (position : int) = 
    1 * snd(word position);;

let doubleLetterScore (word : int -> char * int) (position : int) = 
    2 * snd(word position);;

let trippleLetterScore (word : int -> char * int) (position : int) = 
    3 * snd(word position);;