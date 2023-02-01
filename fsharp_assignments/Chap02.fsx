
let f = fun y -> y+3;;
let g = fun x -> x*x;;
let h = f << g;; // f(g)  dvs. (x*x) + 3

//cube with water
let weight density = fun sidelength -> density * sidelength**3.0;;

let waterWeight = weight 1000.0;;

let methanolWeight = weight 786.5;;

//higher-order
let weight2 density sidelength = density * sidelength**3.0;;

// closure is a value that is a function
// a closure is a triple : (x, exp, env) 
//                          ^identifier, exp to be eval, env used to eval exp

let pi = System.Math.PI;;
let circleArea r = pi * r * r;;
(*
    ^declarations bind pi to a float and circleArea to a closure:
    env = [pi->3.1415..., circleArea->(r, pi*r*r, [pi->3.1415])]
*) 

//function operations :
(*
    argument    |>  function
    function    <|  argument
*)


(* ---- EXERCISES ---- *)
// 2.1  Declare a function f: int -> bool such that f(n) = true exactly when n is divisible by 2 or 3 but not by 5
//      Write down the expected values of f(24), f(27), f(29) and f(30)
let f2 n = ((n%2=0) || (n%3=0)) && (not(n%5=0));;
(*
    f(24) = true
    f(27) = true
    f(29) = false
    f(30) = false
*)

//2.2 Declare an F# function pow: string * int -> string, where: pow(s, n) = s*s*...*s (n times) OBS : string concat
let pow (s : string, n : int) : string = 

    let rec aux (txt : string, t : int) =
        match t with
        | 1 -> txt
        | i when i > 0 -> txt + aux((txt), (i-1))
        | _ -> failwith "cannot match number"

    aux(s,n);;

//2.3 Declare the F# function isIthChar: string * int * char -> bool where the value of
//      isIthChar(str, i, ch) is true if and only if ch is the i’th character in the string str (numbering
//      starting at zero)
let isItChar(s : string, i, c) = s.[i] = c;;

//2.4  Declare the F# function occFromIth: string * int * char -> int where
//      it returns the number of occur. of char c in positions j >= i in string s
let occFromIth(s : string, i, c) = 1;;