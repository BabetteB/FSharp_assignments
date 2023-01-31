//wsl : dotnet fsi

2*3+4;;

let price = 125;;

price * 20;;

it/price=20;;

System.Math.PI;;

let circleAreal r = System.Math.PI * r * r;;

circleArea 1.0;;

circleArea 2.0;;

fun r -> System.Math.PI *r*r;;

it 2.0;;

function
| 2 -> 28
| 4 | 6 | 9 | 11 -> 30
| _ -> 31;;

it 2;;

let daysOfMonth = function
| 2 -> 28
| 4 | 6 | 9 | 11 -> 30
| _ -> 31;;


daysOfMonth 3;;
daysOfMonth 9;;

let rec fact = function
| 0 -> 1 
| n when n > 0 -> n * fact(n-1)
| _ -> failwith "n must be positive for fact to work" ;;

fact 4;;
fact (-1);;

let a = (2.0, 3);;
let (x,y) = a;;

let rec power = function
| (x, 0) -> 1.0
| (x, n) -> x * power(x, (n-1));;

power a;;

power (4.0, 2);;

let a = 3;;

let b = 7.0;;

/// env1 = [a->3, b->7.0]

let c = (2, 8);;

let circleArea r = System.Math.PI * r * r;;

/// // env1 = [a->3, b->7.0, c->(2,8), circleArea->"circle area function"]

let rec gcd = function
| (0, n) -> n
| (m, n) -> gcd(n%m, m);;

gcd(12, 27);;

gcd(36, 116);;

[<EntryPoint>]
let main (param: string[]) = 
    printf "Hello %s\n" param.[0]
    0;;


(* ---- EXERCISES ---- *)
//1.1 Declare a function g: int -> int, where g(n) = n + 4.
let g n = n+4;;

//1.2 Declare a function h: float * float -> float, where h(x, y) = sqrt(x**2 + y**2)
let h (f : float * float) = System.Math.Sqrt((fst f)**2 + (snd f)**2);;


//1.3  Write function expressions corresponding to the functions g and h
let g2 = function
| n -> n+4;;

let h2 = function
| ((x: float), (y: float)) ->  System.Math.Sqrt(x**2 + y**2);;

//1.4 Declare a recursive function f: int -> int, where f(n) = 1+2+...+(n-1)+n
let rec f = function
| 0 -> 0
| n -> n + f(n-1);; 

//1.5 Give an evaluations for F4 (Fibonacci)
let rec fib = function
| 0 -> 0
| 1 -> 1
| n -> fib(n-1) + fib(n-2);;

fib 4;;

//1.6 Declare a recursive function sum: int * int -> int, where sum(m, n) = m + (m + 1) + (m + 2) + ··· + (m + (n − 1)) + (m + n)
let rec sum = function
| (m, 0) -> m
| (m , n) -> m + n + sum(m, (n-1));;

//1.7 Determine a type for each of the expressions:
(*
    1. (System.Math.PI, fact -1)
        float * int
    2. fact(fact 4)
        (int -> int) -> int
    3. power(System.Math.PI, fact 2)
        float * int -> float
    4. (power, fact)
        (float * int -> float)*(int -> int)
*)

//1.8 Consider the declarations and Find the environment obtained from these declarations 
//      and write the evaluations of the expressions f 3 and g 3.
(*
    let a = 5;;
        env1 = [a->5]
    let f a = a + 1;;
        env2 = [a->5, f->n+1]
    let g b = (f b) + a;;
        env3 = [a->5, f->n+1, g->f(n)+a]

    f 3 = 4;;
    g 3 = 9;;
*)

