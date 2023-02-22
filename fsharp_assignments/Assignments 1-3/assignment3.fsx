// bin search from class
(* type 'a bintree =
    | Leaf 
    | Node of 'a bintree * 'a * 'a bintree

let rec insert (x : 'a) (bt : bintree) =
    match x with
    | Leaf                          -> Node (Leaf, x , Leaf)
    | Node(l, y , r)   when x <= y  -> Node (insert x l, y, r )
    | Node(l, y , r)                -> Node (l , y, insert x r )

let rec fromList lst =
    match lst with
    | []    -> Leaf
    | x :: xs -> insert x (fromList xs);; *)




type aExp =
| N of int // Integer value
| V of string // Variable
| WL // Length of the word
| PV of aExp // Point value of character at specific word index
| Add of aExp * aExp // Addition
| Sub of aExp * aExp // Subtraction
| Mul of aExp * aExp // Multiplication
;;

let (.+.) a b = Add (a, b);;
let (.-.) a b = Sub (a, b);;
let (.*.) a b = Mul (a, b);;

let rec evalA a env =
    match a with
    | N n           ->  n
    | V v           -> Map.find v env
    | Add   (a, b)  ->  (evalA a env) + (evalA b env)
    | Sub   (a, b)  ->  (evalA a env) - (evalA b env)
    | Mul   (a, b)  ->  (evalA a env) * (evalA b env);;


//examples
let a1 = N 42;;
let a2 = N 4 .+. (N 5 .-. N 6);;
let a3 = N 4 .*. N 2 .+. N 34;;
let a4 = (N 4 .+. N 2) .*. N 34;;
let a5 = N 4 .+. (N 2 .*. N 34);;




(* E 3.1 : Create a function arithEvalSimple : aExp -> int that given an arithmetic expression a calculates its
integer value *)
let rec arithEvalSimple (e : aExp) =
    match e with
    | N     a       ->  a
    | Add   (a, b)  ->  (arithEvalSimple a) + (arithEvalSimple b)
    | Sub   (a, b)  ->  (arithEvalSimple a) - (arithEvalSimple b)
    | Mul   (a, b)  ->  (arithEvalSimple a) * (arithEvalSimple b);;

(* E 3.2 : Create a function arithEvalState : aExp -> Map<string, int> -> int that given an arithmetic
expression a and a state s returns the integer that a evaluates to where variable values are retrieved
from s . If a variable does not exist in the state use 0 for its value*)
let a6 = V "x";;
let a7 = N 4 .+. (V "y" .-. V "z");;

let rec arithEvalState (a : aExp) (env : Map<string, int>) : int =
    match a with
    | V v -> 
        let b = (Map.tryFind v env)
        match b with
            | Some s -> s
            | None -> 0
    | N n           ->  n
    | Add   (a, b)  ->  (arithEvalState a env) + (arithEvalState b env)
    | Sub   (a, b)  ->  (arithEvalState a env) - (arithEvalState b env)
    | Mul   (a, b)  ->  (arithEvalState a env) * (arithEvalState b env);;





type word = (char * int) list;;

let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_");;
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_");;

let arithDoubleWordScore = N 2 .*. V "_acc_";;
let arithTripleWordScore = N 3 .*. V "_acc_";;

(* E 3.3 : Create a function arithEval : aExp -> word -> Map<string, int> -> int that given an arithmetic
expression a , a word w , and a state s , evaluates a with respect to w and s .*)
let rec arithEval e w env =    
    match e with
    | N n           ->  n
    | V v -> 
        let b = Map.tryFind v env
        match b with
            | Some s -> s
            | None -> 0
    | Add   (a, b)  ->  (arithEval a w env) + (arithEval b w env)
    | Sub   (a, b)  ->  (arithEval a w env) - (arithEval b w env)
    | Mul   (a, b)  ->  (arithEval a w env) * (arithEval b w env)
    | WL            -> List.length w
    | PV p          -> snd (w.[(arithEval p w env)]);;

let hello : word =
    ('H', 4) :: ('E', 1) :: ('L', 1) :: ('L', 1) :: ('O', 1) :: [];;


(* E 3.4 : create a function charEval : cExp -> word -> Map<string, int> -> char 
that given an character expression c , a word w , and a state s , evaluates c
with respect to w and s *)

type cExp =
| C of char (* Character value *)
| ToUpper of cExp (* Converts lower case to upper case character,
non-letters are unchanged *)
| ToLower of cExp (* Converts upper case to lower case character,
non-letters are unchanged *)
| CV of aExp (* Character lookup at word index *)
;;

let rec charEval (e : cExp) (w : word) env =
    match e with
    | C         ch           ->  ch
    | ToUpper   c           -> System.Char.ToUpper (charEval c w env)
    | ToLower   c           -> System.Char.ToLower (charEval c w env)
    | CV        a           -> 
        let i = arithEval a w env
        fst (w.[i]);;

(* E 3.5 : create a function boolEval : bExp -> word ->
Map<string, int> -> bool that given an boolean expression b , a word w , and a state s , evaluates b
with respect to w and s .*)

type bExp = 
| TT                    (* true *)
| FF                    (* false *)
| AEq of aExp * aExp    (* numeric equality *)
| ALt of aExp * aExp    (* numeric less than *)
| Not of bExp           (* boolean not *)
| Conj of bExp * bExp   (* boolean conjunction *)
| IsDigit of cExp       (* check for digit *)
| IsLetter of cExp      (* check for letter *)
| IsVowel of cExp   ;;    (* check for vowel *)

let (~~) b = Not b;;
let (.&&.) b1 b2 = Conj (b1, b2);;
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2) ;;          (* boolean disjunction *)
 
let (.=.)   a b = AEq (a, b) ;;
let (.<.)   a b = ALt (a, b) ;;
let (.<>.)  a b = ~~(a .=. b)                   ;;(* numeric inequality *)
let (.<=.)  a b = a .<. b .||. ~~(a .<>. b)   ;;  (* numeric less than or equal to *)
let (.>=.)  a b = ~~(a .<. b)                   ;;(* numeric greater than or equal to *)
let (.>.)   a b = ~~(a .=. b) .&&. (a .>=. b)  ;; (* numeric greater than *)

let rec boolEval (e : bExp) w env =
    match e with
    | TT            -> true
    | FF            -> false
    | AEq (a, b)    -> 
        let a1 = arithEval a w env
        let b1 = arithEval b w env
        if (a1 = b1) then true else false
    | ALt (a, b)    -> 
        let a1 = arithEval a w env
        let b1 = arithEval b w env
        if (a1 < b1) then true else false
    | Not a         ->
        let b = boolEval a w env
        not b
    | Conj (a, b)   -> 
        let a1 = boolEval a w env
        let b1 = boolEval b w env
        a1 && b1
    | IsDigit c     -> 
        System.Char.IsDigit (charEval c w env)
    | IsLetter c     -> 
        System.Char.IsLetter (charEval c w env)
    | IsVowel c     -> 
        let c1 = charEval c w env
        let c2 = System.Char.ToLower c1
        match c2 with
        | 'a' | 'e' | 'i' | 'o' | 'u' | 'y' -> true
        | _ -> false
    ;;

(* E 3.6 : Create a function isConsonant : cExp -> bExp that given a character expression c returns a bExp s*)
let isConsonant (e : cExp) : bExp =
    Not (IsVowel e);;


type stmnt =
| Skip                          (* does nothing *)
| Ass   of string * aExp        (* variable assignment *)
| Seq   of stmnt * stmnt        (* sequential composition *)
| ITE   of bExp * stmnt * stmnt (* if-then-else statement *) 
| While of bExp * stmnt ;;        (* while statement *)

(* E 3.7 : Create a function evalStmnt : stmnt -> word -> Map<string, int> -> Map<string, int> *)
let rec evalStmnt (s : stmnt) (w : word) env =
    match s with
    | Skip              -> env
    | Ass (str, a)      -> 
        let i = arithEval a w env
        env.Add(str, i)
    | Seq (s1, s2)      -> 
        let newEnv = (evalStmnt s1 w env)
        (evalStmnt s2 w newEnv)
    | ITE (b, s1, s2)   ->
        if (boolEval b w env) then (evalStmnt s1 w env) else (evalStmnt s2 w env)
    | While (b, st)     ->
        if (boolEval b w env)
        then (evalStmnt st w env) |> evalStmnt (While (b, st)) w 
        else env
    ;;

(* E 3.8 : Create a function stmntToSquareFun : stmnt -> squareFun that given a statemnt stm returns a
function that given a word w , a position pos , and an accumultor acc evaluates stm with respect to w and
the inital state map [("_pos_", pos); ("_acc_", acc)] and returns the value of the variable
"_result_" after stm has been evaluated*)
type squareFun = word -> int -> int -> int;;
// word * position * acc

let stmntToSquareFun (s : stmnt) : squareFun =
    (fun w pos acc -> (evalStmnt s w (Map.ofList [("_pos_", pos); ("_acc_", acc)])).["_result_"]  );;

let singleLetterScore = stmntToSquareFun (Ass ("_result_", arithSingleLetterScore));;
let doubleLetterScore = stmntToSquareFun (Ass ("_result_", arithDoubleLetterScore));;
let tripleLetterScore = stmntToSquareFun (Ass ("_result_", arithTripleLetterScore));;

let doubleWordScore = stmntToSquareFun (Ass ("_result_", arithDoubleWordScore));;
let tripleWordScore = stmntToSquareFun (Ass ("_result_", arithTripleWordScore));;

let containsNumbers =
    stmntToSquareFun
        (Seq (Ass ("_result_", V "_acc_"),
            While (V "i" .<. WL,
                ITE (IsDigit (CV (V "i")),
                    Seq (
                        Ass ("_result_", V "_result_" .*. N -1),
                        Ass ("i", WL)),
                    Ass ("i", V "i" .+. N 1)))));;

(* E 3.9 : Create a statement oddConsonants such that the function generated from stmntToSquareFun*)
let oddConsonants = 
    Seq (
            Ass ("_result_", V "_acc_"), 
            While (
                V "i" .<. WL, 
                ITE (
                    Not (IsVowel (CV (V "i"))), 
                    Seq (
                        Ass ("_result_", V "_result_" .*. N -1), 
                        Ass ("i", V "i" .+. N 1)
                    ),
                    Ass ("i", V "i" .+. N 1)
                )
            )
        );;

(* E 3.10 : Create a function calculatePoints2 : square2 list -> word -> int that behaves exactly like
calculatePoints from 2.14 except that this one operates on square2 rather than square types*)
type square = (int * squareFun) list;;
type square2 = (int * stmnt) list;;

let SLS : square2 = [(0, Ass ("_result_", arithSingleLetterScore))];;
let DLS : square2 = [(0, Ass ("_result_", arithDoubleLetterScore))];;
let TLS : square2 = [(0, Ass ("_result_", arithTripleLetterScore))];;

let DWS : square2 = [(1, Ass ("_result_", arithDoubleWordScore))] @ SLS;;
let TWS : square2 = [(1, Ass ("_result_", arithTripleWordScore))] @ SLS;;

let calculatePoints (squares : square list) (word : word) : int =
    //((int * (int -> int)) list) list  <=>     (prioritet * (word -> pos) list) list
    let priWordPoslst           = squares               |> List.mapi    (fun i s1 ->  (List.map (fun s -> (fst s), ((snd s) word i)))  s1) 
    let flattenPriWordPoslst    = priWordPoslst         |> List.fold    (@) []
    let sortFlatPriWordPoslst   = flattenPriWordPoslst  |> List.sortBy  (fun ls -> fst ls)
    let wordList                = sortFlatPriWordPoslst |> List.map     (fun xs -> snd xs)
    let fcmsl = wordList     |> List.fold    (>>) id
    fcmsl 0;;


let calculatePoints2 (square2s : square2 list) (w : word) =
    let squares = square2s |> List.map (fun s -> List.map (fun (pri, stmt) -> (pri, stmntToSquareFun stmt)) s)
    calculatePoints squares w;;
   