// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.IO
open System 

let rec repeat f n = fun x -> (if n = 0 then f x else (repeat f (n-1)) (f x))   

let cons x sigma = Seq.append (Seq.singleton x) sigma

(* Grabs the first item from a stream. *)
let first sigma = Seq.item 0 sigma

(* Removes the first item and gives you the rest of the stream. *)
let rest sigma = Seq.skip 1 sigma

let dropFirst(input) =
    match input with
        | x::xs -> xs
        | [] -> []

let readFile(filepath:string) =
    use sr = new StreamReader(filepath)
    let rec readLines() = 
        if not sr.EndOfStream then
            (cons ( sr.ReadLine() ) (readLines()) ) 
        else
            Seq.empty 
    readLines ()

let rec consList sq n = 
    if n = 0 || Seq.isEmpty sq then [] 
    else first sq :: consList (rest sq) (n-1) 

let stripchars chars str =
  Seq.fold
    (fun (str: string) chr ->
      str.Replace(chr |> Char.ToUpper |> string, "").Replace(chr |> Char.ToLower |> string, ""))
    str chars

let makeList (l1) =  
    List.map (fun x-> Seq.toList x) (List.map (fun (x:string) -> (stripchars (Seq.singleton '\"') x ).Split ',' ) l1)    
     
let tag l = 
    let rec tags n l1 acc =
        match l1 with   
            | x::xs -> tags (n+1) xs ((n,x)::acc)  
            | [] -> acc 
    tags 0 l []


let sortPos l = List.sortBy (fun (a,b) -> b) l  

let totalDiff l1 l2 =
    let rec calc L1 L2 acc =
        match L1 with
            | (a,b)::xs ->
                match L2 with
                    | (c,d)::ys-> calc xs ys ((c,((float)d-(float)b*1.0))::acc) 
                    | [] -> acc
            | [] -> acc
    calc l1 l2 [] 

let percentDiff l1 l2 =
    let rec calc L1 L2 acc = 
        match L1 with 
            | (a,b)::xs -> 
                match L2 with 
                    | (c,d)::ys -> calc xs ys ((c,((float)d/(float)b*100.0))::acc) 
                    | [] -> acc 
            | [] -> acc
    calc l1 l2 []

let periodDiff l =
    match l with
        | x::xs -> fun f n -> f x (List.item n l)
        | [] -> fun f n -> [] 


let mean ls = 
    let rec compute l t n = 
        match l with
            | (a,b)::xs -> compute xs (b+t) (n+1)
            | [] -> t/n 
    compute ls 0 0
    
  
let allDiffs f l = 
    let rec calculateAll x l1 acc = 
        match l1 with
            | y::[] -> (f x y) :: acc
            | y::ys -> calculateAll y ys ((f x y) :: acc) 
            | [] -> acc
    match l with
        | z::zs -> calculateAll z zs []  
        | [] -> []

let totalApr l =
    let comparator (a,b) x =
        if a = x then true
        else false
    let rec def l4 accu =
        match l4 with
            | x::xs -> 
                match x with 
                    | y::ys -> def xs (ys::accu)
                    | [] -> def xs accu
            | [] -> accu
    let rec find f (c,d) l2 =
            match l2 with
                | (a,b)::xs ->
                    if f (c,d) a then (a,b+1)::xs
                    else (a,b)::(find f (c,d) xs)
                | [] -> [(c,1)]
    let rec helper l1 acc =
       match l1 with
        | [] -> acc
        | x::xs -> helper (def l1 []) ((List.fold (fun l3 x -> match x with |(a,b)::xs -> (find (comparator) (a,b) l3) | [] -> []) [] l1)::acc) 
    helper l [] 

let file = "C:\Users\Alex\Desktop\Licensing\Population_Estimates.csv"
let lst = readFile file
let lst1 = consList lst 10
let lst2 = dropFirst lst1  
let lst3 = makeList lst2 
let lst4 = List.map (repeat dropFirst 3) lst3
let lst5 = List.map (fun x -> List.map (fun y -> int y) x ) lst4 
let lst6 = List.map (fun x -> tag x) lst5    
let lst7 = allDiffs (percentDiff) lst6  
let lst8 = allDiffs (totalDiff) lst6
let lst9 = List.map (fun x -> sortPos x) lst7
let lst10 = List.map (fun x -> sortPos x) lst8
let lst11 = List.map (fun x -> sortPos x) lst6
let lst12 = totalApr lst10 
let lst13 = totalApr lst11

let o = [[1];[2];[3];[4];[5];[6]]
