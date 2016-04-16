namespace DoubleFelix

module terms = 


module Rewriting =

   let rnd = System.Random()
   let valA = rnd.Next(0,100)
   let valB = rnd.Next(0,100)

   type Term = 
       | Plus of Term * Term
       | Times of Term * Term
       | Suc of Term 
       | Zero
       | VarA
       | VarB

   let rec print input = 
      match input with 
      | Plus (a,b) ->  print a + "+" + print b 
      | Times (a,b) -> print a + "*" + print b 
      | Suc a -> print a + "+1"
      | Zero -> "0"
      | VarA -> "a"
      | VarB -> "b"

   let rec Eval t = 
       match t with 
       //eval to int
       | Zero -> 0
       | Suc x -> 1 + Eval x
       | Plus (a,b) -> Eval a + Eval b
       | Times (a,b) -> Eval a * Eval b
       | VarA -> valA
       | VarB -> valB



   let rec randomTerm() = 
      let randomNumber = rnd.Next(0, 8)  
      match randomNumber with
      | 0 -> Zero
      | 1 -> Suc (randomTerm())
      | 2 -> Plus (randomTerm(), randomTerm())
      | 3 -> Times (randomTerm(), randomTerm())
      | 4 -> VarA
      | 5 -> VarB
      | _ -> Zero

   let rec depth (t:Term) = 
      match t with 
      | Suc b -> depth b + 1
      | Plus (a,b) -> depth a + depth b 
      | Times (a,b) -> depth a + depth b        
      | _ -> 1



   let rec rewriteManual t = 
      match t with
       //simplify 0
       | Plus (a, Zero) -> rewriteManual a
       | Plus (Zero, a) -> rewriteManual a
       | Times (Zero, a) -> Zero
       | Times (a, Zero) -> Zero
       //simplify 1
       | Times (Suc Zero, a) -> rewriteManual a
       | Times (a, Suc Zero) -> rewriteManual a  

   type Rule = Term * Term
   type RulesList = Rule List

   let randomRule() = 
      let a = randomTerm()
      let b = randomTerm()
      if depth a > depth b then (a,b) else (b,a)

   let RuleCorrect (t1:Term, t2:Term) = 
      Eval t1 = Eval t2

   let RuleUseful (t1:Term, t2:Term) = 
      t1 <> t2

   let mutateN x: RulesList = 
      List.ofSeq (seq {for i in 1 .. x -> randomRule()}) |>
      List.filter RuleUseful |> 
      List.filter RuleCorrect

   let rec applyoneRule (lhs : Term, rhs : Term)(input:Term) = 
      match input with 
      | a when a = lhs -> rhs
      | Plus (a,b) -> Plus (applyoneRule (lhs,rhs) a , applyoneRule (lhs,rhs) b)
      | Times (a,b) -> Times (applyoneRule (lhs,rhs) a , applyoneRule (lhs,rhs) b)
      | Suc a -> Suc (applyoneRule (lhs,rhs) a)
      | Zero -> Zero
      | VarA -> VarA
      | VarB -> VarB 

   let rec rewrite (rules : RulesList) (t:Term) = 
      match rules with
      | [] -> t
      | h :: tail -> rewrite tail (applyoneRule h t)

   let rec rewriteRuleSet (rules : RulesList) (rules2 : RulesList) = 
      match rules with
      | [] -> []
      | (a,b) :: tail -> (a, rewrite rules2 b) :: rewriteRuleSet tail rules2


   let perfectRules x = 
      let allRules = mutateN x
      let cleanRules = rewriteRuleSet allRules allRules //there should be a fixpoint here too, in case more rewriting is needed.
      let uniqueRules = List.ofSeq (Seq.distinct cleanRules)
      List.filter RuleUseful uniqueRules //we need again to filter out useles rules as we have rwwritten now

   [<EntryPoint>]
   let main argv = 
      for termTuple in perfectRules 500 do 
         match termTuple with
         | (a,b) -> printfn "%s " ((print a) + "->" + (print b))
                    

          

      let x = System.Console.ReadLine();
      0 // return an integer exit code





      