namespace DoubleFelix

module OrganizeTestsWithFuchu =
   open NUnit.Framework
   open Fuchu

   // good version of add1
   // let add1 x = x + 1

   // buggy version of add1, fails on multiples of 9
   let add1 x = if (x % 9 <> 0) then x + 1 else x    
   
   [<Test>]
   // a simple test using any assertion framework:
   // Fuchu's own, Nunit, FsUnit, etc
   let ``Assert that add1 is x+1`` x _notUsed = 
      NUnit.Framework.Assert.AreEqual(x+1, add1 x)

   // a single test case with one value
   let simpleTest = 
      testCase "Test with 42" <| 
        ``Assert that add1 is x+1`` 42

   // a parameterized test case with one param
   let parameterizedTest i = 
      testCase (sprintf "Test with %i" i) <| 
        ``Assert that add1 is x+1`` i

   // create a hierarchy of tests 
   // mark it as the start point with the "Tests" attribute
   [<Fuchu.Tests>]
   let tests = 
      testList "Test group A" [
         simpleTest 
         testList "Parameterized 1..10" ([1..10] |> List.map parameterizedTest) 
         testList "Parameterized 11..20" ([11..20] |> List.map parameterizedTest) 
   ]

module Rewriting =


   type Term = 
       | Plus of Term * Term
       | Times of Term * Term
       | Suc of Term 
       | Zero
       | VarA
       | VarB

   let rec depth (t:Term) = 
      match t with 
      | Suc b -> depth b + 1
      | Plus (a,b) -> depth a + depth b 
      | Times (a,b) -> depth a + depth b        
      | _ -> 1

   let rec Eval t = 
       match t with     
       //simplify 0
       | Plus (a, Zero) -> Eval a
       | Plus (Zero, a) -> Eval a
       | Times (Zero, a) -> 0
       | Times (a, Zero) -> 0
       //simplify 1
       | Times (Suc Zero, a) -> Eval a
       | Times (a, Suc Zero) -> Eval a  
       //eval to int
       | Zero -> 0
       | Suc x -> 1 + Eval x
       | Plus (a,b) -> Eval a + Eval b
       | Times (a,b) -> Eval a * Eval b
       | VarA -> 5
       | VarB -> 12


   type RulesList = (Term * Term) List

   let rnd = System.Random()

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

   let randomRule() = 
      let a = randomTerm()
      let b = randomTerm()
      if depth a > depth b then (a,b) else (b,a)

   let RuleCorrect (t1:Term, t2:Term) = 
      Eval t1 = Eval t2

   let correctEqualRules (rules : RulesList) = 
     (List.filter RuleCorrect rules)

   let rec removeUnity (rules : RulesList) =
      match rules with
      | [] -> []
      | (a,b) :: tail -> if a = b then removeUnity tail else (a,b) :: removeUnity tail


   let mutateLots x = 
      let y = seq { for i in 1 .. x -> ( randomRule()) }
      removeUnity (List.ofSeq y)

   let rec rewrite (rules : RulesList) (t:Term) = 
      match rules with
      | [] -> t
      | h :: tail -> match h with 
                     | (a, y) when a = t -> y
                     | (Suc a, Suc y) when a = t -> Suc (rewrite rules y)
                     | (Plus (a, b), Plus (y, z)) when a = t -> Plus (rewrite rules y,b)
                     | (Plus (b, a), Plus (z, y)) when a = t -> Plus (b,rewrite rules y)
                     | (Times (a, b), Times (y, z)) when a = t -> Times (rewrite rules y,b)
                     | (Times (b, a), Times (z, y)) when a = t -> Times (b,rewrite rules y)
                     | _ -> rewrite tail t

   let aFewGoodRules x = 
      let allRules = mutateLots x
      correctEqualRules allRules


   let rec rewriteRuleSet (rules : RulesList) (rules2 : RulesList) = 
      match rules with
      | [] -> []
      | (a,b) :: tail -> (a, rewrite rules2  b) :: rewriteRuleSet tail rules2

   let perfectRules x = 
      let allRules = mutateLots x
      let correctRules = correctEqualRules allRules
      let cleanRules = rewriteRuleSet correctRules correctRules
      let cleanRules = rewriteRuleSet correctRules cleanRules
      let cleanRules = rewriteRuleSet correctRules cleanRules
      let uniqueRules = List.ofSeq (Seq.distinct cleanRules)
      removeUnity uniqueRules

   [<EntryPoint>]
   let main argv = 
      let allRules = perfectRules 10000;

      


   

      //let b2 = OrganizeTestsWithFuchu.tests|> Fuchu.Tests.run


      0 // return an integer exit code





      