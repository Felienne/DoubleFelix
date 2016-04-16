namespace DoubleFelix

module Mutate = 

   open System

   let (|Middle|_|) prefix postfix (input:string) =
      // Check if the string starts with 'prefix', ends with 'postfix' and 
      // is longer than the two (meaning that it contains some middle part)
      if input.StartsWith(prefix) && input.EndsWith(postfix) && 
         input.Length >= (prefix.Length + postfix.Length) then 
         // Strip the prefix/postfix and return 'Some' to indicate success
         let len = input.Length - prefix.Length - postfix.Length
         Some(input.Substring(prefix.Length, len))
      else None // Return 'None' - string doesn't match the pattern

   let getOutputString (s) = 
      match s with
      | Middle "let syntheticFunction() = \"" "\"" mid -> mid
      | all -> all

//stolen from http://stackoverflow.com/questions/3753105/how-can-i-extract-the-middle-part-of-a-string-in-fsharp
   
   let levenshtein word1 word2 =
    let preprocess = fun (str : string) -> str.ToLower().ToCharArray()
    let chars1, chars2 = preprocess word1, preprocess word2
    let m, n = chars1.Length, chars2.Length
    let table : int[,] = Array2D.zeroCreate (m + 1) (n + 1)
    for i in 0..m do
        for j in 0..n do
            match i, j with
            | i, 0 -> table.[i, j] <- i
            | 0, j -> table.[i, j] <- j
            | _, _ ->
                let delete = table.[i-1, j] + 1
                let insert = table.[i, j-1] + 1
                //cost of substitution is 2
                let substitute = 
                    if chars1.[i - 1] = chars2.[j - 1] 
                        then table.[i-1, j-1] //same character
                        else table.[i-1, j-1] + 2
                table.[i, j] <- List.min [delete; insert; substitute]
    table.[m, n], table //return tuple of the table and distance
   
      
   let rnd = System.Random()
   let chars = "ABCDEFGHIJKLMNOPQRSTUVWUXYZ !"

   let swap(input:string) =           
      let randomSpot = rnd.Next(0, chars.Length) 
      let randomChar = chars.[randomSpot].ToString()

      let swapSpot = rnd.Next(0, input.Length) 

      let firstPart = input.Substring(0,swapSpot)
      let secondPart = input.Substring(swapSpot+1,(input.Length-swapSpot)-1)

      firstPart + randomChar + secondPart

   let add(input:string) =           
      let randomSpot = rnd.Next(0, chars.Length) 
      let randomChar = chars.[randomSpot].ToString()

      input + randomChar

   let delete(input:string) = 
      let swapSpot = rnd.Next(0, input.Length) 

      let firstPart = input.Substring(0,swapSpot)
      let secondPart = input.Substring(swapSpot+1,(input.Length-swapSpot)-1)

      firstPart + secondPart

   let mutate(input:string) =  
      let operators = [add; swap; delete]     
      let randomOp = operators.Item(rnd.Next(0, 2))        
      randomOp(input)
      
   let mutateFunction(input:string) = 
      let output = getOutputString input
      "let syntheticFunction() = \"" + mutate (output) + "\""


