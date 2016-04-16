namespace DoubleFelix

module EvalThatShit = 

   open System 
   open System.CodeDom.Compiler 
   open Microsoft.FSharp.Compiler.CodeDom 

   // Assembly path to keep compiled code
   let synthAssemblyPath = "synthetic.dll"

   let CompileFSharpCode(codeString, synthAssemblyPath) =
      use provider = new FSharpCodeProvider() 
      let options = CompilerParameters([||], synthAssemblyPath) 
      let result = provider.CompileAssemblyFromSource( options, [|codeString|] ) 
      // If we missed anything, let compiler show us what's the problem
      if result.Errors.Count <> 0 then  
         for i = 0 to result.Errors.Count - 1 do
               printfn "%A" (result.Errors.Item(i).ErrorText)
      result.Errors.Count = 0

   let Eval(codeString):string =      
      if CompileFSharpCode("module Synthetic.Code\n    "+codeString, synthAssemblyPath) then
         let synthAssembly = Reflection.Assembly.LoadFrom(synthAssemblyPath) 
         let synthMethod  = synthAssembly.GetType("Synthetic.Code").GetMethod("syntheticFunction") 
         synthMethod.Invoke(null, null).ToString()
      else
          "Whoops, no can do!"
   


