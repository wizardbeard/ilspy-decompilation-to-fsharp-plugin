module ILSpy.FSharp.DynamicCompilstionHelper

open FSharp.Compiler.CodeDom
open System.CodeDom.Compiler 
open System.IO

let private compile code assemblies outFile =
    use provider = new FSharpCodeProvider() 
    let compilerOptons = CompilerParameters(assemblies, outFile) 
    let res = provider.CompileAssemblyFromSource( compilerOptons, [|code|] ) 
    printfn "%A" res.Errors
    if res.Errors.Count = 0
    then Some(FileInfo(res.PathToAssembly))  
    else None

let private (++) v1 v2   = Path.Combine(v1, v2)
let private path= @"..\..\..\..\3rdParty\TestsCompilationEnv\"
let private defaultAsms  = [|path++"FSharp.Core.dll"|]
let private randomFile() = path ++ Path.GetRandomFileName() + ".dll"    

type System.CodeDom.Compiler.CodeCompiler with      
    static member CompileFSharpString (str, ?assemblies, ?output) = 
        let assemblies  = defaultArg assemblies defaultAsms 
        let output      = randomFile() |> defaultArg output
        printfn "%A %s" assemblies output  
        compile str assemblies output

let x () =
    let compiledDLLFile = 
        CodeCompiler.CompileFSharpString(
                "module Test\n"
                + "let x = 1"
                ,output = "Test.dll")
    printfn "OutPath: %A" compiledDLLFile

do x()
    