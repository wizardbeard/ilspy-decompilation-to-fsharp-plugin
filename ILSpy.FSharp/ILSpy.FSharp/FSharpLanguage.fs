namespace ILSpy.FSharp

open System
open System.Windows
open System.ComponentModel.Composition
open System.Linq
open System.Windows.Controls
open ICSharpCode.Decompiler
open ICSharpCode.Decompiler.Ast
open ICSharpCode.ILSpy
open ICSharpCode.NRefactory.CSharp
open Mono.Cecil

[<Class>]
[<Export(typeof<Language>)>]
type FSharpLanguage() =
    
    inherit Language()
    
    override this.Name
        with get() = "F#"
    
    override this.FileExtension
        with get() = ".fs"
    
    override this.DecompileMethod(methodDefinition:MethodDefinition, output: ITextOutput, options: DecompilationOptions) = 
        if methodDefinition.Body <> null then
            output.WriteLine ("Size of method: " + methodDefinition.Body.CodeSize.ToString() + " bytes")
            
            let smartOutput = output :?> ISmartTextOutput

            if smartOutput <> null
            then
                smartOutput.AddButton(null, "Click me!", new RoutedEventHandler(fun sender e -> (sender :?> Button).Content <- "I was clicked!"))
                smartOutput.WriteLine()
            
            let c = new DecompilerContext(methodDefinition.Module)
            c.Settings <- options.DecompilerSettings
            c.CurrentType <- methodDefinition.DeclaringType
            let b = new AstBuilder(c)
            b.AddMethod methodDefinition
            b.RunTransformations()
            output.WriteLine("Decompiled AST has " + b.SyntaxTree.DescendantsAndSelf.Count().ToString() + " nodes")