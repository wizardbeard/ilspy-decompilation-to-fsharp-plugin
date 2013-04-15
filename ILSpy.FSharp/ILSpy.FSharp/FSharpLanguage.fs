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
open ICSharpCode.AvalonEdit.Highlighting;
open Mono.Cecil

open ILSpy.FSharp.Transforms

[<Class>]
[<Export(typeof<Language>)>]
type FSharpLanguage() as this =
    inherit Language()

    let hName = "FSharp"

    let highlighting = 
        ICSharpCode.AvalonEdit.Highlighting.Xshd.HighlightingLoader.Load
            (new Xml.XmlTextReader("FSharp-Mode.xshd")
            ,HighlightingManager.Instance)

    do HighlightingManager.Instance.RegisterHighlighting(hName,[|this.FileExtension|],highlighting)
    
    override this.Name
        with get() = "F#"
    
    override this.FileExtension
        with get() = ".fs"

    override this.ProjectFileExtension
        with get() = ".fsproj"

    override this.SyntaxHighlighting
        with get() = HighlightingManager.Instance.GetDefinition hName
    
    override this.DecompileMethod(methodDefinition:MethodDefinition, output: ITextOutput, options: DecompilationOptions) = 
        output.WriteLine "This is method"
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
            let b = new FSAstBuilder(c)
            b.AddMethod methodDefinition
            b.RunTransformations()
            output.WriteLine("Decompiled AST has " + b.SyntaxTree.DescendantsAndSelf.Count().ToString() + " nodes")
            
            output.WriteLine("Children " + b.SyntaxTree.Children.Count().ToString())
            let printer = new FSharpASTPrinter()
            printer.PrintWhatIsThere b.SyntaxTree output
            printer.PrintAST b.SyntaxTree output



            
    override this.DecompileProperty(property: PropertyDefinition, output: ITextOutput, options: DecompilationOptions) =
        output.WriteLine "This is property"
            
        let c = new DecompilerContext(property.Module)
        c.Settings <- options.DecompilerSettings
        c.CurrentType <- property.DeclaringType
        let b = new FSAstBuilder(c)
        b.AddProperty property
        b.RunTransformations()
        output.WriteLine("Decompiled AST has " + b.SyntaxTree.DescendantsAndSelf.Count().ToString() + " nodes")
            
        output.WriteLine("NodeType: " + b.SyntaxTree.NodeType.ToString())
        
        let printer = new FSharpASTPrinter()
        //printer.PrintWhatIsThere b.SyntaxTree output
        printer.PrintAST b.SyntaxTree output

    override this.DecompileField(field: FieldDefinition, output: ITextOutput, options: DecompilationOptions) =
        output.WriteLine "This is field"

    override this.DecompileEvent(ev: EventDefinition, output: ITextOutput, options: DecompilationOptions) =
        output.WriteLine "This is event"

    override this.DecompileType(typeDef: TypeDefinition, output: ITextOutput, options: DecompilationOptions) =
        //output.WriteLine "This is type"

        let c = new DecompilerContext(typeDef.Module)
        c.Settings <- options.DecompilerSettings
        c.CurrentType <- typeDef.DeclaringType
        let b = new FSAstBuilder(c)
        b.AddType typeDef
        b.RunTransformations()
        //output.WriteLine("Decompiled AST has " + b.SyntaxTree.DescendantsAndSelf.Count().ToString() + " nodes")

        let printer = new FSharpASTPrinter()
        printer.PrintWhatIsThere b.SyntaxTree output
        printer.PrintAST b.SyntaxTree output


    override this.DecompileAssembly(assembly: LoadedAssembly, output: ITextOutput, options: DecompilationOptions) =
        output.WriteLine "This is assembly"
        let Types = assembly.AssemblyDefinition.MainModule.Types
        for t in Types do
            output.Write("type:  ")
            t |> string |> output.WriteLine