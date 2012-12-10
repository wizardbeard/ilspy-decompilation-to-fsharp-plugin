namespace ILSpy.FSharp

open ICSharpCode.Decompiler
open ICSharpCode.NRefactory.CSharp
open Microsoft.FSharp.Text.StructuredFormat
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps

type CSharpASTPrinter() =
    
    [<Literal>]
    let moduleName = "FS DECOMPILER"

    let error msg = 
        "(*" + moduleName + ". " + msg + "*)"
        |> wordL

    let printLayout (writer: ITextOutput) layout  =
        if writer <> null then
            StructuredFormat.Display.layout_to_string {StructuredFormat.FormatOptions.Default with PrintWidth=100} layout
            |> writer.WriteLine            

    let rec pwit (ast: AstNode) (output: ITextOutput) count =
        match count with
        | i when i < 0 -> ()
        | 0 -> output.WriteLine("this:" + ast.ToString())
        | i -> 
            for j = 1 to i do output.Write("child")
            output.WriteLine(": " + ast.ToString() + "    of   " + ast.GetType().ToString())
        ast.Children |> Seq.iter (fun child -> pwit child output (count + 1))

    let rec namespaceLayout (nmsp : NamespaceDeclaration) =        
        let nameLayout = wordL nmsp.Name
        let bodyLayout = nmsp.Members |> List.ofSeq |> List.map past |> aboveListL
        wordL "namespace" ++ nameLayout
        @@-- bodyLayout

    and typeDeclLayout (tDecl:TypeDeclaration) =        
        let nameLayout = wordL tDecl.Name
        let bodyLayout = tDecl.Members |> List.ofSeq |> List.map past |> aboveListL
        wordL "type" ++ nameLayout ++ wordL "() ="
        @@-- bodyLayout

    and propDeclLayout (pDecl:PropertyDeclaration) =
        let nameLayout = wordL ("this." + pDecl.Name)
        let bodyLayout = past pDecl.Getter 
        wordL "member" ++ nameLayout
        @@-- bodyLayout

    and accessorLayout (acs:Accessor) =        
        let bodyLayout = past acs.Body
        wordL "with get() ="
        @@-- bodyLayout

    and uDeclLayout (uDecl:UsingDeclaration) =        
        let nameLayout = past uDecl.Import
        wordL "open" ++  nameLayout

    and past (ast: AstNode) =
        let astChildrenCashed = ast.Children |> List.ofSeq
        match ast with
        | :? SyntaxTree -> astChildrenCashed |> List.map (fun child -> past child) |> aboveListL
        | :? NamespaceDeclaration as nmsp -> namespaceLayout nmsp
        | :? Identifier as id -> wordL id.Name
        | :? TypeDeclaration as typeDecl -> typeDeclLayout typeDecl
        | :? UsingDeclaration as uDecl -> uDeclLayout uDecl
        | :? PropertyDeclaration as pDecl -> propDeclLayout pDecl
        | :? Accessor as acs -> accessorLayout acs
        | :? BlockStatement -> astChildrenCashed |> List.map (fun child -> past child) |> aboveListL 
        | :? ReturnStatement -> past ast.FirstChild
        | :? PrimitiveExpression -> ast.ToString() |> wordL
        | x -> "Node is not supported: " + string x |> error

    member this.PrintWhatIsThere(ast: AstNode, output: ITextOutput) =
        pwit ast output 0

    member this.PrintAST(ast: AstNode, output: ITextOutput) =
        past ast
        |> printLayout output

