namespace ILSpy.FSharp

open ICSharpCode.Decompiler
open ICSharpCode.NRefactory.CSharp
open ILSpy.FSahrp.PrinterWrapper

type CSharpASTPrinter() =
    
    [<Literal>]
    let moduleName = "FS DECOMPILER"

    let error msg = 
        wordL "(*" ++ wordL moduleName -+ wordL ". " ++ msg ++ wordL "*)"        

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
        let nameLayout = defL tDecl.Name tDecl true
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
        wordL "open" ++ nameLayout

    and past (ast: AstNode) =
        let astChildrenCashed = ast.Children |> List.ofSeq
        match ast with
        | :? SyntaxTree -> astChildrenCashed |> List.map past |> aboveListL
        | :? NamespaceDeclaration as nmsp -> namespaceLayout nmsp
        | :? Identifier as id -> wordL id.Name
        | :? TypeDeclaration as typeDecl -> typeDeclLayout typeDecl
        | :? UsingDeclaration as uDecl -> uDeclLayout uDecl
        | :? PropertyDeclaration as pDecl -> propDeclLayout pDecl
        | :? Accessor as acs -> accessorLayout acs
        | :? BlockStatement -> astChildrenCashed |> List.map past |> aboveListL        
        | :? ReturnStatement -> past ast.FirstChild        
        | :? PrimitiveExpression -> ast.ToString() |> wordL
        | x -> 
            wordL "Node is not supported:"
            @@-- (wordL "Type :"  ++  (x.NodeType |> string |> wordL)
                  --- wordL "Value:" ++ (x |> string |> wordL))
            |> error

    member this.PrintWhatIsThere(ast: AstNode, output: ITextOutput) =
        pwit ast output 0

    member this.PrintAST(ast: AstNode, output: ITextOutput) =        
        past ast
        |> print output