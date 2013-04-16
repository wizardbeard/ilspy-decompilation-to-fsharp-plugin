namespace ILSpy.FSharp

open ICSharpCode.Decompiler
open ICSharpCode.NRefactory.CSharp
open ILSpy.FSharp.AST
open ILSpy.FSharp.PrinterWrapper

type FSharpASTPrinter() =
    
    [<Literal>]
    let moduleName = "FS DECOMPILER"

    let error msg = 
        wordL "(*" ++ wordL moduleName -+ wordL ". " ++ msg ++ wordL "*)"        

    let rec pwit (ast: AstNode) (output: ITextOutput) count =        
        match count with
        | k when k < 0 -> ()
        | 0 -> output.WriteLine(string ast.NodeType + ",   " + string ast)
        | k -> 
            for j = 1 to k do output.Write("  |")
            output.WriteLine()
            for j = 1 to k do output.Write("  |")
            output.WriteLine()
            for j = 1 to k do output.Write("   ")
            let name = 
                match ast.NodeType with
                | NodeType.Expression when (ast :? ThisReferenceExpression) ->" is thisRef"
                | NodeType.Expression when (ast :? MemberReferenceExpression) ->  " memRef"
                | NodeType.Expression when (ast :? NamedArgumentExpression) ->  " namedArg"
                | NodeType.Token when (ast :? Identifier) -> " ''" + (let x = (ast :?> Identifier) in string x.Name) + "''"
                | NodeType.Token when (ast :? CSharpModifierToken) -> " ''" + (let x = (ast :?> CSharpModifierToken) in string x.Modifier) + "''"
                | NodeType.Token -> ""
                | NodeType.Member when (ast :? FieldDeclaration) -> " ''" + (let x = (ast :?> FieldDeclaration) in string x.Variables.FirstOrNullObject) + "''"
                | NodeType.Member ->  " ''" + (let x = (ast :?> EntityDeclaration) in string x.Name) + "''"
                | _ -> "" 
            output.WriteLine("-- " + string ast.NodeType + name + ",   " + string ast)
        ast.Children |> Seq.iter (fun child -> pwit child output (count + 1))

    let rec namespaceLayout (nmsp : NamespaceDeclaration) =        
        let nameLayout = wordL nmsp.Name
        let bodyLayout = nmsp.Members |> List.ofSeq |> List.map past |> aboveListL 
        wordL "namespace" ++ nameLayout
        @@-- bodyLayout
        |> fold

    and typeDeclLayout (tDecl:TypeDeclaration) =        
        let nameLayout = defL tDecl.Name tDecl true
        let bodyLayout = tDecl.Members |> List.ofSeq |> List.map past |> aboveListL
        wordL "type" ++ nameLayout ++ wordL "() ="
        @@-- bodyLayout
        |> fold

    and anonymFunDeclLayout (funDecl : AnonymousFunctionDeclaration) =
        let nameLayout = defL funDecl.Name funDecl true
        let mutable args = ""
        if funDecl.args.Count = 0 then args <- "()" else
            for arg in funDecl.args do
                args <- (string arg.Name + " ")
        funDecl.body.AcceptVisitor (new InsertParenthesesVisitor())
        let bodyLayout = funDecl.body |> string
        let mutable parameters = ""
        if funDecl.externalParameters.Count = 0 then () else
            parameters <- "///External parameters:" + parameters
            for prmtr in funDecl.externalParameters do
                parameters <- parameters + " " + prmtr.GetText()
        wordL "let" ++ nameLayout ++ (args |> wordL) ++ wordL " =" ++ wordL parameters
        @@-- wordL bodyLayout
        |> fold

    and propDeclLayout (pDecl:PropertyDeclaration) =
        let nameLayout = wordL ("this." + pDecl.Name)
        let bodyLayout = past pDecl.Getter
        wordL "member" ++ nameLayout
        @@-- bodyLayout
        |> fold

    and accessorLayout (acs:Accessor) =        
        let bodyLayout = past acs.Body
        wordL "with get() ="
        @@-- bodyLayout
        |> fold

    and uDeclLayout (uDecl:UsingDeclaration) =        
        let nameLayout = past uDecl.Import
        wordL "open" ++ nameLayout

    and past (ast: AstNode) =
        let astChildrenCashed = ast.Children |> List.ofSeq
        match ast with
        | :? SyntaxTree -> astChildrenCashed |> List.map past |> aboveListL
        
        | :? Accessor as acs -> accessorLayout acs
        | :? BlockStatement -> astChildrenCashed |> List.map past |> aboveListL        
        | :? Identifier as id -> wordL id.Name
        | :? NamespaceDeclaration as nmsp -> namespaceLayout nmsp
        | :? PrimitiveExpression -> ast.ToString() |> wordL
        | :? PropertyDeclaration as pDecl -> propDeclLayout pDecl
        | :? ReturnStatement -> past ast.FirstChild        
        | :? TypeDeclaration as typeDecl -> 
            match typeDecl with
            | :? AnonymousFunctionDeclaration as funDecl -> anonymFunDeclLayout funDecl
            | _ ->  typeDeclLayout typeDecl
        | :? UsingDeclaration as uDecl -> uDeclLayout uDecl
        | x -> 
            wordL "Node is not supported:"
            @@-- (wordL "Type :"  ++  (x.NodeType |> string |> wordL)
                  --- wordL "Value:" ++ (x |> string |> wordL))
            |> error

    member public this.PrintWhatIsThere(ast: AstNode) (output: ITextOutput) =
        pwit ast output 0

    member public this.PrintAST (ast: AstNode) (output: ITextOutput) =        
        past ast
        |> print output