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
            
            
            let x = ast.Annotation<Mono.Cecil.MemberReference>()
            if (string ast = "Sample3") then
                ()
            if (string ast = "Sample3.x@3") then
                ()
            if (string ast = "FSharpList<int>.Empty") then
                ()
            if (string ast = "int[]") then
                ()
        
        
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

    and funDeclLayout (funDecl : FunctionDeclaration) =
        let nameLayout = defL funDecl.Name funDecl true
        let mutable args = " "
        let mutable parameters = ""
        if funDecl.externalParameters.Count = 0 then parameters <- "()" else
            for prmtr in funDecl.externalParameters do
                parameters <- (parameters + " " + (prmtr.Variables |> Seq.nth 0 |> fun x -> x.Name) )

        if funDecl.args.Count = 0 then args <- "_" else
            for arg in funDecl.args do
                args <- (arg.Name + " ")

        funDecl.body.AcceptVisitor (new InsertParenthesesVisitor())
        //let bodyLayout = funDecl.body |> string
        
        
        wordL "let" ++ nameLayout ++ (parameters |> wordL) ++ wordL " = fun" ++ wordL (args + " -> ") ++ (funDecl.body |> string |> wordL)
        //@@-- wordL bodyLayout
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
            | :? FunctionDeclaration as funDecl -> funDeclLayout funDecl
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