namespace ILSpy.FSharp

open ICSharpCode.Decompiler
open ICSharpCode.NRefactory.CSharp

type CSharpASTPrinter() =
    
    let rec pwit (ast: AstNode) (output: ITextOutput) count =
        do 
            match count with
            | i when i < 0 -> ()
            | 0 -> output.WriteLine("this:" + ast.ToString())
            | i -> 
                for j = 1 to i do output.Write("child")
                output.WriteLine(": " + ast.ToString() + "    of   " + ast.GetType().ToString())
            ast.Children |> Seq.iter (fun child -> pwit child output (count + 1))

    let tab = "    "

    let rec past (ast: AstNode) (output: ITextOutput) level  =        
        let leveltab = String.init level (fun _ -> tab)
        match ast with
        | :? SyntaxTree -> ast.Children |> Seq.iter (fun child -> past child output level)
        | :? NamespaceDeclaration -> 
            output.Write("namespace ")
            ast.Children 
            |> Seq.iter (function | :? Identifier as id -> output.Write(id.Name + "\n\n")
                                  | child -> past child output level)
        | :? Identifier as id -> output.Write id.Name
        | :? TypeDeclaration as typeDec ->
            output.Write("type ")
            typeDec.Children
            |> Seq.iter (function | :? Identifier as id -> output.WriteLine(id.Name + "() = \n")
                                  | child -> past child output (level + 1))
        | :? UsingDeclaration ->
            //Как-нибудь в другой раз
            (*output.WriteLine("open " + ast.FirstChild.ToString())*)
            ()
        | :? PropertyDeclaration ->
            output.Write(leveltab + "member this.")
            ast.Children
            |> Seq.iter (function | :? Identifier as id -> output.Write(id.Name + "\n")
                                  | child -> past child output (level + 1))
        | :? Accessor ->
            output.Write(leveltab + "with get() = ")
            ast.Children |> Seq.iter (fun child -> past child output (level + 1))
        | :? BlockStatement -> ast.Children |> Seq.iter (fun child -> past child output level)
        | :? ReturnStatement -> past ast.FirstChild output level
        | :? PrimitiveExpression -> output.Write(ast.ToString())
        | _ -> ()


    member this.PrintWhatIsThere(ast: AstNode, output: ITextOutput) =
        pwit ast output 0

    member this.PrintAST(ast: AstNode, output: ITextOutput) =
        past ast output 0
