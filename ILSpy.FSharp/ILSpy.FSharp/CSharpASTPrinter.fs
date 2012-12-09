namespace ILSpy.FSharp

open ICSharpCode.Decompiler
open ICSharpCode.NRefactory.CSharp

type CSharpASTPrinter() = class
    
    let rec pwit(ast: AstNode, output: ITextOutput, count: int) =
        do 
            match count with
            | i when i < 0 -> ()
            | 0 ->
                output.WriteLine("this:" + ast.ToString())
            | i -> 
                for j = 1 to i do
                        output.Write("child")
                output.WriteLine(": " + ast.ToString() + "    of   " + ast.GetType().ToString())
            for child in ast.Children do
                pwit(child, output, count + 1)

    let tab = "    "

    let rec past(ast: AstNode, output: ITextOutput, level: int) =
        do
            let mutable leveltab = ""
            for i = 1 to level do
                leveltab <- leveltab + tab
            match ast with
            | :? SyntaxTree ->
                for child in ast.Children do
                   past(child, output, level)
            | :? NamespaceDeclaration -> 
                output.Write("namespace ")
                for child in ast.Children do
                    if child :? Identifier then
                        let id = child :?> Identifier
                        output.Write(id.Name + "\n\n")
                    else
                        past(child, output, level)
            | :? Identifier ->
                let id = ast :?> Identifier
                output.Write(id.Name)
            | :? TypeDeclaration ->
                output.Write("type ")
                let typeDec = ast :?> TypeDeclaration
                for child in typeDec.Children do
                    if child :? Identifier then
                        let id = child :?> Identifier
                        output.WriteLine(id.Name + "() = \n")
                    else
                        past(child, output, level + 1)
            | :? UsingDeclaration ->
                //Как-нибудь в другой раз
                (*output.WriteLine("open " + ast.FirstChild.ToString())*)
                ()
            | :? PropertyDeclaration ->
                output.Write(leveltab + "member this.")
                for child in ast.Children do
                    if child :? Identifier then
                        let id = child :?> Identifier
                        output.Write(id.Name + "\n")
                    else
                        past(child, output, level + 1)
            | :? Accessor ->
                output.Write(leveltab + "with get() = ")
                for child in ast.Children do
                    past(child, output, level + 1)
            | :? BlockStatement ->
                for child in ast.Children do
                    past(child, output, level)
            | :? ReturnStatement ->
                past(ast.FirstChild, output, level)
            | :? PrimitiveExpression ->
                output.Write(ast.ToString())
            | _ -> ()


    
    member this.PrintWhatIsThere(ast: AstNode, output: ITextOutput) =
        pwit (ast, output, 0)

    
    member this.PrintAST(ast: AstNode, output: ITextOutput) =
        past(ast, output, 0)
                    
end
