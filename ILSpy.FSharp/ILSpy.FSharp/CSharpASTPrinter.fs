namespace ILSpy.FSharp

open ICSharpCode.Decompiler
open ICSharpCode.NRefactory.CSharp

type CSharpASTPrinter() =
    
    [<Literal>]
    let moduleName = "FS DECOMPILER"

    let mutable output = Unchecked.defaultof<ITextOutput>

    let wordL (txt:string) = output.Write txt 

    let def txt n local = output.WriteDefinition(txt,n,local)

    let aboveListL layoutF lst =
        lst |> List.iter (fun s -> layoutF s; output.WriteLine())        

    let tab layoutF = 
        output.Indent()
        layoutF()
        output.Unindent()

    let ws () = wordL " "

    let brake layoutF =
        output.WriteLine()
        layoutF()

    let error msg = 
        "(*" + moduleName + ". " + msg + "*)"
        |> wordL           

    let rec pwit (ast: AstNode) (output: ITextOutput) count =
        match count with
        | i when i < 0 -> ()
        | 0 -> output.WriteLine("this:" + ast.ToString())
        | i -> 
            for j = 1 to i do output.Write("child")
            output.WriteLine(": " + ast.ToString() + "    of   " + ast.GetType().ToString())
        ast.Children |> Seq.iter (fun child -> pwit child output (count + 1))

    let rec namespaceLayout (nmsp : NamespaceDeclaration) =        
        let nameLayout () = wordL nmsp.Name
        let bodyLayout () = nmsp.Members |> List.ofSeq |> aboveListL past
        wordL "namespace"
        ws()
        nameLayout ()
        brake (fun () -> tab bodyLayout)

    and typeDeclLayout (tDecl:TypeDeclaration) =        
        let nameLayout () = def tDecl.Name tDecl true
        let bodyLayout () = tDecl.Members |> List.ofSeq |> aboveListL past
        wordL "type"
        ws()
        nameLayout ()
        wordL "() ="
        brake (fun () -> tab bodyLayout)

    and propDeclLayout (pDecl:PropertyDeclaration) =
        let nameLayout () = wordL ("this." + pDecl.Name)
        let bodyLayout () = past pDecl.Getter
        wordL "member"
        ws()
        nameLayout ()
        brake (fun () -> tab bodyLayout)

    and accessorLayout (acs:Accessor) =        
        let bodyLayout () = past acs.Body
        wordL "with get() ="
        brake (fun () -> tab bodyLayout)

    and uDeclLayout (uDecl:UsingDeclaration) =        
        let nameLayout () = past uDecl.Import
        wordL "open" 
        ws()
        nameLayout ()

    and past (ast: AstNode) =
        let astChildrenCashed = ast.Children |> List.ofSeq
        match ast with
        | :? SyntaxTree -> astChildrenCashed |> aboveListL (fun child -> past child)
        | :? NamespaceDeclaration as nmsp -> namespaceLayout nmsp
        | :? Identifier as id -> wordL id.Name
        | :? TypeDeclaration as typeDecl -> typeDeclLayout typeDecl
        | :? UsingDeclaration as uDecl -> uDeclLayout uDecl
        | :? PropertyDeclaration as pDecl -> propDeclLayout pDecl
        | :? Accessor as acs -> accessorLayout acs
        | :? BlockStatement -> astChildrenCashed |> aboveListL (fun child -> past child)
        | :? ReturnStatement -> past ast.FirstChild
        | :? PrimitiveExpression -> ast.ToString() |> wordL
        | x -> "Node is not supported: " + string x |> error

    member this.PrintWhatIsThere(ast: AstNode, output: ITextOutput) =
        pwit ast output 0

    member this.PrintAST(ast: AstNode, _output: ITextOutput) =
        output <- _output
        past ast

