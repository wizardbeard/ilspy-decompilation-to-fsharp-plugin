module public ILSpy.FSharp.AST


///open System
///open System.Collections

open ICSharpCode.Decompiler
open ICSharpCode.Decompiler.Ast
open ICSharpCode.NRefactory.CSharp
open ICSharpCode.NRefactory


let  AddChild (parent: AstNode) (child : AstNode) =
    let clone = child.Clone()
    match clone with
    | :? AttributeSection as section -> parent.AddChild<AttributeSection>(section, child.Role :?> ICSharpCode.NRefactory.Role<AttributeSection>)
    | :? AstType as astType -> parent.AddChild<AstType>(astType, child.Role :?> ICSharpCode.NRefactory.Role<AstType>)
    | :? CSharpModifierToken as token -> parent.AddChild<CSharpModifierToken>(token, child.Role :?> ICSharpCode.NRefactory.Role<CSharpModifierToken>)
    | :? Identifier as id -> parent.AddChild<Identifier>(id, child.Role :?> ICSharpCode.NRefactory.Role<Identifier>)
    | _ -> (child :?> AstType) |> ignore

type private deleteThisRefVisitor() =
    inherit DepthFirstAstVisitor()
    
    override this.VisitMemberReferenceExpression(memRef) =
        memRef.FirstChild.Remove()
        memRef.ReplaceWith (new IdentifierExpression((memRef.FirstChild :?> Identifier).Name))
    
type public AnonymousFunctionDeclaration (args : list<ParameterDeclaration>, body : Expression, externalParameters: list<FieldDeclaration>) =
        
    inherit TypeDeclaration()
        
    member this.args = args
    member this.body = body 
    member this.externalParameters = externalParameters
    
    default this.NodeType
        with get() = NodeType.TypeDeclaration

    override this.AcceptVisitor (visitor : IAstVisitor)=
        base.AcceptVisitor visitor ///dummy

    override this.AcceptVisitor<'T> (visitor : IAstVisitor<'T>) =
        Unchecked.defaultof<'T> ///dummy

    override this.AcceptVisitor<'T, 'S> (visitor : IAstVisitor<'T, 'S>, data : 'T) =
        Unchecked.defaultof<'S>

    override this.DoMatch (other: AstNode, match' : ICSharpCode.NRefactory.PatternMatching.Match) =
        true //dummy

    static member GetFromTypeDecl(typeDecl: TypeDeclaration) =
        let mutable args = []
        let mutable externalParameters = []
        let mutable body = null
        let mutable children = []
            
        for child in (typeDecl.Children |> List.ofSeq) do
            match child.NodeType with
            | NodeType.Member ->
                match child with
                | :? FieldDeclaration as FD ->
                    externalParameters <- FD :: externalParameters
                | :? ConstructorDeclaration -> ()
                | :? MethodDeclaration as MD -> ///invoke
                    for ch in (MD.Children |> List.ofSeq) do
                        match ch with
                        | :? ParameterDeclaration as PD -> 
                            args <- PD :: args
                        | :? Statement as statement ->  // body
                            body <- (statement.FirstChild.FirstChild :?> Expression)
                            body.AcceptVisitor(new deleteThisRefVisitor())
                        | _ -> ()
                | _ -> children <- child :: children
            | _ -> children <- child :: children 

        let func = new AnonymousFunctionDeclaration(args, body, externalParameters)
        for child in children do
            AddChild func child
        func

(*module public Visitors
    
type public FSAstVisitor =
    inherit IAstTransform
    abstract member this.VisitFunction*)