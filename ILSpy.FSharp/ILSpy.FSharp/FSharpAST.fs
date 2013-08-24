module public ILSpy.FSharp.AST


///open System
///open System.Collections

open ICSharpCode.Decompiler
open ICSharpCode.Decompiler.Ast
open ICSharpCode.NRefactory.CSharp
open ICSharpCode.NRefactory
open System.Diagnostics
open System.Collections.Generic

(*[<AbstractClassAttribute>]
[<AllowNullLiteralAttribute>]
type public AstNode() =
    
    (*abstract parent : AstNode
    abstract prevSibling : AstNode
    abstract nextSibling : AstNode
    abstract firstChild : AstNode*)
    //abstract lastChild : AstNode
    
    member val firstChild : AstNode = null
        with get, set  
    member val parent : AstNode = null
        with get, set 
    member val prevSibling : AstNode = null
        with get, set 
    member val nextSibling : AstNode = null
        with get, set 

    member public this.Children
        with get() : IEnumerable<AstNode> =
            let rec x (firstChild : AstNode) = 
                seq{
                    let y = firstChild = this
                    Debug.Assert(y)
                    yield firstChild; yield! x firstChild.nextSibling}
            x this.firstChild


type public SyntaxTree() = 
    inherit AstNode()

[<AbstractClassAttribute>]
type public EntityDeclaration() =
    inherit AstNode()
    member val name = "" with get, set

type public ModuleDeclaration() =
    inherit EntityDeclaration()

    

type public ClassDeclaration() =
    inherit EntityDeclaration()

type public PropertyDeclaration() =
    inherit EntityDeclaration()

*)
let  AddChild (parent: AstNode) (child : AstNode) =
    let clone = child.Clone()
    match clone with
    | :? AttributeSection as x    -> parent.AddChild<AttributeSection>( x, child.Role :?> _ )
    | :? AstType as x             -> parent.AddChild<AstType>( x, child.Role :?> _ )
    | :? CSharpModifierToken as x -> parent.AddChild<CSharpModifierToken>( x, child.Role :?> _ )
    | :? Identifier as x          -> parent.AddChild<Identifier>( x, child.Role :?> _ )
    | _ -> ()

type deleteThisRefVisitor() =
    inherit DepthFirstAstVisitor()
    
    override this.VisitMemberReferenceExpression(memRef) =
        memRef.FirstChild.Remove()
        memRef.ReplaceWith (new IdentifierExpression((memRef.FirstChild :?> Identifier).Name))
    
type public FunctionDeclaration (args, body : Expression, externalParameters) =
        
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
        let args = new ResizeArray<_>()
        let externalParameters = new ResizeArray<_>()
        let body = ref null
        let children = new ResizeArray<_>()
            
        for child in (typeDecl.Children |> List.ofSeq) do
            match child.NodeType with
            | NodeType.Member ->
                match child with
                | :? FieldDeclaration as FD -> externalParameters.Add FD
                | :? ConstructorDeclaration -> ()
                | :? MethodDeclaration as MD -> ///invoke
                    MD.Children 
                    |> Seq.iter
                        (function
                            | :? ParameterDeclaration as PD -> args.Add PD
                            | :? Statement as statement ->  // body
                                body := statement.FirstChild.FirstChild :?> Expression
                                (!body).AcceptVisitor(new deleteThisRefVisitor())
                            | _ -> ())
                | _ -> children.Add child
            | _ -> children.Add child

        let func = new FunctionDeclaration(args, !body, externalParameters)
        children |> Seq.iter (fun child -> AddChild func child)
        func

type public FSListExpression(fsList : AstNode list) =
    inherit Expression()

    member this.body = fsList
    static member Empty = 
        FSListExpression([])

    override this.AcceptVisitor (visitor : IAstVisitor)=
        this.body |> Seq.iter (fun x -> x.AcceptVisitor(visitor))
        //base.AcceptVisitor visitor ///dummy
        //()

    override this.AcceptVisitor<'T> (visitor : IAstVisitor<'T>) =
        Unchecked.defaultof<'T> ///dummy

    override this.AcceptVisitor<'T, 'S> (visitor : IAstVisitor<'T, 'S>, data : 'T) =
        Unchecked.defaultof<'S>

    override this.DoMatch (other: AstNode, match' : ICSharpCode.NRefactory.PatternMatching.Match) =
        true //dummy

    override this.ToString() = string this.body
    
    default this.NodeType
        with get() = NodeType.Expression
 
 type public AnonymousFunction() =
    inherit Expression()

    member public this.parameters
        with get() = this.GetChildrenByRole(Roles.Parameter)
    member public this.body
        with get() = this.GetChildByRole(Roles.Expression)

    override this.AcceptVisitor (visitor : IAstVisitor)=
        this.body.AcceptVisitor visitor ///dummy

    override this.AcceptVisitor<'T> (visitor : IAstVisitor<'T>) =
        Unchecked.defaultof<'T> ///dummy

    override this.AcceptVisitor<'T, 'S> (visitor : IAstVisitor<'T, 'S>, data : 'T) =
        Unchecked.defaultof<'S>

    override this.DoMatch (other: AstNode, match' : ICSharpCode.NRefactory.PatternMatching.Match) =
        true //dummy
    
    default this.NodeType
        with get() = NodeType.Unknown

    override this.ToString() =
        (this.parameters |> Seq.map (fun x -> " " + x.Name) |> (Seq.fold (+)) "fun") + " -> " + string this.body
        

(*module public Visitors
    
type public FSAstVisitor =
    inherit IAstTransform
    abstract member this.VisitFunction*)