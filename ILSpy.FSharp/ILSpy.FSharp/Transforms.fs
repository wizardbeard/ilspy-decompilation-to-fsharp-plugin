module public ILSpy.FSharp.Transforms


open System

open ICSharpCode.Decompiler
open ICSharpCode.Decompiler.Ast
open ICSharpCode.Decompiler.Ast.Transforms
open ICSharpCode.NRefactory.CSharp
open ICSharpCode.NRefactory
open AST
    
type public IntroduceFunctions() =
    inherit DepthFirstAstVisitor<obj, obj>()
    interface IAstTransform with
        member this.Run(node : AstNode) =
            let visitor = this :> IAstVisitor<_,_>
            ignore (node.AcceptVisitor<_,_> (this, null))      
        
    override this.VisitTypeDeclaration (typeDeclaration, data) =               
        let itsFunc =
            typeDeclaration.Children 
            |> Seq.exists (fun child -> child.NodeType = NodeType.TypeReference && (string child).StartsWith "FSharpFunc")
        if itsFunc then typeDeclaration.ReplaceWith(FunctionDeclaration.GetFromTypeDecl(typeDeclaration) :> AstNode)
        base.VisitTypeDeclaration (typeDeclaration, data)


type public IntroduceFSListExpressions() =
    inherit DepthFirstAstVisitor<obj, obj>()
    interface IAstTransform with
        member this.Run(node : AstNode) =
            let visitor = this :> IAstVisitor<_,_>
            ignore (node.AcceptVisitor<_,_> (this, null))
    
    override this.VisitInvocationExpression(invocationExpression, data) =
        if (string invocationExpression.FirstChild).StartsWith "FSharpList" && (string invocationExpression.FirstChild).EndsWith ".Cons" then
            let rec x = fun (expr : AstNode) ->
                match expr with
                | :? InvocationExpression -> //FSharpList<>.Cons(...)
                    (expr.Children |> Seq.nth 1)::(x (expr.Children |> Seq.nth 2))
                | :? MemberReferenceExpression -> //FsharpList<>.Empty
                    []
                | _ -> [] //dummy
            let y = new FSListExpression(x invocationExpression)
            invocationExpression.ReplaceWith(y)
            base.VisitInvocationExpression(y, data) //dummy
        else
            base.VisitInvocationExpression(invocationExpression, data)

    override this.VisitMemberReferenceExpression(memberReferenceExpression, data) =
        if (string memberReferenceExpression).StartsWith "FSharpList" && (string memberReferenceExpression).EndsWith ".Empty" then 
            memberReferenceExpression.ReplaceWith(AST.FSListExpression.Empty)
        base.VisitMemberReferenceExpression(memberReferenceExpression, data)
        

module public FSTransformationPipeline =
    
    let CreatePipeline(context : DecompilerContext) : IAstTransform[] =
        [| new PushNegation();
            new DelegateConstruction(context);
            new ReplaceMethodCallsWithOperators(context);
            new IntroduceUnsafeModifier();
            new AddCheckedBlocks();
            new IntroduceFunctions();
            new DeclareVariables(context);
            new ConvertConstructorCallIntoInitializer();
            new DecimalConstantTransform();
            new IntroduceFSListExpressions()|]

    let RunTransformationsUntil (node : AstNode) (abortCondition : Predicate<IAstTransform>) (context : DecompilerContext) =        
        if node <> null then 
            CreatePipeline context |> Seq.iter (fun transform -> 
            context.CancellationToken.ThrowIfCancellationRequested()
            if not (abortCondition <> null && abortCondition.Invoke transform) then transform.Run node)

type public FSAstBuilder(context: DecompilerContext) =
    inherit AstBuilder(context)
    
    member public this.RunTransformations (transformAbortCondition : Predicate<IAstTransform>) =
        FSTransformationPipeline.RunTransformationsUntil this.SyntaxTree transformAbortCondition context
        //this.tra dummy ///transformationsHaveRun = true;
    
    member public this.RunTransformations() =
       
       this.RunTransformations null