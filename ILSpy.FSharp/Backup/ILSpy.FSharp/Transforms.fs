module public ILSpy.FSharp.Transforms


open System

open ICSharpCode.Decompiler
open ICSharpCode.Decompiler.Ast
open ICSharpCode.Decompiler.Ast.Transforms
open ICSharpCode.NRefactory.CSharp
open ICSharpCode.NRefactory
open AST
open Mono.Cecil
    
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
    inherit DepthFirstAstVisitor()
    interface IAstTransform with
        member this.Run(node : AstNode) =
            let visitor = this :> IAstVisitor
            ignore (node.AcceptVisitor(visitor))
    
    override this.VisitInvocationExpression(invocationExpression) =
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
        else
            base.VisitInvocationExpression(invocationExpression)

    override this.VisitMemberReferenceExpression(memberReferenceExpression) =
        if (string memberReferenceExpression).StartsWith "FSharpList" && (string memberReferenceExpression).EndsWith ".Empty" then 
            memberReferenceExpression.ReplaceWith(AST.FSListExpression.Empty)
        else
            base.VisitMemberReferenceExpression(memberReferenceExpression)
 
 type public SubstituteFunctions() =
    inherit DepthFirstAstVisitor()
    interface IAstTransform with
        member this.Run(node : AstNode) =
            let visitor = this :> IAstVisitor
            ignore (node.AcceptVisitor(visitor))
    
    override this.VisitMemberType(memberType) =
        let annotation = memberType.Annotation<Mono.Cecil.TypeDefinition>()
        if(annotation <> null && annotation.BaseType.FullName.StartsWith "Microsoft.FSharp.Core.FSharpFunc") then
            
            let createAstBuilder (x : TypeDefinition) =
                let currentModule = x.Module
                let context = new DecompilerContext(currentModule)
                context.CurrentType <- x
                new AstBuilder(context)

            let codeDomBuilder = createAstBuilder annotation
            codeDomBuilder.AddType(annotation)
            let typeDecl = codeDomBuilder.SyntaxTree.FirstChild :?> TypeDeclaration
            let invoke = (typeDecl.Members |> Seq.find (fun x -> x :? MethodDeclaration && (x :?> MethodDeclaration).Name = "Invoke")):?> MethodDeclaration
            let parameters = invoke.Parameters
            let body = invoke.Body.FirstChild.FirstChild.Clone() :?> Expression
            body.AcceptVisitor(new deleteThisRefVisitor())
            let res = new AnonymousFunction()
            parameters |> Seq.iter (fun x -> res.AddChild<ParameterDeclaration>(x.Clone() :?> ParameterDeclaration, Roles.Parameter))
            res.AddChild<Expression>(body, Roles.Expression)
            
            if memberType.Parent :? ObjectCreateExpression then
                memberType.Parent.ReplaceWith(res)
            else
                ()
        else
            base.VisitMemberType(memberType)

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
            new SubstituteFunctions();
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