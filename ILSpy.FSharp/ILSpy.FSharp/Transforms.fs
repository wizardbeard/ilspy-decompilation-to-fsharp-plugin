module public ILSpy.FSharp.Transforms


open System

open ICSharpCode.Decompiler
open ICSharpCode.Decompiler.Ast
open ICSharpCode.Decompiler.Ast.Transforms
open ICSharpCode.NRefactory.CSharp
open ICSharpCode.NRefactory
    
type public IntroduceFunctions() =
    inherit DepthFirstAstVisitor<obj, obj>()
    interface IAstTransform with
        member this.Run(node : AstNode) =
            let visitor = (this :> DepthFirstAstVisitor<obj, obj>) :> IAstVisitor<obj, obj>
            let nul = null :> obj
            ignore (node.AcceptVisitor<obj, obj> (visitor, nul))      
        
    override this.VisitTypeDeclaration (typeDeclaration, data) =
        let mutable itsFunc = false
        let children = typeDeclaration.Children |> List.ofSeq
        for child in children do
            if child.NodeType = NodeType.TypeReference then
                if (string child).StartsWith "FSharpFunc" then 
                    itsFunc <- true //break need hard
                else ()
            else ()
        if itsFunc then typeDeclaration.ReplaceWith(AST.AnonymousFunctionDeclaration.GetFromTypeDecl(typeDeclaration) :> AstNode)
        base.VisitTypeDeclaration (typeDeclaration, data)

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
            new DecimalConstantTransform()|]

    let RunTransformationsUntil (node : AstNode) (abortCondition : Predicate<IAstTransform>) (context : DecompilerContext) = 
        
        if node = null then ()
        else
            for transform in CreatePipeline context do
                context.CancellationToken.ThrowIfCancellationRequested()
                if abortCondition <> null && abortCondition.Invoke transform then
                    ()
                else
                    transform.Run node

type public FSAstBuilder(context: DecompilerContext) =
    inherit AstBuilder(context)
    
    member public this.RunTransformations (transformAbortCondition : Predicate<IAstTransform>) =
        FSTransformationPipeline.RunTransformationsUntil this.SyntaxTree transformAbortCondition context
        //this.tra dummy ///transformationsHaveRun = true;
    
    member public this.RunTransformations() =
       
       this.RunTransformations null