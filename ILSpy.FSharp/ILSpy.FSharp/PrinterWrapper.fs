module ILSpy.FSahrp.PrinterWrapper

open ICSharpCode.Decompiler

let inline (@@--) l r = 
    fun (output:ITextOutput) ->
        l output
        output.WriteLine()
        output.Indent()
        r output
        output.Unindent() 

let inline (---) l r = 
    fun (output:ITextOutput) ->
        l output
        output.WriteLine()        
        r output

let inline (++) l r = 
    fun (output:ITextOutput) ->
        l output
        output.Write(" ")
        r output

let inline (-+) l r = 
    fun (output:ITextOutput) ->    
        l output
        r output

let fold b = 
    fun (output:ITextOutput) ->    
        output.MarkFoldStart()
        b output
        output.MarkFoldEnd() 


let wordL (txt:string) = fun (output:ITextOutput) -> output.Write txt 

let defL txt n local = fun (output:ITextOutput) -> output.WriteDefinition(txt,n,local)

let aboveListL lst =
    fun (output:ITextOutput) -> lst |> List.iter (fun layoutF -> layoutF output; output.WriteLine())

let print (output:ITextOutput) layoutF = layoutF output

