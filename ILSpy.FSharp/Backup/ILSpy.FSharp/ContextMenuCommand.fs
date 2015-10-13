namespace ILSpy.FSharp

open System
open System.Linq
open ICSharpCode.ILSpy
open ICSharpCode.ILSpy.TreeNodes
open ICSharpCode.TreeView
open Microsoft.Win32
open Mono.Cecil

[<ExportContextMenuEntryAttribute(Header = "_Save Assembly")>]
type SaveAssembly() =
    interface IContextMenuEntry with

        member this.IsVisible(context: TextViewContext) =
            context.SelectedTreeNodes <> null && context.SelectedTreeNodes.All(fun n -> n :? AssemblyTreeNode)

        member this.IsEnabled(context: TextViewContext) =
            context.SelectedTreeNodes <> null && context.SelectedTreeNodes.Length = 1

        member this.Execute(context: TextViewContext) =
            
            if context.SelectedTreeNodes = null then ()
            
            let node = context.SelectedTreeNodes.[0] :?> AssemblyTreeNode
            let asm = node.LoadedAssembly.AssemblyDefinition
            if asm <> null then
                let dlg = new SaveFileDialog()
                dlg.FileName <- node.LoadedAssembly.FileName
                dlg.Filter <- "Assembly|*.dll;*.exe"
                if dlg.ShowDialog(MainWindow.Instance).HasValue && dlg.ShowDialog(MainWindow.Instance).Value then
                    asm.MainModule.Write(dlg.FileName)