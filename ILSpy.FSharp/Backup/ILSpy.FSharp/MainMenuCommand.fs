namespace ILSpy.FSharp

open System
open ICSharpCode.ILSpy

[<ExportMainMenuCommand(Menu = "_File", MenuIcon = "Clear.png", Header = "_Clear List", MenuCategory = "Open", MenuOrder = 1.5)>]
[<ExportToolbarCommand(ToolTip = "Clears the current assembly list", ToolbarIcon = "Clear.png", ToolbarCategory = "Open", ToolbarOrder = 1.5)>]
type UnloadAllAssembliesCommand() =
    inherit SimpleCommand()

    override this.Execute(parameter : obj) =
        for loadedAssembly in MainWindow.Instance.CurrentAssemblyList.GetAssemblies() do
            loadedAssembly.AssemblyList.Unload(loadedAssembly)
