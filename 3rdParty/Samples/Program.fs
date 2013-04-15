// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
module Samples

open System
open System.IO

[<FlagsAttribute>]
type MyEnum =
    |Black = 0
    |Red = 1
    |Green = 2
    |Blue = 4

[<EntryPoint>]
let main argv = 
    
    for i in 0 .. 8 do
        Console.WriteLine (i.ToString() + " " + (enum<MyEnum>(i).ToString()))

    for i in 0 .. 8 do
        Console.WriteLine (i.ToString() + " " + ((~~~enum<MyEnum>(i)).ToString()))

    Console.ReadKey() |> ignore


    0 // return an integer exit code
