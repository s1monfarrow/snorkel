﻿namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("snorkel")>]
[<assembly: AssemblyProductAttribute("snorkel")>]
[<assembly: AssemblyDescriptionAttribute("markdown to other stuff    ")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
