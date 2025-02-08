namespace AdventUtilities

open System.IO

type InputData() =
    member this.GetSolutionDirectory() =
        let rec findSolutionDirectory dir =
            let solutionFile = Path.Combine(dir, "2024-Fsharp.sln")
            if File.Exists solutionFile then
                dir
            else
                let parentDir = Directory.GetParent dir
                if parentDir = null then
                    failwith "Solution directory not found"
                else
                    findSolutionDirectory parentDir.FullName
                    
        findSolutionDirectory(Directory.GetCurrentDirectory())
        
    member this.GetFilePath day fileName =
        let basePath = Path.Combine(this.GetSolutionDirectory(), "InputFiles")
        let printName = $"Day%02d{day}/%s{fileName}.txt"
        Path.Combine(basePath, printName)
        
    member this.ReadAllLines day fileName =
        let filePath = this.GetFilePath day fileName
        File.ReadAllLines filePath
        
    member this.ReadAllText day fileName =
        let filePath = this.GetFilePath day fileName
        File.ReadAllText filePath
        
    member this.ReadWithStreamReader day fileName =
        let filePath = this.GetFilePath day fileName
        seq {
            use sr = new StreamReader(filePath)
            while not sr.EndOfStream do
                yield sr.ReadLine()
        }