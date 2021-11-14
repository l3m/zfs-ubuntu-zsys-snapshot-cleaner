module l3m.ZfsUtils

open System.Collections.Generic

type ZfsSnapshot = {
    FullName: string
    ZSysId: string
    CreationDate: System.DateTimeOffset
}

let runCmd cmd cmdArgs = 
    use p = new System.Diagnostics.Process()
    p.StartInfo <-
        let s = System.Diagnostics.ProcessStartInfo()
        s.RedirectStandardInput <- true
        s.RedirectStandardOutput <- true
        s.CreateNoWindow <- true
        s.UseShellExecute <- false       
        s.FileName <- cmd
        s.Arguments <- cmdArgs
        s
    
    p.Start () |> ignore
    p.StandardOutput.ReadToEnd()

let zfs zfsArgs = runCmd "zfs" zfsArgs

let tryExtractHash (snapshotName: string) =
    let len = String.length snapshotName
    if len < 12 then
        None
    else 
    let hashStart = len - 6
    let underscore = snapshotName.Substring(hashStart-1, 1)
    let hash = snapshotName.Substring(hashStart)
    if underscore <> "_" then
        None
    else
        Some hash 

let (|Int64|_|) (input: string) =
    match System.Int64.TryParse input with
    | true, value -> Some value
    | false, _ -> None

let mkSnapshotEntry (line: string) =
    let parts = line.Split()
    match parts with
    | [| fullName; creationDateUnix |] ->
        let hashOpt = tryExtractHash fullName
        match hashOpt, creationDateUnix with
        | Some hash, Int64 unixSeconds ->
            let creationDate = System.DateTimeOffset.FromUnixTimeSeconds unixSeconds
            let snapshot : ZfsSnapshot = {
                FullName = fullName
                ZSysId = hash
                CreationDate = creationDate
            }            
            Some snapshot
        | _ -> None
    | _ -> None    

let zfsGetSnapshots () =
    let snapshotsOutput = zfs "list -Hp -t snapshot -o name,creation"
    let lines =
        let opts = System.StringSplitOptions.RemoveEmptyEntries
        snapshotsOutput.Split(System.Environment.NewLine, opts)
    let snapshots = 
        lines
        |> Array.choose mkSnapshotEntry
    
    let snapsById = Dictionary<string, List<ZfsSnapshot>>()
    snapshots
    |> Array.iter (fun snap ->
        let snapsWithId = 
            match snapsById.TryGetValue snap.ZSysId with
            | true, snapsWithId -> snapsWithId
            | false, _ ->
                let s = new List<ZfsSnapshot>()
                snapsById.[snap.ZSysId] <- s
                s
        snapsWithId.Add snap
        )
    
    snapsById
    |> Seq.iter (fun kv ->        
        printf "%s %i\n" kv.Key kv.Value.Count)
    

[<EntryPoint>]
let main argv =
    zfsGetSnapshots ()
    1
