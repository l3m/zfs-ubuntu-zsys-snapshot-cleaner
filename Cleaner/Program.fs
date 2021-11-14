module l3m.ZfsUtils

open System.Collections.Generic

type ZfsSnapshot = {
    FullName: string
    ZSysId: string
    CreationDate: System.DateTimeOffset
}

type ZfsSnapshots = {
    ZSysId: string
    // most recent of all snapshot in collection
    CreationDate: System.DateTimeOffset 
    Collection: ZfsSnapshot array
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
    |> Seq.map (fun kv ->
        let mostRecent =
            kv.Value
            |> Seq.sortByDescending (fun x -> x.CreationDate)
            |> Seq.head
        {
            ZSysId = kv.Key
            Collection = kv.Value |> Array.ofSeq
            CreationDate = mostRecent.CreationDate
        }
        )
    |> Array.ofSeq

let getLatestSnaps (snaps: ZfsSnapshots array) =
    let sorted =
        snaps
        |> Array.filter (fun x -> Array.length x.Collection > 1)
        |> Array.sortByDescending (fun x -> x.CreationDate)
    
    let latest = Array.take 10 sorted
    let others = Array.skip 10 sorted
    latest, others 

let zfsPrint (name: string) =
    printf $"zfs %s{name}\n"

let zfsDestroy (snapshot: ZfsSnapshots) =
    snapshot.Collection
    |> Array.iter (fun n ->
        zfs $"destroy %s{n.FullName}"
        |> System.Console.WriteLine)

type Confirmation =
    | Yes
    | No
    | All
    | Exit

let deleteIfConfirmed (snapshots: ZfsSnapshots array) =
    let confirmDeletion (snapshot: ZfsSnapshots) =
        let fmtDate = snapshot.CreationDate.ToString "dd.MM.yyy HH:mm:ss"
        let msg = $"Confirm deleting of snapshots %s{snapshot.ZSysId} created %s{fmtDate} [y/n/A/x] (Yes, No, All, Exit)"
        System.Console.WriteLine msg
        let x = System.Console.ReadKey ()
        match x.KeyChar with
        | 'y'
        | 'Y' -> Confirmation.Yes
        | 'A' -> Confirmation.All
        | 'x'
        | 'X' -> Confirmation.Exit
        | _ -> Confirmation.No
    
    let mutable confirmation = Confirmation.No
    
    for snapshot in snapshots do
        match confirmation with
        | Confirmation.All ->
            zfsDestroy snapshot
        | Confirmation.Exit ->
            ()
        | _ ->
            confirmation <- confirmDeletion snapshot
            if confirmation = Confirmation.Yes || confirmation = Confirmation.All then
                zfsDestroy snapshot

[<EntryPoint>]
let main _ =
    let snaps = zfsGetSnapshots ()
    let _, others = getLatestSnaps snaps
    
    deleteIfConfirmed others
    
    1
