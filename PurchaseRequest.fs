#if INTERACTIVE
#load "Replay.fs"
#else
module PurchaseRequest
#endif

open Replay

// Our "Active Directory" mutable data source
let managedBy = new System.Collections.Generic.Dictionary<string,string>()
managedBy.["Bob"]       <- "Alice"
managedBy.["Charlie"]   <- "Alice"
managedBy.["Donna"]     <- "Alice"
managedBy.["Ellen"]     <- "Alice"
managedBy.["George"]    <- "Fred"
managedBy.["Helena"]    <- "Fred"
managedBy.["Lauren"]    <- "Fred"
managedBy.["Monique"]   <- "Fred"
managedBy.["Alice"]     <- "Zeus"
managedBy.["Fred"]      <- "Zeus"

type Submission = { Requestor: string; Item: string; Cost: double }

type Decision =
    | Yes
    | No

let purchaseRequest (payload : Submission, log : LogEntry list) = replay log {
    let! manager = IO (fun () -> managedBy.[payload.Requestor])
    let! level1Decision = Assign<Decision> { 
        User = manager ; 
        Job = sprintf "Do you approve the purchase of %A by %A for $%A?" payload.Item payload.Requestor payload.Cost }
    match level1Decision with 
        | Yes -> match payload.Cost with 
                    | c when c < 5000.0 -> 
                        let! poID = Assign<string> { 
                            User = "Payroll" ;
                            Job = sprintf "Please issue a PO for %A by %A for $%A" payload.Item payload.Requestor payload.Cost }
                        return sprintf "Purchase order for %A by %A is approved by %A: #%A" payload.Item payload.Requestor manager poID
                    | _ ->
                        let! director = IO (fun () -> managedBy.[manager]) 
                        let! level2Decision = Assign<Decision> { 
                            User = director ;
                            Job = sprintf "Do you approve the purchase of %A by %A for $%A?" payload.Item payload.Requestor payload.Cost }
                        match level2Decision with 
                            | Yes -> 
                                let! poID = Assign<string> { 
                                    User = "Payroll" ;
                                    Job = sprintf "Please issue a PO for %A by %A for $%A" payload.Item payload.Requestor payload.Cost }
                                return sprintf "Purchase order for %A by %A is approved by %A: #%A" payload.Item payload.Requestor director poID
                            | No -> return sprintf "Purchase request for %A by %A is rejected by %A" payload.Item payload.Requestor director
        | No -> return sprintf "Purchase request for %A by %A is rejected by %A" payload.Item payload.Requestor manager
}

// ====================

let instanceOne = new Instance<Submission>(purchaseRequest, { 
    Requestor = "Charlie" ; 
    Item = "Copier" ; 
    Cost = 3000.0 })
let instanceTwo = new Instance<Submission>(purchaseRequest, { 
    Requestor = "Lauren" ; 
    Item = "Airplane" ; 
    Cost = 150000.0 })
let instanceThree = new Instance<Submission>(purchaseRequest, { 
    Requestor = "Charlie" ; 
    Item = "Paperclip" ; 
    Cost = 0.01 })

// ====================

display <| instanceOne.Start
display <| instanceOne.History

display <| instanceOne.ContinueWith(Yes)
display <| instanceOne.ContinueWith(No)
display <| instanceOne.ContinueWith("21390803-23")

managedBy.["Charlie"]   <- "Fred" // Making a change in the datastore

// ====================

display <| instanceTwo.Start
display <| instanceTwo.ContinueWith(Yes)
display <| instanceTwo.ContinueWith(No)

// ====================

display <| instanceThree.Start
display <| instanceThree.History
               
display <| instanceThree.ContinueWith(Yes)
display <| instanceThree.ContinueWith(No)
display <| instanceThree.ContinueWith("21390803-25")