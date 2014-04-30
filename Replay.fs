module Replay

type Assignment = { User : string ; Job : string}

type 'T Favor = 
    | Assign of Assignment
    | IO of (unit -> 'T)

let Assign<'T> assignment = Favor<'T>.Assign assignment

type LogEntry =
    | AssignmentCompleted of obj
    | IOPerformed of obj

type SessionOutcome = 
    | Incomplete of Assignment * LogEntry list
    | Complete of obj * LogEntry list
    | Error of string

type ReplayBuilder(log : LogEntry list) =
    let log = log
    let logEnumerator = (List.toSeq log).GetEnumerator();
    let mutable newLog = new System.Collections.Generic.List<LogEntry>()

    // Remember: `let! result = ???   ;;   ...   ` 
    // means     `Bind( ??? , fun result -> ... )`
    member this.Bind<'T>(favor : 'T Favor, rest : 'T -> SessionOutcome) : SessionOutcome = 
        match favor with
            | Assign(assignment) ->
                match logEnumerator.MoveNext() with
                    | true -> match logEnumerator.Current with
                                    | AssignmentCompleted result -> 
                                        try
                                            rest (result :?> 'T)
                                        with
                                            | ex -> Error ("Code error: " + ex.Message)
                                    | _ -> Error "Log error: Expecting an AssignmentCompleted or end of log"
                    | false -> Incomplete (assignment, List.concat [ log ; List.ofSeq newLog ])
            | IO(operation) ->
                match logEnumerator.MoveNext() with
                    | true -> match logEnumerator.Current with
                                    | IOPerformed result -> 
                                        try
                                            rest (result :?> 'T)
                                        with
                                            | ex -> Error ("Code error: " + ex.Message)
                                    | _ -> Error "Log error: Expecting an IOPerformed or end of log"
                    | false -> 
                        let ioResult = operation()
                        newLog.Add(IOPerformed ioResult)
                        try
                            rest ioResult
                        with
                            | ex -> Error ("Code error: " + ex.Message)
    member this.Return() = 
        Complete (null, List.concat [ log ; List.ofSeq newLog ])
    member this.Return(message) = 
        Complete (message, List.concat [ log ; List.ofSeq newLog ])

type Instance<'P> (procDef : 'P * LogEntry list -> SessionOutcome, payload : 'P) =
    let procDef = procDef
    let mutable history : LogEntry list = []
    let run (hist : LogEntry list) = 
        match procDef (payload,  hist) with
            | Incomplete(assignment, newLog) -> 
                history <- newLog
                assignment :> obj
            | Complete(result, newLog) -> 
                history <- newLog
                result
            | Error(message) -> message :> obj

    member this.Start = run history
    member this.ContinueWith answer : obj = run (List.rev (AssignmentCompleted answer :: List.rev history))
    member this.History = List.toSeq history

let replay (log : LogEntry list) = new ReplayBuilder(log)

let display outcome = printfn "%A" outcome