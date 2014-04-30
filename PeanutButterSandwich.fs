#if INTERACTIVE
#load "Replay.fs"
#else
module PeanutButterSandwich
#endif

open Replay

let peanutButterSandwich (name : string, log : LogEntry list) = replay log {
    let! result = Assign<string> { 
        User = name ; 
        Job = sprintf "Do you like peanut butter, %A? (Yes or No)" name }
    match result with 
        | "Yes" ->
            let! now = IO (fun () -> System.DateTime.Now)
            let! result3 = Assign<string> { 
                User = name ; 
                Job = sprintf "It is currently %A.  Shall I make you a peanut butter sandwich, %A? (Yes or No)" now name }
            match result3 with
                | "Yes" -> return "User likes peanut butter and wants a sandwich."
                | "No" -> return "User likes peanut butter but does not want a sandwich."
                | "TEST FAIL" -> return 1 / 0
                | _ -> return "User likes peanut butter but can't spell"
        | "No" -> return "User does not like peanut butter."
        | _ -> return "User can't spell"
}

// Initiation with our username as the expected payload, empty log
display <| peanutButterSandwich ("Jason", [])

// Answering the first question by appending the answer to the log
display <| peanutButterSandwich ("Jason", 
    [ AssignmentCompleted "Yes" ])

// Providing the time already retrieved
display <| peanutButterSandwich ("Jason", 
    [ AssignmentCompleted "Yes"; 
      IOPerformed (System.DateTime.Parse("4/22/2014 12:00:03 PM")) ])

// Answering the second question by appending the answer to the log
display <| peanutButterSandwich ("Jason", 
    [ AssignmentCompleted "Yes"; 
      IOPerformed (System.DateTime.Parse("4/22/2014 12:00:03 PM")); 
      AssignmentCompleted "Yes" ]) 

// Log-related Error
display <| peanutButterSandwich ("Jason", 
    [ AssignmentCompleted "Yes";
      AssignmentCompleted "Yes" ]) 

// Code-related Error
display <| peanutButterSandwich ("Jason", 
    [ AssignmentCompleted "Yes"; 
      IOPerformed (System.DateTime.Parse("4/22/2014 12:00:03 PM")); 
      AssignmentCompleted "TEST FAIL"]) 


let instanceOne = new Instance<string>(peanutButterSandwich, "Jason")
let instanceTwo = new Instance<string>(peanutButterSandwich, "Riccardo")

display <| instanceOne.Start
display <| instanceOne.ContinueWith("Yes")
display <| instanceOne.ContinueWith("No")

display <| instanceTwo.Start
display <| instanceTwo.ContinueWith("Yes")
display <| instanceTwo.ContinueWith("No")