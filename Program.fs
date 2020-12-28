type Title = Title of string

type Status =
    | Todo
    | Doing
    | Done

type Task = {
    Title: Title
    Status: Status
}

[<EntryPoint>]
let main _ =
    0
