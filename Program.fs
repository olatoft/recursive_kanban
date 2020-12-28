// -------------
// ---- API ----
// -------------

type Title = Title of string

type Status =
    | Todo
    | Doing
    | Done

type Root = {
    Children: Task list
}

and Parent =
    | Task of Task
    | Root of Root

and Task = {
    Title: Title
    Status: Status
    Parent: Parent
    Children: Task list
}

// -------------
// ---- CLI ----
// -------------

[<EntryPoint>]
let main _ =
    0
