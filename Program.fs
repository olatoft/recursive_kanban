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

module Api =
    let addTask parent title =
        let task = {
            Title = title
            Status = Todo
            Parent = parent
            Children = []
        }

        let newChildren =
            match parent with
            | Task parent -> task :: parent.Children
            | Root parent -> task :: parent.Children

        let newParent =
            match parent with
            | Task parent -> Parent.Task { parent with Children = newChildren }
            | Root parent -> Parent.Root { parent with Children = newChildren }

        newParent

    let updateStatus task newStatus =
        { task with Status = newStatus }

// -------------
// ---- CLI ----
// -------------

module Cli =
    let getUserInput message =
        printf message
        let input = System.Console.ReadLine()
        input

    let getTitle () =
        let title = getUserInput "Tittel: "
        Title title

    let addTask parent =
        let title = getTitle ()
        Api.addTask parent title

let root = Parent.Root {
    Children = []
}

let newRoot = Cli.addTask root

[<EntryPoint>]
let main _ =
    0
