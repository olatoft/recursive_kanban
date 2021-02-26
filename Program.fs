// -------------
// ---- API ----
// -------------

type Title = Title of string

module Title =
    let value (Title title) =
        title

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

    let printChildrenWithStatus children status =
        children
        |> List.filter (fun task -> task.Status = status)
        |> List.map (fun task -> task.Title)
        |> List.map Title.value
        |> List.iter (printfn "    %s")

    let printParent parent =
        match parent with
        | Task parent ->
            parent.Title
            |> Title.value
            |> printfn "Tittel: %s"

            let status =
                match parent.Status with
                | Todo -> "Todo"
                | Doing -> "Doing"
                | Done -> "Done"

            printfn "Status: %s" status

            printfn "Todo:"
            printChildrenWithStatus parent.Children Todo

            printfn "Doing:"
            printChildrenWithStatus parent.Children Doing

            printfn "Done:"
            printChildrenWithStatus parent.Children Done

        | Root parent ->
            printfn "Todo:"
            printChildrenWithStatus parent.Children Todo

            printfn "Doing:"
            printChildrenWithStatus parent.Children Doing

            printfn "Done:"
            printChildrenWithStatus parent.Children Done

let root = Parent.Root {
    Children = []
}

[<EntryPoint>]
let main _ =
    let newRoot = Cli.addTask root
    Cli.printParent newRoot
    0
