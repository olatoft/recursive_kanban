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

type TaskDto = {
    Title : string
    Status : string
    Parent : string
    Children : TaskDto array
}

module Api =
    let addTask parent title =
        let task : Task = {
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

    let updateStatus (task : Task) newStatus =
        { task with Status = newStatus }

//module IO =
//    let fileToDto filePath =


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

    let printChildrenWithStatus (children : Task list) status =
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

    let selectChild (parent : Parent) =
        let action index (child : Task) =
            child.Title
            |> Title.value
            |> printfn "%d: %s" index
            
        match parent with
        | Task parent ->
            parent.Children
            |> List.iteri action
        | Root parent ->
            parent.Children
            |> List.iteri action

        let index =
            getUserInput "Velg barn: "
            |> int

        match parent with
        | Task parent ->
            parent.Children
            |> List.item index
        | Root parent ->
            parent.Children
            |> List.item index

    let rec getEvent () =
        let event = getUserInput "Kva vil du gjere? 1: Gå til barne-oppgåve. 2: Lag ny oppgåve. Valg: "

        match event with
        | "1" -> selectChild
        | "2" -> addTask
        | _ -> getEvent ()

    let run parent =
        printParent parent

let root = Parent.Root {
    Children = []
}

[<EntryPoint>]
let main _ =
    let newRoot = Cli.addTask root
    // Cli.printParent newRoot
    Cli.run newRoot
    0
