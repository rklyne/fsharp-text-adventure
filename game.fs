
module text_adventure

open System

type Direction = 
    | North = 'N'
    | South = 'S'
    | East = 'E'
    | West = 'W'

type ScreenChange = ScreenList -> Screen
and  Screen = {
    name: string
    exits : Map<Direction, ScreenChange>
    text : string
}
and ScreenList = Screen list

type Character = {
    name : string
}

type GameSetup = {
    screens : ScreenList
    starting_screen : Screen
}

type Command = (GameState -> GameState)

and Item = {
    name : string
    transitive_verbs : Map<string, (Item -> Command)>
    intransitive_verbs : Map<string, Command>
}

and GameState = {
    screen : Screen
    character : Character
    inventory : Inventory
    items : Map<string, Item>
    screens : ScreenList
}

and Inventory = (Item list)


let room_named name = {
    name = name
    text = ""
    exits = Map<Direction, ScreenChange> [||]
}


// Game actions

let print_message (message: string) state =
    Console.WriteLine message
    state

let move direction state =
    let exits = state.screen.exits
    if not (exits.ContainsKey direction) then print_message "Illegal move" state
    else
        { state with
            screen = (exits.Item direction) state.screens
        }

let help_text = "Use n,e,w,s characters to move. 'quit' to quit"

let quit_game state = 
    { state with screen = room_named "END" }

let prompt (message:string) = 
    Console.Write message
    Console.ReadLine()

let read_user_command state =
    let parse_command (text:string) = 
        match text.ToLower() with
            | "n" | "north" -> print_message "north..." >> move Direction.North
            | "e" | "east" -> print_message "east..."  >> move Direction.East
            | "s" | "south" -> print_message "south..." >> move Direction.South
            | "w" | "west" -> print_message "west..."  >> move Direction.West
            | "quit" -> print_message "Quitting..." >> quit_game
            | "?"       -> print_message help_text
            | _ -> print_message ("Bad command: " + text) >> print_message "'?' for help"
    let input = prompt (state.character.name + "> ")
    parse_command input

let display_frame state =
    Console.WriteLine(state.screen.text)

let initial_state setup character = {
    screen = setup.starting_screen
    character = character
    inventory = []
    items = Map<string, Item> [||]
    screens = setup.screens
}

let game_over_condition setup state = 
    state.screen.exits.Count = 0

let prompt_for_name() = prompt "Please enter your name: "

let play_game setup = 
    let apply_command command state = command state
    let game_over = game_over_condition setup
    let end_game state = display_frame state
    let rec run_frame (current_state:GameState) = 
        display_frame current_state
        let command = read_user_command current_state
        let new_state = apply_command command current_state
        if not (game_over new_state) then
            run_frame new_state
            ()
        else
            end_game new_state


    let character = { Character.name = prompt_for_name() }
    run_frame (initial_state setup character)
    
let room_described text room = 
    { room with text = text }

let get_screen_named name (screens:ScreenList) = 
    let matching = screens |> List.filter (fun screen -> screen.name = name)
    let count = List.length matching
    if (count = 0) then (failwith ("No such screen in set: " + name))
    elif (count > 1) then (failwith ("Multiple screens match name " + name))
    else (List.head matching)

let with_named_exit direction name screen = 
    { screen with
        exits = screen.exits.Add (direction, (get_screen_named name)) }

let load_level_pack () =
    let screen_named = get_screen_named
    let with_text = room_described
    let mx lst =
        Map<Direction, ScreenChange> lst
    let screens = [
        room_named "start" |> with_text "You are in a room. To the north there is an open door..." |> with_named_exit Direction.North "corridor"
        room_named "corridor" |> with_text "You are in a corridor running north to south. To the north there is an sign reading 'winning this way'..." |> with_named_exit Direction.South "start" |> with_named_exit Direction.North "door_room"
        room_named "door_room" |> with_text "" |> with_named_exit Direction.North "end"
        room_named "end" |> with_text "You win!"
    ]
    {
        starting_screen = screen_named "start" screens
        screens = screens
    }
    
[<EntryPoint>]
let main args =
    let level_pack = load_level_pack()
    play_game level_pack
    0

