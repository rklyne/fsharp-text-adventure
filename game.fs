
module text_adventure

open System
open FParsec

// Types

type Direction = 
    | North = 'N'
    | South = 'S'
    | East = 'E'
    | West = 'W'

type Character = {
    name : string ;
}

//More complex types
//
type ScreenChange = ScreenList -> Screen
and  Screen = {
    name: string
    exits : Map<Direction, ScreenChange>
    text : string
    items : Inventory
}
and ScreenList = Screen list
and Command = (GameState -> GameState)
and Item = {
    name : string
}
and GameState = {
    screen : Screen
    character : Character
    items : Inventory
    screens : ScreenList
}
and Inventory = (Item list)

let select_item (name_in:string) items =
    let name = name_in.ToLower()
    try
        match [for item in items do if item.name = name then yield item] with
            | [] -> failwith "no item"
            | [x] -> x
            | _ -> failwith ("many items named " + name)
    with
        | :? Exception -> match [for item in items do if item.name.ToLower().Contains(name) then yield item] with
            | [] -> failwith ("no such item '" + name + "'")
            | [x] -> x
            | _ -> failwith ("too many items match " + name)

assert ((select_item "key" [{Item.name="the key"}]).name = "the key")

let empty_inventory = []:Inventory

type GameSetup = {
    screens : ScreenList ;
    starting_screen : Screen ;
}

let room_named name = {
    name = name
    text = ""
    exits = Map<Direction, ScreenChange> [||]
    items = []
}

// Inventory functions
//

let rec remove_item item inventory = match inventory with
    | []    -> []
    | _item :: rest -> if _item.name = item then rest else remove_item item rest

let add_item item (inventory:Inventory) = item :: inventory

let add_item_to_screen item screen:Screen = {screen with
    items = add_item item screen.items
}

// Screen helper functions
//

let update_screen (screen_mod:Screen->Screen) (state:GameState) =
    { state with
        screen = screen_mod state.screen ;
        screens = List.map (fun (screen:Screen) ->
            if screen.name = state.screen.name then
                screen_mod screen
            else
                screen
            )
            (state.screens:ScreenList) ;
    }

// Game actions
//

let print_message (message: string) state =
    Console.WriteLine message
    state

let display_inventory state =
    print_message ("you have " + String.Join(", ", [for item in state.items -> item.name])) state

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

let pick_up_object item_name state =
    let item = select_item item_name state.screen.items
    {   (update_screen (fun screen ->
            { screen with 
                items = remove_item item.name screen.items
            }) state
        ) with
        items = add_item item state.items
    }

// Parsers
//

let ws = many1 (anyOf "\t ")
let word_chars = String.Join("", ['a' .. 'z']) + String.Join("", ['A' .. 'Z'])                
let word = ws >>. many1Chars (anyOf word_chars)
let rec phrase words =                                                                                       
    match words with                                                                                         
        | [] -> failwith "This phrase parser requires the words in the phrase"                               
        | [wd] -> pstring wd                                                                                 
        | wd::wds -> pstring wd .>> (ws >>. (phrase wds))            
let pick_up = phrase ["pick";"up"] >>. word |>> (fun item -> pick_up_object item)

let parse_exit = pstringCI "exit" <|> pstringCI "quit" |>> (fun _ -> quit_game)
let parse_show_items = (pstringCI "inventory" <|> pstringCI "items") |>> (fun _ -> display_inventory)
let parse_action = pick_up <|> parse_show_items
let parse_movement = 
        (pstring "n" <|> pstring "north" |>> (fun _ -> (print_message "north..." >> move Direction.North)))
    <|> (pstring "e" <|> pstring "east" |>> (fun _ -> (print_message "east..."  >> move Direction.East)))
    <|> (pstring "s" <|> pstring "south" |>> (fun _ -> (print_message "south..." >> move Direction.South)))
    <|> (pstring "w" <|> pstring "west" |>> (fun _ -> (print_message "west..."  >> move Direction.West)))

let parse_game_command = parse_movement <|> parse_exit <|> parse_action

// IO helpers
//
let prompt (message:string) = 
    Console.Write message
    Console.ReadLine()

let read_user_command state =
    let parse_command text =
        match run parse_game_command text with
            | Success(cmd, _, _) -> cmd
            | Failure(msg, _, _) -> print_message ("Bad command: \r\n" + msg)
    let input = prompt (state.character.name + "> ")
    parse_command input

let display_frame state =
    Console.WriteLine(state.screen.text)
    if List.length state.screen.items <> 0 then
        Console.WriteLine("You see a {0} here.",
            String.Join(" and a ", 
                [for item in state.screen.items do
                    yield item.name
                ])
        )
            

let initial_state setup character = {
    screen = setup.starting_screen
    character = character
    items = []
    screens = setup.screens
}
let prompt_for_name() = prompt "Please enter your name: "

// Game startup and shutdown
//

let game_over_condition setup state = 
    state.screen.exits.Count = 0

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

// Levels
//
    
let room_described text room = 
    { room with text = text }

let get_screen_named name (screens:ScreenList) = 
    let matching = screens |> List.filter (fun screen -> screen.name = name)
    match matching with
    | [] -> failwith ("No such screen in set: " + name)
    | [x] -> x
    | _ -> failwith ("Multiple screens match name " + name)

let with_named_exit direction name screen = 
    { screen with
        exits = screen.exits.Add (direction, (get_screen_named name)) }

let new_item name =
    if name = "" then
        failwith "Name is empty"
    else 
        {
            Item.name = name ;
            // transitive_verbs = Map<string, Item->Command> [] ;
            // intransitive_verbs = Map<string, Command> [] ;
        }


let opens_with_key key screen_mod door =
    let verb k =
        if k.name = key.name then update_screen screen_mod
        else print_message "You need the key."
    door // |> with_tverb "open" verb

let new_door key direction exit = 
    new_item "door" |> opens_with_key key (with_named_exit direction exit)
let with_locked_exit key direction exit screen:Screen =
    add_item_to_screen (new_door key direction exit) screen


let load_level_pack () =
    let screen_named = get_screen_named
    let with_text = room_described
    let with_item = add_item_to_screen
    // let collectible = with_tverb "pick up" pick_up_object
    let mx lst =
        Map<Direction, ScreenChange> lst
    let key1 = new_item "Front door key" // |> collectible
    let with_key1_exit = with_locked_exit key1
    let screens = [
        room_named "start" |> with_text "You are in a room. To the north there is an open door..." |> with_named_exit Direction.North "corridor" |> with_item key1 ;
        room_named "corridor" |> with_text "You are in a corridor running north to south. To the north there is an sign reading 'winning this way'..." |> with_named_exit Direction.South "start" |> with_named_exit Direction.North "door_room";
        room_named "door_room" |> with_text "The front door is here" |> with_key1_exit Direction.North "end" ;
        room_named "end" |> with_text "You win!" ;
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

