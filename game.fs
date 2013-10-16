
module text_adventure

open System

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
    name: string ;
    exits : Map<Direction, ScreenChange> ;
    text : string ;
    items : Inventory ;
}
and ScreenList = Screen list
and Command = (GameState -> GameState)
and Item = {
    name : string ;
    transitive_verbs : Map<string, (Item -> Command)> ;
    intransitive_verbs : Map<string, Command> ;
}
and GameState = {
    screen : Screen ;
    character : Character ;
    items : Inventory ;
    screens : ScreenList ;
}
and Inventory = (Item list)

let empty_inventory = []:Inventory

type GameSetup = {
    screens : ScreenList ;
    starting_screen : Screen ;
}

let room_named name = {
    name = name ;
    text = "";
    exits = Map<Direction, ScreenChange> [||] ;
    items = [] ;
}


// Inventory functions
//

let rec remove_item item inventory = match inventory with
    | []    -> []
    | _item :: rest -> if _item.name = item.name then rest else remove_item item rest

let add_item item (inventory:Inventory) = item :: inventory

let add_item_to_screen item screen:Screen = {screen with
    items = add_item item screen.items
}

let with_tverb verb action item = 
    { item with 
        transitive_verbs = item.transitive_verbs.Add (verb, action) ;
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

let print_message message state =
    Console.WriteLine (message:string)
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
    { state with
        screen = room_named "END"
    }

let pick_up_object item state =
    {   (update_screen (fun screen ->
            { screen with 
                items = remove_item item screen.items
            }) state
        ) with
        items = add_item item state.items
    }


// IO helpers
//
let prompt (message:string) = 
    Console.Write message
    Console.ReadLine()

let read_user_command state =
    let parse_command (text:string) =
        let other_input (text:string) =
            let verbs = [for item in state.screen.items do
                yield! [for key_value in item.intransitive_verbs do
                    let verb, command = key_value.Key, key_value.Value
                    if text.StartsWith(verb) then
                        yield command
                ]
            ]
            if (List.length verbs) = 1 then
                (verbs.[0]:Command)
            else 
                print_message ("Bad command: " + text) >> print_message "'?' for help"
        match text.ToLower() with
            | "n" | "north" -> print_message "north..." >> move Direction.North
            | "e" | "east" -> print_message "east..."  >> move Direction.East
            | "s" | "south" -> print_message "south..." >> move Direction.South
            | "w" | "west" -> print_message "west..."  >> move Direction.West
            | "quit" | "exit" -> print_message "Quitting..." >> quit_game
            | "?"       -> print_message help_text
            | _ -> other_input text
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
            

let prompt_for_name() = prompt "Please enter your name: "

// Game startup and shutdown
//
let initial_state setup character = 
    {
        screen = (setup.starting_screen  :Screen)    ;
        character = (character           :Character) ;
        items = []                                   ;
        screens = setup.screens                      ;
    }

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


    let character:Character = {name=prompt_for_name()}
    run_frame (initial_state setup character)
    

// Levels
//
let room_described text room =
    { room with
        text = text 
    }

let get_screen_named name (screens:ScreenList) = 
    let matching = ((List.filter (fun screen -> (screen.name = name)) screens):ScreenList)
    let count = List.length matching
    if (count = 0) then (failwith ("No such screen in set: " + name))
    elif (count > 1) then (failwith ("Multiple screens match name " + name))
    else ((List.head matching):Screen)

let with_named_exit direction name screen =
    { screen with
        exits = screen.exits.Add (direction, (get_screen_named name)) ;
    }

let new_item name =
    if name = "" then
        failwith "Name is empty"
    else 
        {
            name = name ;
            transitive_verbs = Map<string, Item->Command> [] ;
            intransitive_verbs = Map<string, Command> [] ;
        }


let opens_with_key key screen_mod door =
    let verb k =
        if k.name = key.name then update_screen screen_mod
        else print_message "You need the key."
    door |> with_tverb "open" verb

let new_door key direction exit = 
    new_item "door" |> opens_with_key key (with_named_exit direction exit)
let with_locked_exit key direction exit screen:Screen =
    add_item_to_screen (new_door key direction exit) screen


let load_level_pack () =
    let screen_named = get_screen_named
    let with_text = room_described
    let with_item = add_item_to_screen
    let collectible = with_tverb "pick up" pick_up_object
    let mx lst =
        Map<Direction, ScreenChange> lst
    let key1 = new_item "Front door key" |> collectible
    let with_key1_exit = with_locked_exit key1
    let screens = [
        room_named "start" |> with_text "You are in a room. To the north there is an open door..." |> with_named_exit Direction.North "corridor" |> with_item key1 ;
        room_named "corridor" |> with_text "You are in a corridor running north to south. To the north there is an sign reading 'winning this way'..." |> with_named_exit Direction.South "start" |> with_named_exit Direction.North "door_room";
        room_named "door_room" |> with_text "The front door is here" |> with_key1_exit Direction.North "end" ;
        room_named "end" |> with_text "You win!" ;
    ]
    {
        starting_screen = screen_named "start" screens;
        screens = screens
    }:GameSetup
    


[<EntryPoint>]
let main args =
    let level_pack = load_level_pack()
    play_game level_pack
    0

