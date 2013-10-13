
module text_adventure

open System

type Direction = 
    | North = 'N'
    | South = 'S'
    | East = 'E'
    | West = 'W'

type ScreenChange = ScreenList -> Screen
and  Screen = {
    name: string ;
    exits : Map<Direction, ScreenChange> ;
    text : string ;
}
and ScreenList = Screen list

type Character = {
    name : string ;
}

type GameSetup = {
    screens : ScreenList ;
    starting_screen : Screen ;
}

type Command = (GameState -> GameState)

and Item = {
    name : string ;
    transitive_verbs : Map<string, (Item -> Command)> ;
    intransitive_verbs : Map<string, Command> ;
}

and GameState = {
    setup : GameSetup ;
    screen : Screen ;
    character : Character ;
    inventory : Inventory ;
    items : Map<string, Item> ;
}

and Inventory = (Item list)



// Game actions

let print_message message state =
    Console.WriteLine (message:string)
    state

let move direction state =
    let exits = state.screen.exits
    if not (exits.ContainsKey direction) then print_message "Illegal move" state
    else
        { state with
            screen = (exits.Item direction) state.setup.screens
        }

let help_text = "Use n,e,w,s characters to move. "

let prompt (message:string) = 
    Console.Write message
    Console.ReadLine()

let read_user_command state =
    let parse_command text = 
        match text with
            | "n" | "N" -> print_message "north..." >> move Direction.North
            | "e" | "E" -> print_message "east..."  >> move Direction.East
            | "s" | "S" -> print_message "south..." >> move Direction.South
            | "w" | "W" -> print_message "west..."  >> move Direction.West
            | "?"       -> print_message help_text
            | _ -> print_message ("Bad command: " + text) >> print_message "'?' for help"
    let input = prompt (state.character.name + "> ")
    parse_command input

let display_frame state =
    Console.WriteLine(state.screen.text)

let initial_state setup character = {
    setup = (setup                   :GameSetup) ;
    screen = (setup.starting_screen  :Screen)    ;
    character = (character           :Character) ;
    inventory = ([]                  :Inventory) ;
    items = Map<string, Item> [||]           ;
}

let game_over_condition setup state = 
    state.screen.exits.Count = 0

let prompt_for_name() = prompt "Please enter your name: "

let play_game setup = 
    let screens = setup.screens
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


    let character:Character = {name=prompt_for_name()} ;
    run_frame (initial_state setup character)
    
let load_level_pack () =
    let screen_named name (screens:ScreenList) = 
        let matching = ((List.filter (fun screen -> (screen.name = name)) screens):ScreenList)
        let count = List.length matching
        if (count = 0) then (failwith ("No such screen in set: " + name))
        elif (count > 1) then (failwith ("Multiple screens match name " + name))
        else ((List.head matching):Screen)
    let mx lst =
        Map<Direction, ScreenChange> lst
    let screens = [
        {
            name = "end" ;
            exits = mx [||] ;
            text = "You win!" ;
        };
        {
            name = "start" ;
            exits = mx [|(Direction.North, screen_named "corridor")|] ;
            text = "You are in a room. To the north there is an open door..." ;
        };
        {
            name = "corridor" ;
            exits = mx [|(Direction.South, screen_named "start");(Direction.North, screen_named "end")|] ;
            text = "You are in a corridor running north to south. To the north there is an sign reading 'winning this way'..." ;
        };
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

