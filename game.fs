
module text_adventure

open System

type Direction = 
    | North = 'N'
    | South = 'S'
    | East = 'E'
    | West = 'W'

type Screen = {
    exits : (Direction*Screen)[] ;
    text : string ;
}

type Character = {
    name : string ;
}

type GameSetup = {
    screens : Screen[] ;
    starting_screen : Screen ;
}

type GameState = {
    setup : GameSetup ;
    screen : Screen ;
    character : Character ;
}

// Game actions

let print_message message state =
    Console.WriteLine (message:string)
    state

let move direction state =
    let exits = (Array.filter (fun x -> direction = fst x) state.screen.exits)
    if not ((Array.length exits) = 1) then print_message "Illegal move" state
    else
        { state with
            screen = snd exits.[0] ;
        }

let help_text = "Use n,e,w,s characters to move. "


let read_user_command state =
    let parse_command text = 
        match text with
            | "n" | "N" -> print_message "north..." >> move Direction.North
            | "e" | "E" -> print_message "east..."  >> move Direction.East
            | "s" | "S" -> print_message "south..." >> move Direction.South
            | "w" | "W" -> print_message "west..."  >> move Direction.West
            | "?"       -> print_message help_text
            | _ -> print_message ("Bad command: " + text) >> print_message "'?' for help"
    Console.WriteLine(state.character.name + "> ")
    let input = Console.ReadLine()
    parse_command input

let display_frame state =
    Console.WriteLine(state.screen.text)

let initial_state setup character = {
    setup = setup ;
    screen = setup.starting_screen ;
    character = character ;
}

let game_over_condition setup state = 
    (Array.length state.screen.exits) = 0

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


    let character:Character = {name="Salvador Dali"};
    run_frame (initial_state setup character)
    
let load_level_pack () =
    let s2 = {
        exits = [||] ;
        text = "You win!" ;
    }
    let s1 = {
        exits = [|(Direction.North, s2)|] ;
        text = "You are in a room. To the north there is an open door..." ;
    }
    {
        starting_screen = s1;
        screens = [|s1;s2|];
    }:GameSetup
    
[<EntryPoint>]
let main args =
    let level_pack = load_level_pack()
    play_game level_pack
    0

