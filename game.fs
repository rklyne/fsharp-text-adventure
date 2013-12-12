
namespace textadventure
    module main = 
        open System
        open core
        open FParsec

        // Types

        let empty_inventory = empty_inventory

        type GameSetup = {
            screens : ScreenList ;
            starting_screen : Screen ;
        }

        let room_named name = {
            name = name
            text = ""
            fixed_exits = Map<Direction, ScreenChange> [||]
            items = []
            terminal = false
        }

        let terminal_room room = {room with terminal = true}

        // Inventory functions
        //

        let add_item_to_screen (item:Item) (screen:Screen) : Screen =
            {screen with items = add_item item screen.items}

        let can_collect_item (item:Item) =
            not item.is_fixed

        // Screen helper functions
        //

        let update_screen (screen_mod:Screen->Screen) (state:GameState) : GameState =
            { state with
                screen = screen_mod state.screen
                screens = List.map (fun (screen:Screen) ->
                    if screen.name = state.screen.name then
                        screen_mod screen
                    else
                        screen
                    )
                    (state.screens:ScreenList)
            }

        // Game actions
        //

        let print_message (message: string) state =
            Console.WriteLine message
            state

        let display_inventory state =
            print_message ("you have " + String.Join(", ", [for item in state.items -> item.name])) state

        let move direction state =
            let exits = state.screen.exits state
            if not (exits.ContainsKey direction) then print_message "Illegal move" state
            else
                { state with
                    screen = (exits.Item direction) state.screens
                }

        let help_text = "Use n,e,w,s characters to move. 'quit' to quit"

        let quit_game state = 
            { state with screen = terminal_room (room_named "END") }

        let drop_object (item_name:string) (state:GameState) =
            let item = (state.select_item item_name) : Item
            let screen_change screen =
                { screen with Screen.items = add_item item screen.items }
            (update_screen screen_change 
                { state with
                    items = ((remove_item item state.items):Inventory)
                })

        let pick_up_object item_name (state:GameState) =
            try
                let item = state.screen.select_item item_name : Item 
                if can_collect_item item then
                    let screen_change screen =
                        { screen with Screen.items = remove_item item screen.items }
                    (update_screen screen_change
                        {state with GameState.items = add_item item state.items})
                else
                    print_message "You cannot pick that up." state
            with
                | ItemSelectError msg -> print_message (String.Format("There is no '{0}' here...", item_name)) state

        let perform_intransitive_verb_action verb subject_name with_item_name (state:GameState) =
            let subject = state.screen.select_item subject_name
            let with_item = state.select_item with_item_name
            (subject.verbs.Item verb) with_item state

        // Parsers
        //

        let _ws = many (anyOf "\t ")
        let ws = many1 (anyOf "\t ")
        let pstringws x = _ws >>. (pstringCI x)
        let word_chars = String.Join("", ['a' .. 'z']) + String.Join("", ['A' .. 'Z'])
        let any_word = ws >>. many1Chars (anyOf word_chars)
        let rec phrase words =
            match words with
                | [] -> failwith "This phrase parser requires the words in the phrase"
                | [wd] -> pstringws wd
                | wd::wds -> pstringws wd .>> (ws >>. (phrase wds))
        let pick_up = (pstringws "take" <|> phrase ["pick";"up"]) >>. any_word |>> (fun item -> pick_up_object item)
        let put_down = (pstringws "drop" <|> phrase ["put";"down"]) >>. any_word |>> (fun item -> drop_object item)

        let parse_exit =
            pstringws "exit" <|> pstringws "quit" |>> (fun _ -> quit_game)
        let parse_help =
            pstringws "help" <|> pstringws "?" |>> (fun _ -> print_message help_text)
        let parse_show_items =
            (pstringws "inventory" <|> pstringws "items") |>> (fun _ -> display_inventory)
        let parse_action =
            pick_up <|> put_down <|> parse_show_items <|> parse_help

        let intransitive_verb_words = ["open"; "close"; "push"]
        let parse_item_verb = 
            let (h::t) = List.map pstringws intransitive_verb_words
            (List.fold (fun l r -> l <|> r) h t) .>>. any_word .>> pstringws "with" .>>. any_word |>> (fun ((verb, subject), with_item) -> perform_intransitive_verb_action verb subject with_item)

        let parse_movement = 
                (pstringws "n" <|> pstringws "north" |>> (fun _ -> (print_message "north..." >> move Direction.North)))
            <|> (pstringws "e" <|> pstringws "east" |>> (fun _ -> (print_message "east..."  >> move Direction.East)))
            <|> (pstringws "s" <|> pstringws "south" |>> (fun _ -> (print_message "south..." >> move Direction.South)))
            <|> (pstringws "w" <|> pstringws "west" |>> (fun _ -> (print_message "west..."  >> move Direction.West)))

        let parse_game_command = parse_exit <|> parse_action <|> parse_movement <|> parse_item_verb

        // IO helpers
        //
        let prompt (message:string) = 
            Console.Write message
            Console.ReadLine()

        let read_user_command state =
            let parse_command text =
                if text = null then
                    quit_game
                else
                    match run parse_game_command text with
                        | Success(cmd, _, _) -> cmd
                        | Failure(msg, _, _) -> print_message ("Bad command: \r\n" + (msg.Replace("(case-insensitive)", "")))
            let input = prompt (state.character.name + "> ")
            parse_command input

        let display_frame state =
            Console.WriteLine("")
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
            state.screen.terminal

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
                fixed_exits = screen.fixed_exits.Add (direction, (get_screen_named name)) }

        let new_item name =
            if name = "" then
                failwith "Name is empty"
            else 
                { default_item with
                    Item.name = name
                    Item.is_fixed = true
                }

        let collectible item =
            {item with Item.is_fixed = false}

        let new_locked_door (key:Item) direction exit = 
            let command (k:Item) =
                if k.name = key.name then (update_screen (with_named_exit direction exit))
                else print_message "You need the key."
            new_item "door" |> add_verb "open" command
        let with_locked_exit key direction exit (screen:Screen) =
            (add_item_to_screen (new_locked_door key direction exit) screen) : Screen


        let load_level_pack () =
            let screen_named = get_screen_named
            let with_text = room_described
            let with_item = add_item_to_screen
            // let collectible = with_tverb "pick up" pick_up_object
            let key1 = (new_item "Front door key" |> collectible):Item
            let with_key1_exit direction screen_name = with_locked_exit key1 direction screen_name
            let screens = [
                room_named "start" |> with_text "You are in a room. To the north there is an open door..." |> with_named_exit Direction.North "corridor" |> with_item key1
                room_named "corridor" |> with_text "You are in a corridor running north to south. To the north there is an sign reading 'winning this way'..." |> with_named_exit Direction.South "start" |> with_named_exit Direction.North "door_room"
                room_named "door_room" |> with_text "The front door is here" |> (with_key1_exit Direction.North "end")
                room_named "end" |> with_text "You win!" |> terminal_room
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

