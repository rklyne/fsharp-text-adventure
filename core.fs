
namespace textadventure
    module core =
        // Types

        type Direction = 
            | North = 'N'
            | South = 'S'
            | East = 'E'
            | West = 'W'

        type Character = {
            name : string ;
        }

        exception ItemSelectError of string
        type Item = {
            name : string
            is_fixed : bool
            verbs : Map<string,Item->Command>
        }
        and Inventory = (Item list)

        and ScreenChange = ScreenList -> Screen
        and Screen = {
            name: string
            fixed_exits : Map<Direction, ScreenChange>
            text : string
            items : Inventory
            terminal : bool
        }
            with
                member self.exits state = self.fixed_exits

        and ScreenList = Screen list
        and Command = (GameState -> GameState)
        and GameState = {
            screen : Screen
            character : Character
            items : Inventory
            screens : ScreenList
        }

        let empty_inventory = []:Inventory
        let default_item = {
            name = ""
            is_fixed = true
            verbs = Map<string,Item->Command> []
        }



        let rec remove_item_by_name (item:string) inventory =
            match inventory with
            | []    -> []
            | (_item:Item) :: rest -> if _item.name = item then rest else _item :: (remove_item_by_name item rest)
        let remove_item (item:Item) = remove_item_by_name item.name

        let add_item (item:Item) (inventory:Inventory) = (item :: inventory) : Inventory

        let add_verb word command item =
            { item with verbs=item.verbs.Add(word,command) }

        let select_item_from_list (items:Inventory) (name_in:string) : Item =
            let name = name_in.ToLower()
            let none msg = raise (ItemSelectError msg)
            let result x = x
            try
                match [for item in items do if item.name = name then yield item] with
                | []    -> none "no item"
                | [x]   -> result x
                | _     -> none ("many items named " + name)
            with
            | _ ->
                match [for item in items do if item.name.ToLower().Contains(name) then yield item] with
                | []    -> none ("no such item '" + name + "'")
                | [x]   -> result x
                | _     -> none ("too many items match " + name)



        type GameState with
            member self.select_item (name_in:string) : Item =
                select_item_from_list self.items name_in

        type Screen with
            member self.select_item (name_in:string) : Item =
                select_item_from_list self.items name_in

