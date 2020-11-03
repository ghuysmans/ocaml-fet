open Utils

type t = {
  name: Room.t;
  capacity: int;
  building: Building.t option;
}

let header = [|
  "Room";
  "Room Capacity";
  "Building";
|]

let of_array = function
  | [| r; c; b |] -> {
    name = Room.of_string r;
    capacity = int_of_string c;
    building = option_of_csv Building.of_string b
  }
  | _ ->
    failwith "Rooms_and_buildings.of_array"

let to_array {name; capacity; building} = [|
  No_plus.to_string name;
  string_of_int capacity;
  csv_of_option No_plus.to_string building
|]
