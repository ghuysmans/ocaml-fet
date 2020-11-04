open Utils

type t = {
  name: string;
  capacity: int;
  building: Building.t option;
}

let header = [
  "Room";
  "Room Capacity";
  "Building";
]

let of_list = function
  | [name; c; b] -> {
    name;
    capacity = int_of_string c;
    building = option_of_csv Building.of_string b
  }
  | _ ->
    failwith "Rooms_and_buildings.of_list"

let to_list {name; capacity; building} = [
  name;
  string_of_int capacity;
  csv_of_option No_plus.to_string building
]
