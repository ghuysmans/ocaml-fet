type t = {
  name: string; (* FIXME not empty? *)
  capacity: int;
  building: string; (* FIXME option? *)
}

let header = [
  "Room";
  "Room Capacity";
  "Building";
]

let of_list = function
  | [name; c; building] -> {
    name;
    capacity = int_of_string c;
    building;
  }
  | _ ->
    failwith "Rooms_and_buildings.of_list"

let to_list {name; capacity; building} = [
  name;
  string_of_int capacity;
  building;
]
