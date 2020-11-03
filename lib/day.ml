type t =
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

let of_string = function
  | "Monday" -> Monday
  | "Tuesday" -> Tuesday
  | "Wednesday" -> Wednesday
  | "Thursday" -> Thursday
  | "Friday" -> Friday
  | "Saturday" -> Saturday
  | "Sunday" -> Sunday
  | _ -> failwith "Day.of_string"

let to_string = function
  | Monday -> "Monday"
  | Tuesday -> "Tuesday"
  | Wednesday -> "Wednesday"
  | Thursday -> "Thursday"
  | Friday -> "Friday"
  | Saturday -> "Saturday"
  | Sunday -> "Sunday"
