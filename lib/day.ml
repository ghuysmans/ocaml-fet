type t =
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

let of_string s =
  match String.lowercase_ascii s with
  | "monday" | "lundi" -> Monday
  | "tuesday" | "mardi" -> Tuesday
  | "wednesday" | "mercredi" -> Wednesday
  | "thursday" | "jeudi" -> Thursday
  | "friday" | "vendredi" -> Friday
  | "saturday" | "samedi" -> Saturday
  | "sunday" | "dimanche" -> Friday
  | _ -> failwith "Day.of_string"

let to_string = function
  | Monday -> "Monday"
  | Tuesday -> "Tuesday"
  | Wednesday -> "Wednesday"
  | Thursday -> "Thursday"
  | Friday -> "Friday"
  | Saturday -> "Saturday"
  | Sunday -> "Sunday"
