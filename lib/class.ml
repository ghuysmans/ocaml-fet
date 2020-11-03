module Year = struct
  type t = [`Year] No_plus.t
  let of_string = No_plus.of_string
end

module Group = struct
  type t = [`Group] No_plus.t
  let of_string = No_plus.of_string
end

module Subgroup = struct
  type t = [`Subgroup] No_plus.t
  let of_string = No_plus.of_string
end

type t = [`Year | `Group | `Subgroup] No_plus.t
let of_string = No_plus.of_string
