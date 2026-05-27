type t = Handler.t -> Handler.t

let rec pipeline mws handler =
  match mws with [] -> handler | m :: ms -> m (pipeline ms handler)
