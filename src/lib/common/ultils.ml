let fOption (f : 'a -> 'b) (d : 'b) (o : 'a option) =
(* let stringOption fString s t = *)
  match o with
  | Some v -> f v
  | None   -> d

