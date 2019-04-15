type command = 
  | Hit
  | Check

exception Malformed

let parse str = 
  if str = "hit" then Hit 
  else if str = "check" then Check
  else raise Malformed