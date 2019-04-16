module type GameSig = sig

end

module GameCheck : GameSig = Game

module type CommandSig = sig

end

module CommandCheck : CommandSig = Command

module type StateSig = sig

end

module StateCheck : StateSig = State

module type AuthorsSig = sig
  val hours_worked : int list
end

module AuthorsCheck : AuthorsSig = Authors
