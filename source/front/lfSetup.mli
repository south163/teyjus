(** Sets up Teyjus for solving queries by:
      - translating the given signature
      - emitting the translated sig & mod files
      - compile & link the translated files
      - setup the simulator state **)
open Front

val setup_system : string ref -> int ref -> Lfsig.signature -> string ref -> (Absyn.amodule * Metadata.metadata)
