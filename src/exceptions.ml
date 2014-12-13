(* Processor Exception*)
exception MissingEOF

(* Scanning Exception *)
exception IllegalCharacter of char * int
exception IndentationError of int
exception UnmatchedQuotation of int

(* Analyzer Exception *)
exception NameCollision of string
exception TypeNotFound of string
exception IDNotFound of string
exception TypeMismatch