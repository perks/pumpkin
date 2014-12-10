(* Scanning Exception *)
exception IllegalCharacter of char * int
exception IndentationError of int
exception UnmatchedQuotation of int
