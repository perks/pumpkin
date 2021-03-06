(* Processor Exception*)
exception MissingEOF

(* Scanning Exception *)
exception IllegalCharacter of char * int
exception IndentationError of int
exception UnmatchedQuotation of int
exception IllegalToken of string

(* Analyzer Exception *)
exception NameCollision of string
exception TypeNotFound of string
exception IDNotFound of string
exception TypeMismatch
exception InvalidOperation of string
exception UnimplementedOperation of string * string
exception InvalidIndexing of string
exception ArrayOutOfBounds
exception IfRequiresBool of string
exception WrongParameterType of string
exception ComposedIntermediateTakesMultipleArguments
exception InvalidMapKeyType
exception UnimplementedCallType of int
exception PipingIntoNonFunc
exception InvalidWildcard

(* Compiler exception!! *)
exception ReservedFuncTypeMisMatch
