signature FRAME =
sig
    type frame
    type access
    type register = string

    val wordSize : int
    val newFrame : { name : Temp.label, formals : bool list } -> frame
    val name : frame -> Temp.label
    val formals : frame -> access list
    val allocLocal : frame -> bool -> access
    val spillLoc : frame -> int

    val RV : Temp.temp
    val FP : Temp.temp
    val exp : access -> Tree.exp -> Tree.exp

    val printAccess : access -> string

    val string : Temp.label * string -> string

    val prologue : frame -> string
    val epilogue : frame -> string
end