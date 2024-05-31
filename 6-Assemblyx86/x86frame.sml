structure X86Frame : FRAME =
struct
    datatype access = InFrame of int | InReg of Temp.temp
    type frame = { name : Temp.label, formals : access list, frameOff : int ref}

    type register = string

    val RV = Temp.newtemp ()
    val FP = Temp.newtemp ()

    val wordSize = 4
    fun name ({name, ...} : frame) = name
    fun formals ({formals, ...} : frame) = formals
    fun allocLocal ({frameOff, ...} : frame) escape =
        case escape
            of true => 
                let val f = InFrame (!frameOff)
                in frameOff := !frameOff - wordSize; f end
             | false => InReg (Temp.newtemp ())
    
    fun spillLoc ({frameOff, ...} : frame) = 
            let val f = !frameOff
            in frameOff := !frameOff - wordSize; f end

    fun newFrame { name, formals } =
        let 
            val off = ref (wordSize * 2)
            (* fun formalsSpace escape =
                (case escape
                    of true =>
                            let val f = InFrame (!off)
                            in off := !off + wordSize; f end
                     | false => InReg (Temp.newtemp ())) *)
            fun formalsSpace escape =
                (let val f = InFrame (!off)
                    in off := !off + wordSize; f end)

            val accs = map formalsSpace formals
        in
        { name = name, formals = accs, frameOff = ref (~wordSize) }
        end

    fun exp (InReg t) _ = Tree.TEMP t 
      | exp (InFrame off) r = Tree.MEM(Tree.BINOP(Tree.PLUS, r, Tree.CONST(off)))

    fun printAccess (InFrame w) = ("(InFrame " ^ Int.toString w ^ ")")
      | printAccess (InReg _) = "InReg"

    fun replace old new s = String.translate (fn c => if c = old then new else Char.toString c) s

    fun string (lab, str) =
        let
            val str' = replace #"\"" "\\\"" str (* replace #"\"" "\\\"" (replace #"\n" "\n" str) *)
        in
        Symbol.name lab ^ ":"
        ^ "\n.long " ^ Int.toString (String.size str)
        ^ "\n.ascii \"" ^ str' ^ "\"\n"
        end

    fun prologue ({name, frameOff, ...} : frame) =
                         Symbol.name name ^ ":\n"
                       ^ "PUSH %ebp\n"
                       ^ "MOV %esp, %ebp\n"
                       ^ "ADD $-" ^ Int.toString(~(!frameOff + wordSize)) ^ ", %esp\n"
                       ^ "PUSH %ebx\nPUSH %edi\nPUSH %esi\n"
    fun epilogue ({frameOff, ...} : frame) =
                         "POP %esi\nPOP %edi\nPOP %ebx\n"
                       ^ "ADD $" ^ Int.toString(~(!frameOff + wordSize)) ^ ", %esp\n"
                       ^ "POP %ebp"
                       ^ "\nRET\n"
                       
end