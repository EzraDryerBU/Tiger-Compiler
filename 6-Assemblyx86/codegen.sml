structure CodeGen =
struct
  structure T = Tree
  structure A = Assem
   structure F = X86Frame

  fun codegen (frame : F.frame) (stm: T.stm) : (Temp.temp A.instr) list =
    let
        val ilist = ref (nil : (Temp.temp A.instr) list)
        fun emit x = ilist := x :: !ilist
        fun result(gen) = let val t = Temp.newtemp() in  gen t; t end

        fun intString i = if i >= 0 then Int.toString i else "-" ^ Int.toString (~i)

        fun cmp T.EQ = "je"
          | cmp T.NE = "jne"
          | cmp T.LT = "jl"
          | cmp T.GT = "jg"
          | cmp T.LE = "jle"
          | cmp T.GE = "jge"            
        
        and munchStm (T.LABEL l) = emit(A.LABEL {assem = Symbol.name l ^ ":\n", lab = l})
          | munchStm (T.JUMP (T.NAME l, ls)) =
            emit (A.JUMP { assem = "JMP `j0\n", jump = [l] })
          | munchStm (T.CJUMP (relop, e1, e2, l1, l2)) =
            (emit (A.OPER { assem = "CMP `s1, `s0\n", dst = [], src = [munchExp e1, munchExp e2] });
             emit (A.JUMP { assem = cmp relop ^ " `j0\n",
                            jump = [l1, l2] }))
          | munchStm (T.MOVE (T.TEMP t1, T.TEMP t2)) = 
            emit (A.OPER { assem = "MOV `s0, `d0\n", dst = [t1], src = [t2] })
          | munchStm (T.MOVE (T.TEMP t1, e2)) = 
            emit (A.OPER { assem = "MOV `s0, `d0\n", dst = [t1], src = [munchExp e2] })
          | munchStm (T.MOVE (T.MEM e1, T.NAME c)) = 
            emit (A.OPER { assem = "MOVL $" ^ Symbol.name c ^ ", (`d0)\n", dst = [munchExp e1], src = [] })
          | munchStm (T.MOVE (T.MEM e1, e2)) = 
            emit (A.OPER { assem = "MOV `s0, (`d0)\n", dst = [munchExp e1], src = [munchExp e2] })
          | munchStm (T.MOVE (e1, e2)) = 
            emit (A.OPER { assem = "MOV `s0, `d0\n", dst = [munchExp e1], src = [munchExp e2] })
          | munchStm (T.EXP e) = (munchExp e; ())


        and munchBinop (oper : string) e1 e2 =
            result (fn r => 
                (emit (A.OPER
                { assem = "MOV `s0, `d0\n"
                , src = [munchExp e1], dst = [r]});
                emit (A.OPER
                { assem = oper ^ " `s1, `d0\n"
                , src = [r, munchExp e2], dst = [r]})))
        
        and emitPush (T.NAME c) = emit (A.OPER { assem = "PUSH $" ^ Symbol.name c ^ "\n", dst = [], src = [] })
          | emitPush (T.CONST c) = emit (A.OPER { assem = "PUSH $" ^ intString c ^ "\n", dst = [], src = [] })
          | emitPush e = emit (A.OPER { assem = "PUSH `s0\n", dst = [], src = [munchExp e] })

        and munchExp (T.MEM e) = result (fn r => emit (A.OPER
                { assem = "MOV (`s0), `d0\n"
                , src = [munchExp e], dst = [r]}))
          | munchExp (T.BINOP(T.PLUS, e1, e2)) = munchBinop "ADD" e1 e2
          | munchExp (T.BINOP(T.MINUS, e1, e2)) = munchBinop "SUB" e1 e2
          | munchExp (T.BINOP(T.MUL, e1, e2)) = munchBinop "IMUL" e1 e2
          | munchExp (T.BINOP(T.DIV, e1, e2)) =
                (
                (* emit (A.OPER
                { assem = "MOV $0, %edx\n"
                , src = [], dst = []}); *)
                emit (A.OPER
                { assem = "MOV `s0, `d0\n"
                , src = [munchExp e1], dst = [F.RV]});
                emit (A.OPER
                { assem = "CDQ\n"
                , src = [], dst = []});
                emit (A.OPER
                { assem = "IDIV `s0\n"
                , src = [munchExp e2], dst = [F.RV]});
                F.RV
                )

          | munchExp (T.CONST i) =
            result  (fn r => emit (A.OPER
                { assem = "MOV $" ^ intString i ^ ", `d0\n"
                , src = [], dst = [r]}))
          | munchExp (T.TEMP t) = t
          | munchExp (T.CALL(T.NAME c, es)) =
              (emit (A.OPER { assem = "PUSH %ecx\n", dst = [], src = [] });
               emit (A.OPER { assem = "PUSH %edx\n", dst = [], src = [] });
               app emitPush (rev es);
               emit (A.OPER { assem = "CALL " ^ Symbol.name c ^ "\n", dst = [], src = [] });
               emit (A.OPER { assem = "ADD $" ^ intString (F.wordSize * length es) ^ ", %esp\n", dst = [], src = [] });
               emit (A.OPER { assem = "POP %edx\n", dst = [], src = [] });
               emit (A.OPER { assem = "POP %ecx\n", dst = [], src = [] });
               X86Frame.RV)
          | munchExp (T.NAME c) =
                result (fn r => emit (A.OPER { assem = "MOV $" ^Symbol.name c ^ ", `d0\n", dst = [r], src = [] }))
          | munchExp e = (Printtree.printtree(TextIO.stdOut, T.EXP e);raise ErrorMsg.Error)
    in
    munchStm stm; rev (!ilist)
    end
end