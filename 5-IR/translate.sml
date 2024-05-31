structure F = X86Frame

structure Translate : TRANSLATE =
struct

    datatype level = Outermost | Level of {prev_level : level, frame : X86Frame.frame, eqc : unit ref}
    type access = level * X86Frame.access

    val outermost = Outermost
    fun newEqc () = (
      let
        val ret : unit ref = ref ()
      in
        ret
      end
    )
    fun newLevel {parent, name, formals} = (
      let
        val nf = F.newFrame {name=name, formals=true::formals}
      in
        Level({prev_level=parent, frame=nf, eqc=newEqc()})
      end

    )
    fun formals lev = (
      case lev of
        Outermost => ([])
        | Level({prev_level, frame, eqc}) => (
          let
            val forms = F.formals frame
            fun mapHelper fAccess = (
              (lev, fAccess)
            )
          in
            map mapHelper forms
          end
        ) 
    )
    fun allocLocal lev b = (
      case lev of
        Outermost => (lev, F.allocLocal (F.newFrame{name=Temp.newlabel(), formals=[]}) b)
        | Level({prev_level, frame, eqc}) => (
           (lev, F.allocLocal frame b)
      )
    )

    fun printAccess_ (_, acc) = X86Frame.printAccess acc
    fun printAccess n loc = print("var " ^ Symbol.name n ^ " " ^ printAccess_ loc ^ "\n")
    fun printLevel n l =
      print("function " ^ Symbol.name n ^ " "
            ^ (String.concatWith " " (map printAccess_ (formals l))) ^ "\n")


(*********************************************************************)
    datatype exp = Ex of Tree.exp
                | Stm of Tree.stm
                | Cond of Temp.label * Temp.label -> Tree.stm 

    datatype frag = PROC of { body : Tree.stm, frame : X86Frame.frame }
                  | STRING of Temp.label * string

    val frags : frag list ref = ref []
    fun printFrags_ ([] : frag list) : unit = ()
      | printFrags_ (f :: fs : frag list) : unit =
        ((
          case f
            of PROC {body, frame={name, formals, frameOff}} => (print(Symbol.name name ^ " = " ); Printtree.printtree(TextIO.stdOut, body))
             | STRING(lbl, s) => (print(Symbol.name lbl); print(" = "); print(s); print("\n"))
         );
        printFrags_ fs)
    fun printFrags () : unit = printFrags_ (!frags)

    (*seq : Tree.stm list -> Tree.stm *)       
    fun seq [] = Tree.EXP (Tree.CONST 0)
      | seq [s] = s
      | seq (s::rest) = Tree.SEQ (s, seq rest)
            
    fun toEx (Ex e) = e
      | toEx (Stm s) = Tree.ESEQ(s, Tree.CONST 0)
      | toEx (Cond cond) =
        let
          val r = Temp.newtemp ()
          val t = Temp.newlabel ()
          val f = Temp.newlabel ()
        in
          Tree.ESEQ(seq[ Tree.MOVE(Tree.TEMP r, Tree.CONST 1),
          cond(t, f),
          Tree.LABEL f,
          Tree.MOVE(Tree.TEMP r, Tree.CONST 0),
          Tree.LABEL t],
          Tree.TEMP r)
        end


    fun toStm (Stm s) = s
      | toStm (Ex e) = Tree.EXP(e)
      | toStm (cond) = Tree.EXP(toEx cond)
    
    fun toCond (Cond c) = c
      | toCond (Ex e) = 
        let
          fun ret (l1, l2) = (
            case e of
              Tree.CONST 0 => (Tree.LABEL(l2))
              | _ => (
                Tree.CJUMP(Tree.EQ, Tree.CONST(0), e, l2, l1)
              )
          )
        in
          ret
        end
      | toCond (Stm s) = (print "You should not be here"; raise ErrorMsg.Error)

    fun procEntryExit { level = Level {frame, ...}, body } =
          frags := PROC { body = Tree.MOVE(Tree.TEMP X86Frame.RV, toEx body), frame = frame } :: (!frags)
        | procEntryExit { level = Outermost, ...} = (print "procEntryExit: impossible"; raise ErrorMsg.Error)

    
    fun recFindVar (decLevel) (useLevel) = (
      case (decLevel = useLevel) of
        true => (Tree.TEMP(X86Frame.FP))
        | false => (
            case useLevel of
              Level({prev_level=pl, frame, eqc}) => (
                let
                  val ret = recFindVar (decLevel) (pl)
                in
                  Tree.MEM(ret)
                end
              )
              | Outermost => (print "You should not be here"; raise ErrorMsg.Error)   
          )
    )

    (*val simpleVar : access * level -> exp*)
    fun simpleVar (acc as (accLevel, x86acc), useLev) = (
        let
          val memSeq = recFindVar (accLevel) (useLev)
        in
          Ex(X86Frame.exp (x86acc) (memSeq))
        end
    )

    (* val fieldVar : exp * int -> exp *)
    fun fieldVar(e,i) = (
      let
        val tExp = toEx(e)
        val wSize = Tree.CONST(F.wordSize)
        val offset = Tree.BINOP(Tree.MUL, Tree.CONST(i), wSize)
        val ret = Tree.MEM(Tree.BINOP(Tree.PLUS, tExp, offset))
      in
        Ex(ret)
      end
    )

    (* val subscriptVar : exp * exp -> exp *)
    fun subscriptVar (e1, e2) = (
      let
        val treeExp1 = toEx(e1)
        val treeExp2 = toEx(e2)
        val wSize = Tree.CONST(F.wordSize)
        val offset = Tree.BINOP(Tree.MUL, treeExp2, wSize)
        val ret = Tree.MEM(Tree.BINOP (Tree.PLUS, treeExp1, offset))
      in
        Ex(ret)
      end
    )

    (*val nilExp : exp*)
    val nilExp = Ex(Tree.CONST 0)

    (*val intExp : int -> exp*)
    fun intExp i = Ex(Tree.CONST(i))

    (*val stringExp : string -> exp*)
    fun stringExp s = 
      let
        val newL = Temp.newlabel()
        val newF = STRING(newL, s)
      in
        frags := (newF::(!frags));
        Ex(Tree.NAME(newL))
      end

    (*val callExp : level * level * Temp.label * exp list -> exp*)
    fun callExp(useLevel, funcLevel as Level({prev_level, frame, eqc}), lab, expList) = (
      let
        val tExpList = map toEx expList
        val staticLink = recFindVar (prev_level) (useLevel)
      in

        Ex(Tree.CALL(Tree.NAME(lab), staticLink::tExpList))
      end
    )
    | callExp(useLevel, Outermost, lab, expList) = (
      let
        val tExpList = map toEx expList
      in
        Ex(Tree.CALL(Tree.NAME(lab), tExpList))
      end
    )

    (* val opExp : exp * Absyn.oper * exp * Types.ty -> exp *)
    fun opExp (exp1, oper, exp2, t) = (
      let
        fun genericRet (operation) = (
          let
            fun ret (l1, l2) = (
              Tree.CJUMP(operation, toEx(exp1), toEx(exp2), l1, l2)
            ) 
          in
            ret
          end
        )
      in
        case oper of 
        A.PlusOp => (Ex(Tree.BINOP(Tree.PLUS, toEx(exp1), toEx(exp2))))
        | A.MinusOp => (Ex(Tree.BINOP(Tree.MINUS, toEx(exp1), toEx(exp2))))
        | A.TimesOp => (Ex(Tree.BINOP(Tree.MUL, toEx(exp1), toEx(exp2))))
        | A.DivideOp => (Ex(Tree.BINOP(Tree.DIV, toEx(exp1), toEx(exp2))))
        | A.EqOp => (
            case t of 
              Types.STRING => (Ex(Tree.CALL(Tree.NAME(Temp.namedlabel("stringEqual")), [toEx(exp1), toEx(exp2)])))
              | _ => (Cond(genericRet(Tree.EQ)))
        )
        | A.NeqOp => (
            case t of 
              Types.STRING => (
                let
                  val ret = (Tree.CALL(Tree.NAME(Temp.namedlabel("stringEqual")), [toEx(exp1), toEx(exp2)]))
                in
                  Cond(fn (t, f) => Tree.CJUMP(Tree.EQ, ret, Tree.CONST(1), f, t))
                end
              )
              | _ => (Cond(genericRet(Tree.NE)))
        )   
        | A.LtOp => (Cond(genericRet(Tree.LT)))
        | A.LeOp => (Cond(genericRet(Tree.LE)))
        | A.GtOp => (Cond(genericRet(Tree.GT)))
        | A.GeOp => (Cond(genericRet(Tree.GE)))
      end        
    )

    (* val assignExp : exp * exp -> exp *)
    fun assignExp (e1, e2) = (
      let
        val varExp = toEx(e1)
        val initExp = toEx(e2) 
        val ret = Tree.MOVE(varExp, initExp)
      in
        Stm(ret)
      end
    )
    
    (* val recordExp : exp list -> exp *)
    fun recordExp (exps : exp list) : exp = (
      let
        val recTemp = Temp.newtemp()
        val allocExp = (Tree.CALL(Tree.NAME(Temp.namedlabel("allocRecord")), [Tree.CONST(length(exps))])) 
        val recInitExp = Tree.MOVE(Tree.TEMP recTemp, (allocExp));
        fun assignFieldExps ([] : exp list) (_ : int) : Tree.stm list = []
          | assignFieldExps (e :: exps : exp list) (i : int) : Tree.stm list =
            (toStm(assignExp((Ex(Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.TEMP recTemp, Tree.CONST(i * X86Frame.wordSize))))), e))
            :: (assignFieldExps exps (i+1)))
        val fieldExps = (assignFieldExps exps 0)
      in
        Ex(Tree.ESEQ(seq (recInitExp :: fieldExps), Tree.TEMP recTemp))
      end
    )

    (* val seqExp : exp list -> exp *)
    (*Need the last thing to be an expression*)
    fun seqExp expList = (
      let
        val treeExps = map toEx expList
        fun toExpList [] = Tree.CONST 0
        | toExpList [e] = e
        | toExpList (e::rest) = (
          Tree.ESEQ(Tree.EXP(e), toExpList(rest))
        )
      in
        Ex(toExpList(treeExps))
      end
    )

    (*ifExp : exp * exp * exp option -> Tree.exp*)
    fun ifExp (test, thenExp, SOME elseExp) = (
      let
        val trueBranch = Temp.newlabel ()
        val falseBranch = Temp.newlabel ()
        val j = Temp.newlabel ()
        val eThen = toEx thenExp
        val eElse = toEx elseExp
        val res = Temp.newtemp ()

        val cTest = toCond test

        val s = seq [cTest(trueBranch, falseBranch), 
                    Tree.LABEL trueBranch,
                    Tree.MOVE(Tree.TEMP res, eThen), 
                    Tree.JUMP (Tree.NAME j, [j]),
                    Tree.LABEL falseBranch,
                    Tree.MOVE(Tree.TEMP res, eElse),
                    Tree.LABEL(j)]

      in
        Ex (Tree.ESEQ(s, Tree.TEMP(res)))
      end
    )
    | ifExp (test, thenExp, NONE) = (
      let
        val trueBranch = Temp.newlabel ()
        val falseBranch = Temp.newlabel ()
        val eThen = toEx thenExp
        val cTest = toCond test

        val s = seq [cTest(trueBranch, falseBranch), 
                    Tree.LABEL trueBranch,
                    Tree.EXP eThen, 
                    Tree.LABEL falseBranch]

      in
        Stm s
      end
    )

    (*exit label?, cond, body*)
    (* val whileExp : Temp.label * exp * exp -> exp *)
    fun whileExp (done, test, body) = (
      (*test:
          if not(condition) goto done
            body
            goto test
        done *)
        let
          val start = Temp.newlabel()
          val main = Temp.newlabel()
          val bodyStm = toStm(body)
          val ctest = toCond(test)
          val ret = seq([
            Tree.LABEL(start),
            ctest(main, done),
            Tree.LABEL(main),
            bodyStm,
            Tree.JUMP(Tree.NAME(start), [start]),
            Tree.LABEL(done)
          ])
        in
          Stm(ret)
        end
    )

    (*exit label?, var/init, lo, hi, body*)
    (* val forExp : Temp.label * exp * exp * exp * exp -> exp *)
    fun forExp(done, init, lo, hi, body) = (
      (*let var i := lo
          var limit := hi
        in while i <= limit
          do (body; i := i + 1)
        end*)

      let
        val limit = Temp.newtemp()
        val start = Temp.newlabel()
        val main = Temp.newlabel()
        (* val end = Temp.newlabel() USE done*)
        val ret = seq ([
          Tree.MOVE(toEx(init), toEx(lo)),
          Tree.MOVE(Tree.TEMP(limit), toEx(hi)),
          Tree.LABEL(start),
          Tree.CJUMP(Tree.LE, toEx(init), Tree.TEMP(limit), main, done),
          Tree.LABEL(main),
          toStm(body),
          Tree.MOVE(toEx(init), Tree.BINOP(Tree.PLUS, toEx(init), Tree.CONST(1))),
          Tree.JUMP(Tree.NAME(start), [start]),
          Tree.LABEL(done)
        ])
      in
        Stm(ret)
      end
    )

    (* val breakExp : Temp.label -> exp *)
    fun breakExp lab = (
      Stm(Tree.JUMP(Tree.NAME(lab), [lab]))
    )
    
    (* val letExp : exp list * exp -> exp *)
    fun letExp (expList, e) = (
      let
        val treeStmList = map toStm expList
        val stms = seq(treeStmList)
      in
        Ex(Tree.ESEQ(stms, toEx(e)))
      end
    )

    (* val arrayExp : exp * exp -> exp  *)
    (*Maybe take out last line of let block and replace with callExp for ret*)
    fun arrayExp(e1, e2) = (
      let
       val sizeExp = toEx(e1)
       val initExp = toEx(e2)
       val arrayTemp = Temp.newtemp()
       val callExp = (Tree.CALL(Tree.NAME(Temp.namedlabel("initArray")), [sizeExp, initExp]))
       (* val arrayInitExp = Tree.MOVE(Tree.TEMP(arrayTemp), (callExp)) *)
      in
        Ex(callExp)
        (* Ex(Tree.ESEQ(arrayInitExp, Tree.TEMP arrayTemp)) *)
      end
    )

end


