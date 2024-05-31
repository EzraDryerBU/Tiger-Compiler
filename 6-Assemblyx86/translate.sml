
structure Translate : TRANSLATE =
struct
    structure A = Absyn
    structure T = Tree

    datatype level = Outermost | Level of { prev_level : level, frame : X86Frame.frame, eqc : unit ref  }
    type access = level * X86Frame.access

    datatype frag = PROC of { body : Tree.stm, frame : X86Frame.frame }
                  | STRING of Temp.label * string

    val frags : frag list ref = ref []

    val outermost = Outermost
    fun newLevel { parent : level, name : Temp.label, formals : bool list} =
        Level { prev_level = parent, frame = X86Frame.newFrame { name = name, formals = true :: formals }, eqc = ref () }  
    fun formals (l as Level {frame, ...}) = map (fn frml => (l, frml)) (X86Frame.formals frame)
      | formals Outermost = []
    fun allocLocal (level as Level {frame, ...}) escape = (level, X86Frame.allocLocal frame escape)
      | allocLocal Outermost _ = (print "allocLocal: impossible"; raise ErrorMsg.Error)

    fun printAccess_ (_, acc) = X86Frame.printAccess acc
    fun printAccess n loc = print("var " ^ Symbol.name n ^ " " ^ printAccess_ loc ^ "\n")
    fun printLevel n (l as Level {frame, ...}) =
      print("function " ^ Symbol.name n ^ " " ^ Symbol.name (X86Frame.name frame) ^ " "
            ^ (String.concatWith " " (map printAccess_ (formals l))) ^ "\n")
    
    datatype exp = Ex of Tree.exp
                 | Stm of Tree.stm
                 | Cond of Temp.label * Temp.label -> Tree.stm
    
    fun seq [] = T.EXP (T.CONST 0)
      | seq [s] = s
      | seq ((s :: stms) : T.stm list) = T.SEQ (s, seq stms)

    fun toEx (Ex e) = e
      | toEx (Cond cond) =
        let
          val r = Temp.newtemp ()
          val t = Temp.newlabel ()
          val f = Temp.newlabel ()
        in
          T.ESEQ(seq[ T.MOVE(T.TEMP r, T.CONST 1),
                      cond(t, f),
                      T.LABEL f,
                      T.MOVE(T.TEMP r, T.CONST 0),
                      T.LABEL t], 
                 T.TEMP r)
        end
      | toEx (Stm s) = T.ESEQ(s, T.CONST 0)

    fun toStm (Ex e) = T.EXP e
      | toStm (Cond cond) = T.EXP (toEx (Cond cond)) 
      | toStm (Stm s) = s

    fun toCond (Cond c) = c
      | toCond (Ex e) = fn (t, f) => T.CJUMP(T.NE, e, T.CONST 0, t, f)

    val slStart = T.TEMP X86Frame.FP

    fun searchStaticLink (e : T.exp) (Level at) (Level goal) : T.exp =
        (case (#eqc at = #eqc goal)
          of true => e
           | false => (case (Symbol.name (X86Frame.name (#frame at)) = Symbol.name (X86Frame.name (#frame goal))) of true => print "EQUAL NAMES" | false => ();
                      print (Symbol.name (X86Frame.name (#frame at)) ^ " " ^ Symbol.name (X86Frame.name (#frame goal)) ^ "\n");
                      searchStaticLink (T.MEM (T.BINOP (T.PLUS, e, T.CONST 8))) (#prev_level at) (Level goal)))
      | searchStaticLink (e : T.exp) (Level at) Outermost = searchStaticLink (T.MEM (T.BINOP (T.PLUS, e, T.CONST 8))) (#prev_level at) Outermost
      | searchStaticLink e Outermost Outermost = T.MEM e
      | searchStaticLink _ _ _ = ErrorMsg.impossible "accessing variable in Outermost"

    fun simpleVar ((acc_lev, x86_a), var_l) =
        Ex (X86Frame.exp x86_a (searchStaticLink slStart var_l acc_lev))

    fun fieldVar(arr, index) =
      let
        val e_arr = toEx arr
        val loc = index * X86Frame.wordSize
      in
        Ex (T.MEM (T.BINOP(T.PLUS, e_arr, T.CONST loc)))
      end

    fun subscriptVar(arr, index) =
      let
        val e_arr = toEx arr
        val e_index = toEx index
      in
        Ex (T.MEM (T.BINOP(T.PLUS, e_arr, T.BINOP(T.MUL, e_index, T.CONST(X86Frame.wordSize)))))
      end

    val nilExp = Ex(T.CONST(0))

    fun intExp i = Ex(T.CONST(i))

    fun stringExp s =
      let
            val l = Temp.newlabel ()
            val f = STRING(l, s)
      in
      frags := f :: !frags; Ex (T.NAME l)
      end

    fun callExp (at, Outermost, func, args) = Ex (T.CALL (T.NAME func, map toEx args))
      | callExp (at, Level call, func, args) =
      let
        val link = searchStaticLink slStart at (#prev_level call)
        val e_args = map toEx args
      in
      Ex (T.CALL (T.NAME func, link :: e_args))
      end

    fun eqOpExp (left, A.EqOp, right, Types.STRING) =
          Ex (T.CALL(T.NAME(Temp.namedlabel "stringEqual"), [toEx left, toEx right]))
      | eqOpExp (left, A.NeqOp, right, Types.STRING) = Cond(fn (t, f) =>
          T.CJUMP(T.EQ, (T.CALL(T.NAME(Temp.namedlabel "stringEqual"), [toEx left, toEx right])), T.CONST 0, t, f))
      | eqOpExp (left, A.EqOp, right, _) = Cond(fn (t, f) => T.CJUMP(T.EQ, toEx left, toEx right, t, f))
      | eqOpExp (left, A.NeqOp, right, _) = Cond(fn (t, f) => T.CJUMP(T.NE, toEx left, toEx right, t, f))

    fun opExp (left, A.PlusOp, right) = Ex(T.BINOP(T.PLUS, toEx left, toEx right)) 
      | opExp (left, A.MinusOp, right) = Ex(T.BINOP(T.MINUS, toEx left, toEx right)) 
      | opExp (left, A.TimesOp, right) = Ex(T.BINOP(T.MUL, toEx left, toEx right)) 
      | opExp (left, A.DivideOp, right) = Ex(T.BINOP(T.DIV, toEx left, toEx right)) 
      | opExp (left, A.EqOp, right) = (print "Use eqOpExp"; raise ErrorMsg.Error)
      | opExp (left, A.NeqOp, right) = (print "Use eqOpExp"; raise ErrorMsg.Error)
      | opExp (left, A.LtOp, right) = Cond(fn (t, f) => T.CJUMP(T.LT, toEx left, toEx right, t, f))
      | opExp (left, A.LeOp, right) = Cond(fn (t, f) => T.CJUMP(T.LE, toEx left, toEx right, t, f))
      | opExp (left, A.GtOp, right) = Cond(fn (t, f) => T.CJUMP(T.GT, toEx left, toEx right, t, f))
      | opExp (left, A.GeOp, right) = Cond(fn (t, f) => T.CJUMP(T.GE, toEx left, toEx right, t, f))

    fun recordExp fields =
      let
        val num_fields = length fields
        val r = Temp.newtemp ()
        val alloc = T.MOVE(T.TEMP r, T.CALL(T.NAME (Temp.namedlabel "allocRecord"), [T.CONST(num_fields)]))

        fun init (ex, n) = T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP r, (T.CONST (n * X86Frame.wordSize)))), toEx ex)
        val field_set = map init (ListPair.zip (fields, List.tabulate (num_fields, fn x => x)))
      in
      Ex (T.ESEQ (seq (alloc :: field_set), T.TEMP r))
      end

    fun init [] = []
      | init [x] = []
      | init (x :: xs) = x :: init  xs

    fun seqExp [] = Ex (T.CONST 0)
      | seqExp [e] = e
      | seqExp (stms) = Ex (T.ESEQ (seq (map toStm (init stms)), toEx (List.last stms)))

    fun assignExp(loc, v) =
      let
        val e_loc = toEx loc
        val e_v = toEx v
      in
      Stm (T.MOVE(e_loc, e_v))
      end

    fun ifExp(test, then', SOME else') =
        let
          val ctest = toCond test
          val e_then = toEx then'
          val e_else = toEx else'

          val r = Temp.newtemp ()
          val t = Temp.newlabel ()
          val f = Temp.newlabel ()
        
          val j = Temp.newlabel ()

          val s = (seq[ ctest(t, f),
                        T.LABEL t,
                        T.MOVE(T.TEMP r, e_then),
                        T.JUMP(T.NAME j, [j]),
                        T.LABEL f,
                        T.MOVE(T.TEMP r, e_else),
                        T.LABEL j])
        in
        Ex (T.ESEQ(s, T.TEMP r))
        end
      | ifExp(test, then', NONE) =
        let
          val ctest = toCond test
          val e_then = toEx then'

          val r = Temp.newtemp ()
          val t = Temp.newlabel ()
          val f = Temp.newlabel ()
        
          val j = Temp.newlabel ()

          val s = seq[ ctest(t, f),
                       T.LABEL t,
                       T.MOVE(T.TEMP r, e_then),
                       T.JUMP(T.NAME j, [j]),
                       T.LABEL f,
                       T.LABEL j]
        in
        Stm s
        end

    fun whileExp(donel, test, body) =
      let
        val testl = Temp.newlabel ()
        val bodyl = Temp.newlabel ()

        val ctest = toCond test
        val ebody = toStm body

        val s = seq [ T.LABEL testl,
                      ctest(bodyl, donel),
                      T.LABEL bodyl,
                      ebody,
                      T.JUMP(T.NAME testl, [testl]),
                      T.LABEL donel
        ]
      in
      Stm s
      end
    
    fun forExp(donel, var, lo, hi, body) =
      let

        val e_var = toEx var
        val e_lo = toEx lo
        val e_hi = toEx hi
        val e_body = toStm body

        val testl = Temp.newlabel ()
        val bodyl = Temp.newlabel ()
        val incrl = Temp.newlabel ()


        (* val s = seq [ T.MOVE(e_var, e_lo),
                      T.LABEL incrl,
                      T.CJUMP(T.LE, e_var, e_hi, bodyl, donel),
                      T.LABEL bodyl,
                      e_body,
                      T.MOVE(e_var, T.BINOP(T.PLUS, e_var, T.CONST 1)),
                      T.JUMP(T.NAME incrl, [incrl]),
                      T.LABEL donel ] *)
        val s = seq [ T.MOVE(e_var, e_lo),
                      T.CJUMP(T.LE, e_var, e_hi, bodyl, donel),
                      T.LABEL bodyl,
                      e_body,
                      T.CJUMP(T.LT, e_var, e_hi, incrl, donel),
                      T.LABEL incrl,
                      T.MOVE(e_var, T.BINOP(T.PLUS, e_var, T.CONST 1)),
                      T.JUMP(T.NAME bodyl, [bodyl]),
                      T.LABEL donel
        ]
      in
      Stm s
      end
    
    fun breakExp breakl = Stm (T.JUMP(T.NAME breakl, [breakl]))

    fun letExp (decs, body) = Ex(T.ESEQ(seq (map toStm decs), toEx body))
    
    fun arrayExp (size, init) = Ex (T.CALL(T.NAME(Temp.namedlabel "initArray"), [toEx size, toEx init]))

    fun procEntryExit { level = Level {frame, ...}, body } =
          frags := PROC { body = T.MOVE(T.TEMP X86Frame.RV, toEx body), frame = frame } :: (!frags)
      | procEntryExit { level = Outermost, ...} = (print "procEntryExit: impossible"; raise ErrorMsg.Error)
end