structure Interpret : sig val interpret : Tree.stm -> unit end =
struct

  structure H = HashTable
  structure T = Tree

  fun printExp e = Printtree.printtree(TextIO.stdOut, T.EXP e);

  exception NoTemp
  val sizeHint = 128
  val tempVals : (Temp.temp,T.exp) H.hash_table = 
		H.mkTable((fn t => HashString.hashString (Temp.makestring t)), op = ) (sizeHint, NoTemp)

  exception NoMem
  val memVals : (int,T.exp) H.hash_table = 
		H.mkTable((fn t => Word.fromInt (t mod 128)), op = ) (sizeHint, NoMem)

  val initSP = ~X86Frame.wordSize
  val initAlloced = X86Frame.wordSize

  val FP = X86Frame.FP
  val SP = ref initSP
  val alloced = ref initAlloced

  fun pushStack e = (SP := !SP - X86Frame.wordSize; H.insert memVals (!SP, e) )
  fun popStack _ = (SP := !SP + X86Frame.wordSize )

  fun adjustSP (m : int) = SP := (if m > !SP then !SP else m)

  exception Unsupported


  fun reset () = (SP := initSP; alloced := initAlloced) 

  fun find x [] : 'b option = NONE
    | find x ((y, v) :: xs) = if x = y then SOME v else find x xs

  fun repTempExp (pairs : ((Temp.temp * Temp.temp) list) ref) (e : T.exp) =
    let
      fun rep (T.BINOP(b, e1, e2)) = T.BINOP(b, rep e1, rep e2)
        | rep (T.MEM e) = T.MEM (rep e)
        | rep (T.TEMP t) = (case find t (!pairs)
                              of SOME new_temp => T.TEMP new_temp
                               | NONE => let val n = Temp.newtemp () in
                                         pairs := ((t, n) :: !pairs); T.TEMP n end)
        | rep (T.ESEQ (s, e)) = T.ESEQ(repTempStm pairs s, rep e)
        | rep (T.NAME l) = T.NAME l
        | rep (T.CONST i) = T.CONST i
        | rep (T.CALL (c, es)) = T.CALL (rep c, map rep es)
    in
    rep e
    end
  and repTempStm (pairs : ((Temp.temp * Temp.temp) list) ref) (s : T.stm) =
    let
      fun rep (T.SEQ(e1, e2)) = T.SEQ(rep e1, rep e2)
        | rep (T.LABEL l) = T.LABEL l
        | rep (T.JUMP (e, ls)) = T.JUMP (repTempExp pairs e, ls)
        | rep (T.CJUMP (rel, e1, e2, l1, l2)) =
            T.CJUMP (rel, repTempExp pairs e1, repTempExp pairs e2, l1, l2)
        | rep (T.MOVE(e1, e2)) =
            T.MOVE(repTempExp pairs e1, repTempExp pairs e2)
        | rep (T.EXP e) = T.EXP (repTempExp pairs e)
    in
    rep s
    end

  fun appBinop T.PLUS i1 i2 = i1 + i2
    | appBinop T.MINUS i1 i2 = i1 - i2
    | appBinop T.MUL i1 i2 = i1 * i2
    | appBinop T.DIV i1 i2 = Int.quot (i1, i2)
    | appBinop _ _ _ = raise Unsupported

  fun appRelop T.EQ i1 i2 = i1 = i2
    | appRelop T.NE i1 i2 = not (i1 = i2)
    | appRelop T.LT i1 i2 = i1 < i2
    | appRelop T.GT i1 i2 = i1 > i2
    | appRelop T.LE i1 i2 = i1 <= i2
    | appRelop T.GE i1 i2 = i1 >= i2
    | appRelop _ _ _ = raise Unsupported
  
  fun lookupFS _ [] = raise Unsupported
    | lookupFS l (Tr.STRING (lab, s) :: xs) = if l = lab then s else lookupFS l xs
    | lookupFS l (x :: xs) = lookupFS l xs

  fun lookupFragString l = lookupFS l (!Tr.frags)

  fun lookupFF _ [] = ErrorMsg.impossible "expected function"
    | lookupFF nm (Tr.PROC {body, frame} :: xs) =
      if X86Frame.name frame = nm then (X86Frame.formals frame, body) else lookupFF nm xs
    | lookupFF nm (x :: xs) = lookupFF nm xs

  fun lookupFragFunc nm = lookupFF nm (!Tr.frags)

  fun expInt (T.CONST i) = i
    | expInt _ = ErrorMsg.impossible "expected int"

  fun expLabel (T.NAME l) = l
    | expLabel _ = ErrorMsg.impossible "expected label"

  datatype runStm = RunStm of T.stm | RunNextSeq | NoStm

  datatype interpMode = Run | JumpTo of T.label | Discard

  fun interpretStm (stm : T.stm) =
      let
        fun jump (l : Temp.label) (T.SEQ(T.LABEL sl, s) : T.stm) = if l = sl then RunStm s else jump l s
          | jump (l : Temp.label) (T.LABEL sl : T.stm) = if l = sl then RunNextSeq else NoStm
          | jump l (T.SEQ (s1, s2)) = (case jump l s1 of NoStm => jump l s2 | RunNextSeq => RunStm s2 | RunStm s => RunStm (T.SEQ (s, s2)))
          | jump l _ = NoStm

        and unsafeJump l s = case jump l s
                                of RunStm s' => (interp s'; false)
                                 | RunNextSeq => true
                                 | _ => (print ("label " ^ Symbol.name l ^ " not found"); Printtree.printtree(TextIO.stdOut, stm); raise Unsupported)

        and interp (T.SEQ(s1, s2)) = (if interp s1 then interp s2 else false)
          | interp (T.LABEL l) = true
          | interp (T.JUMP (T.NAME l, _)) = (unsafeJump l stm; false)
          | interp (T.JUMP (_, _)) = raise Unsupported
          | interp (T.CJUMP(relop, e1, e2, l1, l2)) =
            let
              val i1 = expInt (interpretExp e1)
              val i2 = expInt (interpretExp e2)
              val res = appRelop relop i1 i2
            in
            (if res then unsafeJump l1 stm else unsafeJump l2 stm; false)
            end
          | interp (T.MOVE(T.TEMP t, e2)) =
            let
              val v = interpretExp e2
            in
              H.insert tempVals (t, v); true
            end
          | interp (T.MOVE(T.MEM(e1), e2)) =
            let
              val m = expInt (interpretExp e1)
              val v = interpretExp e2
            in
              adjustSP m; H.insert memVals (m, v); true
            end
          | interp (T.MOVE(_, _)) = raise Unsupported
          | interp (T.EXP e) = (interpretExp e; true)
      in
      (interp stm; ())
      end

  and interpretExp (exp : T.exp) : T.exp =
      case exp
          of T.BINOP(binop, e1, e2) =>
            let
              val i1 = expInt (interpretExp e1)
              val i2 = expInt (interpretExp e2)
            in
              T.CONST (appBinop binop i1 i2)
            end
           | T.MEM l => let
                          val i = expInt (interpretExp l) 
                          val v = H.lookup memVals i
                        in v end
           | T.TEMP t => H.lookup tempVals t
           | T.ESEQ(s, e) => (interpretStm s; interpretExp e)
           | T.NAME n => T.NAME n
           | T.CONST i => T.CONST i
           | T.CALL(c, es) =>
              let
                val nm = expLabel c
                val es' = map interpretExp es
              in
              case Symbol.name nm
                of "print" => (case es'
                    of [T.NAME l] => (print (lookupFragString l); T.CONST 0)
                     | _ => raise Unsupported)
                 | "getchar" => (case es'
                    of [] =>
                      let
                        val c = TextIO.inputN(TextIO.stdIn, 1)
                        val new = Temp.newlabel ()
                        val _ = Tr.frags := Tr.STRING (new, c) :: (!Tr.frags)
                      in
                      T.NAME new
                      end
                     | _ => raise Unsupported)
                 | "ord" => (case es'
                    of [T.NAME l] => 
                      let
                          val s = lookupFragString l
                          val ord = Char.ord(String.sub(s, 0))
                      in
                      T.CONST ord
                      end
                     | _ => raise Unsupported)
                 | "chr" => (case es'
                    of [T.CONST i] =>
                      let
                        val chr = Char.toString (Char.chr i)

                        val new = Temp.newlabel ()
                        val _ = Tr.frags := Tr.STRING (new, chr) :: (!Tr.frags)
                      in
                      T.NAME new
                      end
                     | _ => raise Unsupported)
                 | "concat" => (case es'
                    of [T.NAME l1, T.NAME l2] =>
                      let
                        val s1 = lookupFragString l1
                        val s2 = lookupFragString l2

                        val new = Temp.newlabel ()
                        val _ = Tr.frags := Tr.STRING (new, s1 ^ s2) :: (!Tr.frags)
                      in
                      T.NAME new
                      end
                     | _ => raise Unsupported)
                 | "initArray" => (case es'
                  of [T.CONST i, v] =>
                    let
                      val start = !alloced
                      val locs = List.tabulate (i, fn j => j * X86Frame.wordSize + start)
                    in
                    (
                    alloced := i * X86Frame.wordSize + !alloced;
                    app (fn m => H.insert memVals (m, v)) locs;
                    T.CONST start
                    )
                    end
                   | _ => raise Unsupported)
                 | "allocRecord" => (case es'
                  of [T.CONST i] =>
                    let
                      val start = !alloced
                      val locs = List.tabulate (i, fn j => j * X86Frame.wordSize + start)
                    in
                    (
                    alloced := i * X86Frame.wordSize + !alloced;
                    app (fn m => H.insert memVals (m, T.CONST 0)) locs;
                    T.CONST start
                    )
                    end
                   | _ => raise Unsupported)
                 | "stringEqual" => (case es'
                    of [T.NAME l1, T.NAME l2] =>
                      let
                        val s1 = lookupFragString l1
                        val s2 = lookupFragString l2
                      in
                      if s1 = s2 then T.CONST 1 else T.CONST 0
                      end
                     | _ => raise Unsupported)
                 | s =>
                  let
                    val (formals, body) = lookupFragFunc nm
                    val conv_forms = map (fn f => X86Frame.exp f (T.TEMP FP)) formals

                    (* rename temporaries *)
                    val re_forms = map (fn f => case f
                                                  of T.TEMP _ => T.TEMP (Temp.newtemp ())
                                                   | T.MEM m => T.MEM m) conv_forms
                    val re_body = repTempStm
                                  (ref (List.mapPartial (fn (c, r) => case (c, r)
                                                                    of (T.TEMP t1, T.TEMP t2) => SOME (t1, t2)
                                                                     | _ => NONE)
                                            ((T.TEMP FP, T.TEMP FP)
                                                :: (T.TEMP X86Frame.RV, T.TEMP X86Frame.RV)
                                                :: ListPair.zip (conv_forms, re_forms))))
                                  body

                    fun regArgs (cf, e) = (case cf
                                                of T.TEMP t => H.insert tempVals (t, e)
                                                 | T.MEM _ => ())
                    fun stackArgs (cf, e) = (case cf
                                                of T.TEMP t => ()
                                                 | T.MEM _ => pushStack e)
                    val _ = app regArgs (ListPair.zip (re_forms, es'))
                    val pre_sp = !SP
                    val _ = app stackArgs (ListPair.zip (List.rev re_forms, List.rev es'))
                    val curr_fp = H.lookup tempVals FP
                    val fp = interpretExp curr_fp
                    val curr_rv = H.lookup tempVals X86Frame.RV
                    val rv = interpretExp curr_rv

                    val _ = (H.insert tempVals (FP, T.CONST (!SP));
                             interpretStm re_body)
                    val v = interpretExp (T.TEMP X86Frame.RV)

                    val _ = SP := pre_sp
                  in
                  H.insert tempVals (FP, fp);
                  H.insert tempVals (X86Frame.RV, rv);
                  v
                  end
              end

  fun interpret (prog : T.stm) : unit = (reset ();
                                         H.insert tempVals (FP, T.CONST 0);
                                         H.insert tempVals (X86Frame.RV, T.CONST 0);
                                         H.insert memVals (0, T.CONST 0);
                                         print("INTERPRETER\n");
                                         interpretStm prog;
                                         print("\nEND INTERPRETER\n"))
end
