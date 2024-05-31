structure T = Types
structure E = Env

structure Semant =
struct
  type venv = E.enventry S.table
  type tenv = T.ty S.table
  type level = Translate.level

  datatype loopstat = InLoop | NotLoop

  fun checkInt (ty : T.ty) (s : string) (p : A.pos) =
      case ty of T.INT => ()
               | T.BOTTOM => ()
               | _ => ErrorMsg.error p ("TYPE: " ^ s ^ " passed wrong type\nexpected: int\npassed: " ^ T.printType ty)

  fun checkString (ty : T.ty) (s : string) (p : A.pos) =
      case ty of T.STRING => ()
               | T.BOTTOM => ()
               | _ => ErrorMsg.error p ("TYPE: " ^ s ^ " passed wrong type\nexpected: string\npassed: " ^ T.printType ty)

  fun checkUnit (ty : T.ty) (s : string) (p : A.pos) =
      case ty of T.UNIT => ()
               | T.BOTTOM => ()
               | _ => ErrorMsg.error p ("TYPE: " ^ s ^ " passed wrong type\nexpected: unit\npassed: " ^ T.printType ty)

  fun checkEqOp (T.RECORD (t1, u1)) (T.RECORD (t2, u2)) (p : A.pos) =
            if u1 = u2 then ()
                        else ErrorMsg.error p
                                ("TYPE: equality operator given incompatible record types\nleft type: "
                                      ^ T.printType (T.RECORD (t1, u1))  ^ "\nright type: " ^ T.printType (T.RECORD (t2, u2)))
    | checkEqOp (T.RECORD _) T.NIL _ = ()
    | checkEqOp T.NIL (T.RECORD _) _ = ()
    | checkEqOp (T.ARRAY (t1, u1)) (T.ARRAY (t2, u2)) p =
            if u1 = u2 then ()
                       else ErrorMsg.error p 
                               ("TYPE: equality operator given incompatible array types\nleft type: "
                                      ^ T.printType (T.ARRAY (t1, u1))  ^ "\nright type: " ^ T.printType( T.ARRAY (t2, u2)))
    | checkEqOp T.INT T.INT _ = ()
    | checkEqOp T.STRING T.STRING _ = ()
    | checkEqOp t1 t2 p = ErrorMsg.error p ("TYPE: equality operator given incompatible types\nleft type: "
                                      ^ T.printType t1  ^ "\nright type: " ^ T.printType t2)


  fun checkCompOp T.INT T.INT _ = ()
    | checkCompOp T.STRING T.STRING _ = ()
    | checkCompOp t1 t2 p =
        ErrorMsg.error p ("TYPE: comparison operator given incompatible types\nleft type: "
                                      ^ T.printType t1  ^ "\nright type: " ^ T.printType t2)

  fun compatType (T.BOTTOM, _, _, _, _, _) = ()
    | compatType (_, T.BOTTOM, _, _, _, _) = ()
    | compatType (t1, t2, p, s, ann1, ann2) =
      if T.compatType t1 t2
        then ()
        else ErrorMsg.error p ("TYPE: " ^ s ^ "\n" ^ ann1 ^ ": " ^ T.printType t1 ^ "\n" ^ ann2 ^ ": " ^ T.printType t2)

  fun lookTy (tenv : tenv) s pos : T.ty =
      case S.look (tenv, s)
        of SOME (T.NAME (n, r)) => (case !r of SOME t => t | NONE => T.BOTTOM)
         | SOME t => t
         | NONE => (ErrorMsg.error pos ("SCOPE: unrecognized type name " ^ S.name s); T.BOTTOM)


  fun actualTy (tenv : tenv) (T.NAME (_, tr)) pos : T.ty = (case !tr
                                                            of SOME ty => ty
                                                             | NONE => T.BOTTOM)
    | actualTy _ t _ = t

  (* Avoid "NIL", if possible. *)
  fun selectType T.NIL (t : T.ty) = t
    | selectType t _ = t

  fun checkReadOnly pos (venv : venv) (A.SimpleVar (s, _)) = 
              (case S.look(venv, s)
                of SOME (E.VarEntry { readonly = true, ... }) =>
                      ErrorMsg.error pos ("READONLY: readonly variable " ^ S.name s ^ " assigned to")  
                 | _ => ())
    | checkReadOnly _ _ _ = ()

  fun transVar (loop : loopstat) (level : level) (venv : venv) (tenv : tenv) (v : A.var) : Tr.exp * T.ty =
      let
        fun trvar (A.SimpleVar (s, pos)) = 
              (case S.look(venv, s)
                of SOME (E.VarEntry { ty, access, ... }) => (Tr.simpleVar (access, level), actualTy tenv ty pos)
                | SOME (E.FunEntry { result, ...}) => (ErrorMsg.error pos ("SCOPE: function symbol " ^ S.name s ^ " used as a variable"); (Tr.nilExp, result))
                | NONE => (ErrorMsg.error pos ("SCOPE: unrecognized variable " ^ S.name s); (Tr.nilExp, T.BOTTOM)))
          | trvar (A.FieldVar(v, sym, pos)) =
              (case trvar v
                of (tr_v, T.RECORD (fields, _)) =>
                    let
                      val num_fields = length fields
                      val tab_fields = ListPair.zip (fields, List.tabulate (num_fields, fn x => x))
                    in
                    (case List.find (fn ((s, _), _) => s = sym) tab_fields
                      of SOME ((_, ty), num) => (Tr.fieldVar (tr_v, num), actualTy tenv ty pos)
                       | NONE => (ErrorMsg.error pos ("SCOPE: unrecognized record field " ^ S.name sym); (Tr.nilExp, T.BOTTOM)))
                    end
                 | (_, t) => (ErrorMsg.error pos ("SCOPE: unrecognized record " ^ T.printType t); (Tr.nilExp, T.BOTTOM)))
          | trvar (A.SubscriptVar(v, exp, pos)) =
              let
                val (tr_e, ty_e) = transExp loop NONE level venv tenv exp
              in
              (checkInt ty_e "array subscript" pos;
                case trvar v
                  of (tr_v, T.ARRAY (ty, _)) => (Tr.subscriptVar (tr_v, tr_e), actualTy tenv ty pos)
                   | _ => (ErrorMsg.error pos ("SCOPE: unrecognized array"); (Tr.nilExp, T.BOTTOM)))
              end

      in
      trvar v
      end

  and transExp (loop : loopstat) (break_lab : Temp.label option) (level : level) (venv : venv) (tenv : tenv) (e : A.exp) : Tr.exp * T.ty =
  let
    fun trexp (A.VarExp v) = transVar loop level venv tenv v
      | trexp A.NilExp = (Tr.nilExp, T.NIL)
      | trexp (A.IntExp i) = (Tr.intExp i, T.INT)
      | trexp (A.StringExp (s, _)) = (Tr.stringExp s, T.STRING)
      | trexp (A.CallExp { func, args, pos }) = 
        (case S.look (venv, func)
           of SOME (E.FunEntry { formals, result, level = dec_level, label, ... }) =>
              let
                  fun checkArgs ([] : A.exp list) ([] : T.ty list) = []
                    | checkArgs [] (_ :: _) = (ErrorMsg.error pos ("SCOPE: too few arguments for function " ^ S.name func); [])
                    | checkArgs (_ :: _) [] = (ErrorMsg.error pos ("SCOPE: too many arguments for function " ^ S.name func); [])
                    | checkArgs (e :: es) (f :: fs) =
                      let val (re, te) = trexp e in
                      compatType(f, te, pos, "incompatible function argument type", "expected", "passed");
                      re :: checkArgs es fs
                      end
                                in
              case Int.compare (List.length args, List.length formals)
                of EQUAL => let val te = checkArgs args formals in (Tr.callExp (level, dec_level, label, te), result) end
                 | LESS => (ErrorMsg.error pos "SCOPE: too few arguments passed for function"; (Tr.nilExp, result))
                 | GREATER => (ErrorMsg.error pos "SCOPE: too many arguments passed for function"; (Tr.nilExp, result))
              end

            | SOME (E.VarEntry { ty, ... }) => (ErrorMsg.error pos "SCOPE: incorrect use of variable as function"; (Tr.nilExp, ty))
            | NONE => (ErrorMsg.error pos "SCOPE: unrecognized function"; (Tr.nilExp, T.BOTTOM)))

      | trexp (A.OpExp { left, oper = A.PlusOp, right, pos }) =
          let
            val (e_left, ty_left) = trexp left
            val (e_right, ty_right) = trexp right
          in
          (checkInt ty_left "operator" pos; checkInt ty_right "operator" pos; (Tr.opExp(e_left, A.PlusOp, e_right), T.INT))
          end
      | trexp (A.OpExp { left, oper = A.MinusOp, right, pos }) =
          let
            val (e_left, ty_left) = trexp left
            val (e_right, ty_right) = trexp right
          in
          (checkInt ty_left "operator" pos; checkInt ty_right "operator" pos; (Tr.opExp(e_left, A.MinusOp, e_right), T.INT))
          end
      | trexp (A.OpExp { left, oper = A.TimesOp, right, pos }) =
          let
            val (e_left, ty_left) = trexp left
            val (e_right, ty_right) = trexp right
          in
          (checkInt ty_left "operator" pos; checkInt ty_right "operator" pos; (Tr.opExp(e_left, A.TimesOp, e_right), T.INT))
          end
      | trexp (A.OpExp { left, oper = A.DivideOp, right, pos }) =

          let
            val (e_left, ty_left) = trexp left
            val (e_right, ty_right) = trexp right
          in
          (checkInt ty_left "operator" pos; checkInt ty_right "operator" pos; (Tr.opExp(e_left, A.DivideOp, e_right), T.INT))
          end
      | trexp (A.OpExp { left, oper = A.EqOp, right, pos }) =
        let
            val (e_left, ty_left) = trexp left
            val (e_right, ty_right) = trexp right
        in
        checkEqOp ty_left ty_right pos; (Tr.eqOpExp(e_left, A.EqOp, e_right, ty_left), T.INT)
        end
      | trexp (A.OpExp { left, oper = A.NeqOp, right, pos }) =
        let
            val (e_left, ty_left) = trexp left
            val (e_right, ty_right) = trexp right
        in
        checkEqOp ty_left ty_right pos; (Tr.eqOpExp(e_left, A.NeqOp, e_right, ty_left), T.INT)
        end
      | trexp (A.OpExp { left, oper = A.LtOp, right, pos }) =
        let
            val (e_left, ty_left) = trexp left
            val (e_right, ty_right) = trexp right
        in
        checkCompOp ty_left ty_right pos; (Tr.opExp(e_left, A.LtOp, e_right), T.INT)
        end
      | trexp (A.OpExp { left, oper = A.LeOp, right, pos }) =
        let
            val (e_left, ty_left) = trexp left
            val (e_right, ty_right) = trexp right
        in
        checkCompOp ty_left ty_right pos; (Tr.opExp(e_left, A.LeOp, e_right), T.INT)
        end
      | trexp (A.OpExp { left, oper = A.GtOp, right, pos }) =
        let
            val (e_left, ty_left) = trexp left
            val (e_right, ty_right) = trexp right
        in
        checkCompOp ty_left ty_right pos; (Tr.opExp(e_left, A.GtOp, e_right), T.INT)
        end
      | trexp (A.OpExp { left, oper = A.GeOp, right, pos }) =
        let
            val (e_left, ty_left) = trexp left
            val (e_right, ty_right) = trexp right
        in
        checkCompOp ty_left ty_right pos; (Tr.opExp(e_left, A.GeOp, e_right), T.INT)
        end

      | trexp (A.RecordExp { fields, typ, pos }) =
        (case lookTy tenv typ pos
          of T.RECORD (fs, u) =>
              let 
                  fun checkFields ([] : (S.symbol * A.exp * A.pos) list) ([] : (S.symbol * T.ty) list) = []
                    | checkFields [] (_ :: _) = (ErrorMsg.error pos "SCOPE: missing fields for record"; [])
                    | checkFields (_ :: _) [] = (ErrorMsg.error pos "SCOPE: too many fields for record"; [])
                    | checkFields ((s1, e, p) :: es) ((s2, t) :: fs) =
                      (if s1 = s2 then () else ErrorMsg.error p "SCOPE: mismatched record field names";
                       let val (re, ty_f) = trexp e in
                          compatType(actualTy tenv t pos, ty_f, p, "incorrect type in record", "field's type", "expression's type");
                          re :: checkFields es fs
                       end)
                  
                  val exps = checkFields fields fs
              in
              (Tr.recordExp exps, T.RECORD (fs, u))
              end
           | _ => (ErrorMsg.error pos "SCOPE: expected record but got other symbol"; (Tr.nilExp, T.BOTTOM)))
      | trexp (A.SeqExp es) =
          let
              fun walk [] = ([Tr.nilExp], T.UNIT)
                | walk [(e, _)] = let val (tr_e, ty_e) = trexp e in ([tr_e], ty_e) end
                | walk ((e, _) :: es) = let val (e_seq, ty_seq) = walk es
                                            val (e_h, _) = trexp e
                                        in 
                                        (e_h :: e_seq, ty_seq)
                                        end
              val (r_es, ty) = walk es
          in
          (Tr.seqExp r_es, ty)
          end
      | trexp (A.AssignExp { var, exp, pos }) =
        let
          val (e_var, ty_var) = transVar loop level venv tenv var
          val (e_val, ty_val) = trexp exp
        in
        (compatType(ty_val, ty_var, pos, "incorrect types in assignment", "bindee's type", "bound's type");
         checkReadOnly pos venv var;
         (Tr.assignExp(e_var, e_val), T.UNIT))
        end
      | trexp (A.IfExp { test, then', else' = NONE, pos}) = 
              let
                val (test_e, test_ty) = trexp test
                val (then_e, then_ty) = trexp then'
              in
              checkInt test_ty "if test" pos;
              checkUnit then_ty "if expression" pos;
              (Tr.ifExp (test_e, then_e, NONE), then_ty)
              end
      | trexp (A.IfExp { test, then', else' = SOME e, pos}) =
              (
              let
                val (test_e, test_ty) = trexp test
                val (then_e, then_ty) = trexp then'
                val (else_e, else_ty) = trexp e
              in
                (checkInt test_ty "if test" pos;
                compatType(then_ty, else_ty, pos, "if expression branches incompatible", "then type", "else type");
                (Tr.ifExp (test_e, then_e, SOME else_e), selectType then_ty else_ty))
              end)
      | trexp (A.WhileExp { test, body, pos}) =
        let
          val break_lab = Temp.newlabel ()

          val (e_test, ty_test) = trexp test
          val (e_body, ty_body) = transExp InLoop (SOME break_lab) level venv tenv body
          val e_while = Tr.whileExp (break_lab, e_test, e_body)
        in
        (checkInt ty_test "while test" pos;
         checkUnit ty_body "while body" pos;
         (e_while, T.UNIT))
        end
      | trexp (A.ForExp { var, escape, lo, hi, body, pos, ...}) =
        let
          val loc = Tr.allocLocal level (!escape)
          val venv' = S.enter(venv, var, E.VarEntry { access = loc, ty = T.INT, readonly = true })

          val break_lab = Temp.newlabel ()

          val e_v = Tr.simpleVar (loc, level)
          val (e_lo, ty_lo) = trexp lo
          val (e_hi, ty_hi) = trexp hi
          val (e_body, ty_body) = transExp InLoop (SOME break_lab) level venv' tenv body

          val e_for = Tr.forExp (break_lab, e_v, e_lo, e_hi, e_body)
        in
        (Tr.printAccess var loc;
        checkInt ty_lo "for lower bound" pos;
         checkInt ty_hi "for upper bound" pos;
         checkUnit ty_body "for body" pos;
         (e_for, T.UNIT))
        end
      | trexp (A.BreakExp pos) = 
        (case loop
          of InLoop => ()
           | NotLoop => ErrorMsg.error pos "MISPLACED: break used outside loop";
         case break_lab
            of SOME bl => (Tr.breakExp bl, T.UNIT)
             | NONE => (Tr.nilExp, T.UNIT))
      | trexp (A.LetExp {decs, body, pos}) =
          let
              fun walk vt [] = vt
                | walk {exps, venv, tenv} (d :: ds) =
                    let
                       val {exps = es, venv = vn, tenv = tn} = transDec loop break_lab level venv tenv d
                    in
                    walk {exps = exps @ es, venv = vn, tenv = tn} ds
                    end
              val { exps, venv = venv2, tenv = tenv2 } = walk {exps = [], venv = venv, tenv = tenv} decs

              val (e_body, e_ty) = transExp loop break_lab level venv2 tenv2 body
          in
          (Tr.seqExp (exps @ [e_body]), e_ty)
          end
      | trexp (A.ArrayExp { typ, size, init, pos }) =
        let
          val (e_size, ty_size) = trexp size
          val (e_init, ty_init) = trexp init
         in
        (checkInt ty_size "array size" pos;
        case lookTy tenv typ pos
          of T.ARRAY (t, u) => (compatType(actualTy tenv t pos, ty_init, pos, "array initializer of incorrect type", "expected", "actual");
                                (Tr.arrayExp (e_size, e_init), T.ARRAY (t, u)))
           | _ => (ErrorMsg.error pos "SCOPE: expected array but got other symbol"; (Tr.nilExp, T.BOTTOM)))
        end
  in
    trexp e
  end
  
  and transDec (loop : loopstat) (break_lab : Temp.label option) (level : level) (venv : venv) (tenv : tenv) (d : A.dec) : { exps : Tr.exp list, venv : venv, tenv : tenv} =
      case d
        of A.FunctionDec fs =>
            let
              fun getResult r = case r
                                  of SOME (r, p) => lookTy tenv r p
                                   | NONE => T.UNIT

              fun add ({ name, params, result, body, pos } : A.fundec, table) =
                  let
                    val formals = map (fn { typ, pos, ... } => lookTy tenv typ pos) params
                    val res = getResult result

                    val newname = Temp.newlabel ()

                    val formals_esc = map (fn { escape, ...} => !escape) params
                    val new_level = Tr.newLevel {parent = level, name = newname, formals = formals_esc}
                  in
                    Tr.printLevel name new_level;
                    S.enter(table, name, E.FunEntry { level = new_level, label = newname, formals = formals, result = res })
                  end
                
              val venv_fs = foldr add venv fs
              
              fun checkBody ({ name = fun_name, params, result, body, pos, ... } : A.fundec) =
                let
                  val new_level = case S.look(venv_fs, fun_name)
                                    of SOME (E.FunEntry { level, ...}) => level
                                      | _ => (print "transDec: impossible- function not in venv"; raise ErrorMsg.Error)

                  fun addParams (({ name, typ, pos, escape,... } : A.field, loc), table) =
                      let
                        val ventry = E.VarEntry { access = loc, ty = lookTy tenv typ pos, readonly = false }
                      in
                      S.enter(table, name, ventry)
                      end
                  val venv_frm = foldr addParams venv_fs (ListPair.zip (params, tl (Tr.formals new_level)))

                  val res = getResult result
                  val (body_e, body_ty) = transExp loop break_lab new_level venv_frm tenv body
                in
                  Tr.procEntryExit{level = new_level, body = body_e};
                  compatType (res, body_ty, pos, "function's declared type does not match its definition's type", "declared type", "definition's type");
                  body_ty
                end
              
              fun checkDups ([] : A.fundec list) = ()
                | checkDups ({name = n, pos, ...} :: ds) =
                    if List.exists (fn ({name = dn, ...} : A.fundec) => n = dn) ds
                        then ErrorMsg.error pos ("DUPLICATE: Mutually recursive functions with duplicate name " ^ S.name n)
                        else ()
            in
              checkDups fs;
              map checkBody fs;
              { exps = [], venv = venv_fs, tenv = tenv }
            end
         | A.VarDec { name, typ, init, escape, pos} =>
            let 
              val (init_e, init_ty) = transExp loop break_lab level venv tenv init

              val loc = Tr.allocLocal level (!escape)
              val lval = Tr.simpleVar(loc, level)
              val assign = Tr.assignExp(lval, init_e)
            in
            case typ
              of SOME (ty_s, ty_s_pos) =>
                let
                  val annot_ty = lookTy tenv ty_s ty_s_pos
                  val sel_ty = selectType annot_ty init_ty
                  val ventry = E.VarEntry { access = loc, ty = sel_ty, readonly = false }
                in
                (Tr.printAccess name loc;
                  compatType(annot_ty, init_ty, pos, "annotated type does not match expression's type", "annotation type", "expression type");
                 { exps = [assign], venv = Symbol.enter(venv, name, ventry), tenv = tenv })
                end
               | NONE => 
                  ((case init_ty
                      of T.NIL => ErrorMsg.error pos "TYPE: type of nil cannot be inferred" 
                      | _ => ());
                  let
                    val ventry = E.VarEntry { access = loc, ty = init_ty, readonly = false }
                  in
                  Tr.printAccess name loc;
                  { exps = [assign], venv = Symbol.enter(venv, name, ventry), tenv = tenv}
                  end)
            end
         | A.TypeDec ts =>
            let
              val tnames = map (fn { name, ty, pos } => (name, pos, T.NAME(name, ref NONE))) ts
              fun addPrelim ((name, _, tn), table) = Symbol.enter(table, name, tn)
              val tenv_prelim = foldr addPrelim tenv tnames

              fun addFinal ({ name, ty, pos }, table) = Symbol.enter(table, name, transTy tenv_prelim ty)
              val tenv_final = foldr addFinal tenv ts

              fun checkDups ([]) = ()
                | checkDups ({name = n, pos, ty = _} :: ds) =
                    if List.exists (fn ({name = dn, pos = _, ty = _}) => n = dn) ds
                        then ErrorMsg.error pos ("DUPLICATE: Mutually recursive types with duplicate name " ^ S.name n)
                        else ()

              fun setRef [] = ()
                | setRef ((name, pos, T.NAME(n, r)) :: tss) =
                  let
                    fun dig dn seen =
                      case S.look(tenv_final, dn)
                        of SOME (T.NAME (new_dn, _)) =>
                              (case !r
                                of SOME ty => SOME ty
                                 | NONE => case List.find (fn n => new_dn = n) seen
                                            of SOME bn => (ErrorMsg.error pos ("LOOP: recursive type cycle - " ^ S.name bn); NONE)
                                             | NONE => let val rt = dig new_dn (new_dn :: seen) in r := rt; rt end)
                         | t => (r := t; t)
                  in
                  dig name []; setRef tss
                  end
            in
            setRef tnames; checkDups ts; { exps = [], venv = venv, tenv = tenv_final }
            end
  
  and transTy                (tenv : tenv) (t : A.ty) : T.ty = 
    let
      fun trty (A.NameTy (n, p)) = (case S.look(tenv, n)
                                    of SOME t => t
                                     | NONE => (ErrorMsg.error p ("SCOPE: unrecognized type " ^ S.name n); T.BOTTOM))
        | trty (A.RecordTy fs) =
          let
            fun con ({name, typ, pos, ...} : A.field) =
              case S.look(tenv, typ)
                of SOME t => (name, t)
                 | NONE => (ErrorMsg.error pos ("SCOPE: unrecognized field type"); (name, T.BOTTOM))
          in
          T.RECORD (map con fs, ref ())
          end
        | trty (A.ArrayTy (s, pos)) =
          case S.look(tenv, s)
            of SOME t => T.ARRAY(t, ref ())
              | NONE => (ErrorMsg.error pos ("SCOPE: unrecognized type"); T.ARRAY(T.BOTTOM, ref()))
    in
    trty t
    end

  fun transProg e =
    let
      val level = Tr.newLevel { parent = Tr.outermost, name = Temp.namedlabel "tigermain" , formals = []}
      val (e, _) = transExp NotLoop NONE level E.base_venv E.base_tenv e
    in
    Tr.procEntryExit {level = level, body = e}
    end
end

(* 
structure Main = 
struct
  fun comp fileName =
    let
      val _ = Tr.frags := []
      val absyn = Parse.parse fileName
      val _ = FindEscape.findEscape absyn
      val (tree, ty) = Semant.transProg absyn
      val tree_stm = Tr.toStm tree
    in
    Printtree.printtree(TextIO.stdOut, tree_stm);
    app (fn fr => case fr
                    of Tr.PROC { body, frame } =>
                        (print (Symbol.name (X86Frame.name frame));
                        app (fn f => Printtree.printtree
                                           (TextIO.stdOut,
                                            Tree.EXP (X86Frame.exp f (Tree.TEMP X86Frame.FP)))
                            ) (X86Frame.formals frame);
                        print " = ";
                        Printtree.printtree(TextIO.stdOut, body);
                        print "\n")
                     | Tr.STRING (l, s) => print (Symbol.name l ^ " = \"" ^ s ^ "\"\n")) (!Tr.frags);
    print "\nIntepreter Output:\n";
    Interpret.interpret tree_stm;
    print "\n----\n";
    (tree, ty)
    end
  fun compile (_, [fileName]) = (comp fileName; OS.Process.success)
    | compile _ = (print "Incorrect arguments: pass a filename"; OS.Process.failure)
end
*)