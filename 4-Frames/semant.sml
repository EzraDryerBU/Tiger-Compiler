(* structure A = Absyn *)
structure T = Types
(* structure S = Symbol *)
structure E = Env

structure Semant =
struct
  type venv = E.enventry S.table
  type tenv = T.ty S.table

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
                of SOME (E.VarEntry {readonly = true, ... }) =>
                      ErrorMsg.error pos ("READONLY: readonly variable " ^ S.name s ^ " assigned to")  
                 | _ => ())
    | checkReadOnly _ _ _ = ()

  fun transVar (loop : loopstat) (venv : venv) (tenv : tenv) (v : A.var) (l : Tr.level): T.ty =
      let
        fun trvar (A.SimpleVar (s, pos)) = 
              (case S.look(venv, s)
                of SOME (E.VarEntry {ty, ... }) => actualTy tenv ty pos
                | SOME (E.FunEntry {result, ...}) => (ErrorMsg.error pos ("SCOPE: function symbol " ^ S.name s ^ " used as a variable"); result)
                | NONE => (ErrorMsg.error pos ("SCOPE: unrecognized variable " ^ S.name s); T.BOTTOM))
          | trvar (A.FieldVar(v, sym, pos)) =
              (case trvar v
                of T.RECORD (fields, _) =>
                    (case List.find (fn (s, _) => s = sym) fields
                      of SOME (_, ty) => actualTy tenv ty pos
                       | NONE => (ErrorMsg.error pos ("SCOPE: unrecognized record field " ^ S.name sym
                       ); T.BOTTOM))
                 | t => (ErrorMsg.error pos ("SCOPE: unrecognized record " ^ T.printType t); T.BOTTOM))
          | trvar (A.SubscriptVar(v, exp, pos)) =
              (checkInt (transExp loop venv tenv exp l) "array subscript" pos;
                case trvar v
                  of T.ARRAY (ty, _) => actualTy tenv ty pos
                   | _ => (ErrorMsg.error pos ("SCOPE: unrecognized array"); T.BOTTOM))

      in
      trvar v
      end

  and transExp (loop : loopstat) (venv : venv) (tenv : tenv) (e : A.exp) (l : Tr.level): T.ty =
  let
    fun trexp (A.VarExp v) = transVar loop venv tenv v l
      | trexp A.NilExp = T.NIL
      | trexp (A.IntExp _) = T.INT
      | trexp (A.StringExp _) = T.STRING
      | trexp (A.CallExp {func, args, pos }) = 
        (case S.look (venv, func)
           of SOME (E.FunEntry {formals, result, level, label}) =>
              let
                  fun checkArgs ([] : A.exp list) ([] : T.ty list) = ()
                    | checkArgs [] (_ :: _) = ErrorMsg.error pos ("SCOPE: too few arguments for function " ^ S.name func)
                    | checkArgs (_ :: _) [] = ErrorMsg.error pos ("SCOPE: too many arguments for function " ^ S.name func)
                    | checkArgs (e :: es) (f :: fs) =
                      let val te = trexp e in
                      compatType(f, te, pos, "incompatible function argument type", "expected", "passed")
                      end
              in
              case Int.compare (List.length args, List.length formals)
                of EQUAL => (checkArgs args formals; result)
                 | LESS => (ErrorMsg.error pos "SCOPE: too few arguments passed for function"; result)
                 | GREATER => (ErrorMsg.error pos "SCOPE: too many arguments passed for function"; result)
              end
            | SOME (E.VarEntry {ty, ... }) => (ErrorMsg.error pos "SCOPE: incorrect use of variable as function"; ty)
            | NONE => (ErrorMsg.error pos "SCOPE: unrecognized function"; T.BOTTOM))

      | trexp (A.OpExp {left, oper = A.PlusOp, right, pos }) =
          (checkInt (trexp left) "operator" pos; checkInt (trexp right) "operator" pos; T.INT)
      | trexp (A.OpExp {left, oper = A.MinusOp, right, pos }) =
          (checkInt (trexp left) "operator" pos; checkInt (trexp right) "operator" pos; T.INT)
      | trexp (A.OpExp {left, oper = A.TimesOp, right, pos }) =
          (checkInt (trexp left) "operator" pos; checkInt (trexp right) "operator" pos; T.INT)
      | trexp (A.OpExp {left, oper = A.DivideOp, right, pos }) =
          (checkInt (trexp left) "operator" pos; checkInt (trexp right) "operator" pos; T.INT)
      | trexp (A.OpExp {left, oper = A.EqOp, right, pos }) = (checkEqOp (trexp left) (trexp right) pos; T.INT)
      | trexp (A.OpExp {left, oper = A.NeqOp, right, pos }) = (checkEqOp (trexp left) (trexp right) pos; T.INT)
      | trexp (A.OpExp {left, oper = A.LtOp, right, pos }) = (checkCompOp (trexp left) (trexp right) pos; T.INT)
      | trexp (A.OpExp {left, oper = A.LeOp, right, pos }) = (checkCompOp (trexp left) (trexp right) pos; T.INT)
      | trexp (A.OpExp {left, oper = A.GtOp, right, pos }) = (checkCompOp (trexp left) (trexp right) pos; T.INT)
      | trexp (A.OpExp {left, oper = A.GeOp, right, pos }) = (checkCompOp (trexp left) (trexp right) pos; T.INT)

      | trexp (A.RecordExp {fields, typ, pos }) =
        (case lookTy tenv typ pos
          of T.RECORD (fs, u) =>
              let 
                  fun checkFields ([] : (S.symbol * A.exp * A.pos) list) ([] : (S.symbol * T.ty) list) = ()
                    | checkFields [] (_ :: _) = ErrorMsg.error pos "SCOPE: missing fields for record"
                    | checkFields (_ :: _) [] = ErrorMsg.error pos "SCOPE: too many fields for record"
                    | checkFields ((s1, e, p) :: es) ((s2, t) :: fs) =
                      (if s1 = s2 then () else ErrorMsg.error p "SCOPE: mismatched record field names";
                       let val te = trexp e in
                          compatType(actualTy tenv t pos, te, p, "incorrect type in record", "field's type", "expression's type")
                       end)
              in
              checkFields fields fs; T.RECORD (fs, u)
              end
           | _ => (ErrorMsg.error pos "SCOPE: expected record but got other symbol"; T.BOTTOM))
      | trexp (A.SeqExp es) =
          let
              fun walk [] = T.UNIT
                | walk [(e, _)] = trexp e
                | walk ((e, _) :: es) = (trexp e; walk es)
          in
          walk es
          end
      | trexp (A.AssignExp {var, exp, pos }) =
        (compatType(trexp exp, transVar loop venv tenv var l, pos, "incorrect types in assignment", "bindee's type", "bound's type");
         checkReadOnly pos venv var;
         T.UNIT)
      | trexp (A.IfExp {test, then', else' = NONE, pos}) = 
              let
                val then_ty = trexp then'
              in
              checkInt (trexp test) "if test" pos;
              checkUnit then_ty "if expression" pos;
              then_ty
              end
      | trexp (A.IfExp {test, then', else' = SOME e, pos}) =
              (checkInt (trexp test) "if test" pos;
              let
                val then_ty = trexp then'
                val else_ty = trexp e
              in
                (compatType(then_ty, else_ty, pos, "if expression branches incompatible", "then type", "else type");
                selectType then_ty else_ty)
              end)
      | trexp (A.WhileExp {test, body, pos}) =
        (checkInt (trexp test) "while test" pos;
         checkUnit (transExp InLoop venv tenv body l) "while body" pos;
         T.UNIT)
      | trexp (A.ForExp {var, escape, lo, hi, body, pos}) =
        let
          val acc = (Tr.allocLocal l (!escape))
          val venv' = S.enter(venv, var, E.VarEntry {ty = T.INT, readonly = true, access=acc})
        in
        (checkInt (trexp lo) "for lower bound" pos;
         checkInt (trexp hi) "for upper bound" pos;
         checkUnit (transExp InLoop venv' tenv body l) "for body" pos;
         Tr.printAccess var (acc);
         T.UNIT)
        end
      | trexp (A.BreakExp pos) = 
        (case loop
          of InLoop => ()
           | NotLoop => ErrorMsg.error pos "MISPLACED: break used outside loop";
           T.UNIT)
      | trexp (A.LetExp {decs, body, pos}) =
          let
              fun walk vt [] = vt
                | walk {venv, tenv} (d :: ds) = walk (transDec loop venv tenv d l) ds
              val {venv = venv2, tenv = tenv2 } = walk {venv = venv, tenv = tenv} decs
          in
          transExp loop venv2 tenv2 body l
          end
      | trexp (A.ArrayExp {typ, size, init, pos }) =
        (checkInt (trexp size) "array size" pos;
        case lookTy tenv typ pos
          of T.ARRAY (t, u) => (compatType(actualTy tenv t pos, trexp init, pos, "array initializer of incorrect type", "expected", "actual"); T.ARRAY (t, u))
           | _ => (ErrorMsg.error pos "SCOPE: expected array but got other symbol"; T.BOTTOM))
  in
    trexp e
  end
  
  and transDec (loop : loopstat) (venv : venv) (tenv : tenv) (d : A.dec) (l : Tr.level): {venv : venv, tenv : tenv} =
      case d
        of A.FunctionDec fs =>
            let
              fun getResult r = case r
                                  of SOME (r, p) => lookTy tenv r p
                                   | NONE => T.UNIT

              fun add ({name, params, result, body, pos } : A.fundec, table) =
                  let
                    val formals = map (fn {typ, pos, ... } => lookTy tenv typ pos) params
                    val res = getResult result
                  in
                    S.enter(table, name, E.FunEntry {formals = formals, result = res, level=l, label=Temp.newlabel() })
                  end
                
              val venv_fs = foldr add venv fs
              
              fun checkBody ({name, params, result, body, pos} : A.fundec) =
                let
                  fun getEscape({name, escape, typ, pos}) = (
                    !escape
                  )
                  fun getEscapeList p = map getEscape p
                  val lev = (Tr.newLevel {parent=l, name=Temp.newlabel(), formals=getEscapeList params})
                  val levForms = Tr.formals lev
                  val zipParamsForms = ListPair.zip (params, levForms)

                  fun addParams (({name, escape, typ, pos}, a): A.field * Tr.access, table) = (
                      S.enter(table, name, E.VarEntry {ty = lookTy tenv typ pos, readonly = false, access=a})
                      )
                  

                  val venv_frm = foldr addParams venv_fs zipParamsForms

                  val res = getResult result
                  val _ = Tr.printLevel name lev;
                  val body_ty = transExp loop venv_frm tenv body lev
                in
                 

                  compatType (res, body_ty, pos, "function's declared type does not match its definition's type", "declared type", "definition's type");
                  
                  body_ty
                end
              
              fun checkDups ([] : A.fundec list) = ()
                | checkDups ({name = n, pos, ...} :: ds) =
                    if List.exists (fn ({name = dn, ...} : A.fundec) => n = dn) ds
                        then ErrorMsg.error pos ("DUPLICATE: Mutually recursive functions with duplicate name " ^ S.name n)
                        else ()
            in
              checkDups fs; map checkBody fs; {venv = venv_fs, tenv = tenv }
            end
         | A.VarDec {name, escape, typ, init, pos} =>
            let 
              val init_ty = transExp loop venv tenv init l
              val acc = (Tr.allocLocal l (!escape))
            in
            case typ
              of SOME (ty_s, ty_s_pos) =>
                let
                  val annot_ty = lookTy tenv ty_s ty_s_pos
                  val sel_ty = selectType annot_ty init_ty
                in
                (compatType(annot_ty, init_ty, pos, "annotated type does not match expression's type", "annotation type", "expression type");
                Tr.printAccess name (acc);
                 {venv = Symbol.enter(venv, name, E.VarEntry {ty = sel_ty, readonly = false, access=(acc)}), tenv = tenv })
                end
               | NONE => 
                  ((case init_ty
                      of T.NIL => ErrorMsg.error pos "TYPE: type of nil cannot be inferred" 
                      | _ => ());
                    Tr.printAccess name (acc);
                  {venv = Symbol.enter(venv, name, E.VarEntry {ty = init_ty, readonly = false, access=(acc)}), tenv = tenv})
            end
         | A.TypeDec ts =>
            let
              val tnames = map (fn {name, ty, pos } => (name, pos, T.NAME(name, ref NONE))) ts
              fun addPrelim ((name, _, tn), table) = Symbol.enter(table, name, tn)
              val tenv_prelim = foldr addPrelim tenv tnames

              fun addFinal ({name, ty, pos }, table) = Symbol.enter(table, name, transTy tenv_prelim ty)
              val tenv_final = foldr addFinal tenv ts

              fun checkDups ([]) = ()
                | checkDups ({name = n, pos, ty = _} :: ds) =
                    if List.exists (fn ({name = dn, pos = _, ty = _}) => n = dn) ds
                        then ErrorMsg.error pos ("DUPLICATE: Mutually recursive types with duplicate name " ^ S.name n)
                        else checkDups ds

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
                  dig name []; checkDups ts; setRef tss
                  end
                | setRef(_) = (ErrorMsg.error 0 "error reahced on line 316 of semant.sml")
            in
            setRef tnames; checkDups ts; {venv = venv, tenv = tenv_final }
            end
  
  and transTy (tenv : tenv) (t : A.ty) : T.ty = 
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

  fun transProg e = transExp NotLoop E.base_venv E.base_tenv e (Tr.newLevel ({parent=Tr.outermost, name=Temp.newlabel(), formals=[]}))
end

structure Main = 
struct
  fun comp fileName = (
    let
      val prog = (Parse.parse fileName)
    in
      FindEscape.findEscape prog; 
      Semant.transProg (prog)
    end
    )
  fun compile (_, [fileName]) = (comp fileName; OS.Process.success)
    | compile _ =  OS.Process.failure

end