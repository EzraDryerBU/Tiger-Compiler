structure T = Types

structure Semant =
struct
  type venv = Env.enventry Symbol.table
  type tenv = T.ty Symbol.table

  (*Keep track of loop depth*)
  val nestDepth : int ref = ref 0

  fun incrementNestDepth () = nestDepth := !nestDepth + 1
  fun decrementNestDepth () = nestDepth := !nestDepth - 1
  fun setNestDepth (n) = nestDepth := n
  fun checkInLoop (pos, errorMsg) =
    if (!nestDepth = 0)
    then ErrorMsg.error pos errorMsg
    else ()

  fun checkInt (pos : A.pos) (ty : T.ty) =
    case ty of 
        T.INT => ()
        | _ => ErrorMsg.error pos "TYPE: Expected Int, got other type";
  
  (*: T.ty * T.ty * A.pos * string*)
  fun checkMatchingTypes (ty1, ty2, pos, mssg) : bool = 
    case (ty1, ty2) of
      (T.RECORD(_, u1), T.RECORD(_, u2)) => if u1 = u2 then true
            else (ErrorMsg.error pos mssg; false)
    | (T.RECORD(_), T.NIL) => true
    | (T.NIL, T.RECORD(_)) => true
    | (T.NIL, T.NIL) => true
    | (T.INT, T.INT) => true
    | (T.STRING, T.STRING) => true
    | (T.ARRAY(_, u1), T.ARRAY(_, u2)) => if u1 = u2 then true
      else (ErrorMsg.error pos mssg; false)
    | (T.UNIT, _) => true
    | (_, T.UNIT) => true
    | (T.BOTTOM, _) => true
    | (_, T.BOTTOM) => true
    | (T.NAME(sym1, _), T.NAME(sym2, _)) => if (String.compare(Symbol.name sym1, Symbol.name sym2) = EQUAL)
    then true else (ErrorMsg.error pos mssg; false)
    | (_, _) => (ErrorMsg.error pos mssg; false)

  fun checkMatchingTypesNoMssg (ty1 : T.ty) (ty2 : T.ty) (pos : A.pos) : bool = 
    case (ty1, ty2) of
      (T.RECORD(_, u1), T.RECORD(_, u2)) => if u1 = u2 then true
            else (false)
    | (T.RECORD(_), T.NIL) => true
    | (T.NIL, T.RECORD(_)) => true
    | (T.NIL, T.NIL) => true
    | (T.INT, T.INT) => true
    | (T.STRING, T.STRING) => true
    | (T.ARRAY(_, u1), T.ARRAY(_, u2)) => if u1 = u2 then true
      else (false)
    | (T.UNIT, _) => true
    | (_, T.UNIT) => true
    | (T.BOTTOM, _) => true
    | (_, T.BOTTOM) => true
    | (T.NAME(sym1, _), T.NAME(sym2, _)) => if (String.compare(Symbol.name sym1, Symbol.name sym2) = EQUAL)
    then true else (false)
    | (_, _) => (false)

  fun checkComparison (pos : A.pos) (ty1 : T.ty) (ty2 : T.ty) = 
    case (ty1, ty2) of
            (T.RECORD(_, u1), T.RECORD(_, u2)) => if u1 = u2 then () 
            else ErrorMsg.error pos "TYPE: Attempting to compare Records of different types"
          | (T.RECORD(_), NIL) => ()
          | (NIL, T.RECORD(_)) => ()
          | (T.INT, T.INT) => ()
          | (T.STRING, T.STRING) => ()
          | (T.ARRAY(_, u1), T.ARRAY(_, u2)) => if u1 = u2 then () 
            else ErrorMsg.error pos "TYPE: Attempting to compare Arrays of different types"
          | (T.UNIT, T.UNIT) => ()
          | (T.NIL, T.NIL) => ErrorMsg.error pos "TYPE: Attempting to compare two NIL values"
          | (_, _) => ErrorMsg.error pos "TYPE: Attempting to compare two values of different types"


  fun lookupTy (pos : A.pos) (ty_sym : A.symbol) (tenv : tenv) : T.ty =
      case Symbol.look (tenv, ty_sym)
        of SOME (T.NAME (_, r)) =>
              (case !r
                of SOME ty => ty
                  | NONE => (ErrorMsg.error pos ("SCOPE: Did not recognize type " ^ Symbol.name ty_sym); T.BOTTOM))
         | SOME ty => ty
         | NONE => (ErrorMsg.error pos ("SCOPE: Did not recognize type " ^ Symbol.name ty_sym); T.BOTTOM)

  fun checkIntOrString (pos : A.pos) (ty1 : T.ty) (ty2 : T.ty) =
    case (ty1, ty2) of
        (T.INT, T.INT) => ()
      | (T.STRING, T.STRING) => ()
      | (_, _) => (ErrorMsg.error pos "TYPE: Expected two values of both String or both Int, got other types")




  fun transVar (venv : venv) (tenv : tenv) (v : A.var) : T.ty = 
    case v of
        A.SimpleVar(symb, pos) => 
          (case Symbol.look(venv, symb) of
            SOME(Env.VarEntry{ty, writable=_}) => ty
            | SOME(Env.FunEntry{formals, result=r}) => r
            | _ => (ErrorMsg.error pos "SCOPE: Variable not found"; T.BOTTOM))
      | A.FieldVar(v, symb, pos) => 
          (case transVar venv tenv v of
            T.RECORD(symTyList, _) =>
              let 
                fun checkRecField ((s1, t1)::rest1 : (Symbol.symbol * T.ty) list) (s2 : Symbol.symbol) (pos) : T.ty =
                 (case (s1 = s2) of
                    true => t1
                    | false => (checkRecField rest1 s2 pos))
                | checkRecField ([]) (s2) (pos) = 
                  (ErrorMsg.error pos "SCOPE: Attempting to access non-exsisting field of record type"; T.BOTTOM)
              in
                checkRecField symTyList symb pos
              end
            | _ => (ErrorMsg.error pos "SCOPE: Attempting to access field of non-record type"; T.BOTTOM))
      
      | A.SubscriptVar(v, exp, pos) => 
          (case transVar venv tenv v of
            T.ARRAY(t, u) => (checkInt pos (transExp venv tenv exp); t)
            | _ => ((ErrorMsg.error pos "SCOPE: Attempting to index a non-array type"; T.BOTTOM)))

  and transExp (venv : venv) (tenv : tenv) (e : A.exp) = 
    let 
      fun trexp (A.OpExp{left, oper, right, pos} : A.exp) : T.ty = 
        (case oper of
          A.PlusOp => ((checkInt pos (trexp left));
                       (checkInt pos (trexp right)); T.INT)
          | A.MinusOp => ((checkInt pos (trexp left));
                          (checkInt pos (trexp right)); T.INT)
          | A.TimesOp => ((checkInt pos (trexp left));
                          (checkInt pos (trexp right)); T.INT)
          | A.DivideOp => ((checkInt pos (trexp left));
                           (checkInt pos (trexp right)); T.INT)
          | A.EqOp => ((checkComparison pos (trexp right) (trexp left)); T.INT)
          | A.NeqOp => ((checkComparison pos (trexp right) (trexp left)); T.INT)
          | A.LtOp => ((checkIntOrString pos (trexp right) (trexp left)); T.INT)
          | A.LeOp => ((checkIntOrString pos (trexp right) (trexp left)); T.INT)
          | A.GtOp => ((checkIntOrString pos (trexp right) (trexp left)); T.INT)
          | A.GeOp => ((checkIntOrString pos (trexp right) (trexp left)); T.INT))
      | trexp (A.VarExp(var)) = (transVar venv tenv var)
      | trexp (A.NilExp) = T.NIL
      | trexp (A.IntExp _) = T.INT
      | trexp (A.StringExp _) = T.STRING
      | trexp (A.CallExp{func, args, pos}) = 
        (case Symbol.look (venv, func) of
          SOME (Env.FunEntry{formals, result}) =>  
            (let 
              fun checkArgs (f::formalTys : T.ty list) (a::argExps : A.exp list) (pos) = (
                  checkMatchingTypes (f, (trexp a), pos, "SCOPE: Mismatched formal and argument type");
                  (checkArgs formalTys argExps pos))
              | checkArgs ([]) (a::argExps) pos = (ErrorMsg.error pos "SCOPE: Giving more arguments than expected for this function"; T.BOTTOM)
              | checkArgs (f::formalTys) ([]) pos = (ErrorMsg.error pos "SCOPE: Giving less arguments than expected for this function"; T.BOTTOM)
              | checkArgs ([]) ([]) pos = result
            in
              checkArgs formals args pos
            end)
          | SOME(_) => (ErrorMsg.error pos "SCOPE: symbol is not a function"; T.BOTTOM)
          | NONE => (ErrorMsg.error pos "SCOPE: function is out of scope"; T.BOTTOM))
      | trexp (A.RecordExp{fields, typ, pos}) = 
        (case Symbol.look (tenv, typ) of
          SOME(r) => (
            case r of
              T.RECORD(symAndTypes, _) => 
                let
                  fun checkFields ((s1, t1)::rest1 : (Symbol.symbol * T.ty) list) 
                  ((s2, e2, p)::rest2 : (Symbol.symbol * A.exp * A.pos) list) (pos)=
                    (case ((s1 = s2) andalso (t1 = (trexp e2))) of
                        true => (checkFields rest1 rest2 pos)
                        | false => (ErrorMsg.error pos "SCOPE: Mismatching fields in record"; T.BOTTOM))
                  | checkFields ([]) ((s2, e2, p)::rest2) (pos) =
                    (ErrorMsg.error pos "SCOPE: More fields in record than expected"; T.BOTTOM)
                  | checkFields ((s1, t1)::rest1) ([]) (pos) =
                    (ErrorMsg.error pos "SCOPE: Less fields in record than expected"; T.BOTTOM)
                  | checkFields ([]) ([]) (pos) = r
                in
                  checkFields symAndTypes fields pos
                end
              | _ => (ErrorMsg.error pos "SCOPE: Specified variable is not a record"; T.BOTTOM))
          | NONE => (ErrorMsg.error pos "SCOPE: Could not find specified record type"; T.BOTTOM))
      | trexp (A.SeqExp(expList)) = 
        let
          fun evalSeq ((e,p)::eList) = (trexp e; evalSeq eList)
          | evalSeq ([]) = T.UNIT
        in
          evalSeq expList
        end
      | trexp (A.AssignExp{var, exp, pos}) = (   
          let
            fun getActualEntry (v) (varEnv) (pos) = (
              case v of
                A.SimpleVar(sym, _) => Symbol.look(varEnv, sym)
                | A.FieldVar(v1, _, _) => getActualEntry v1 varEnv pos
                | A.SubscriptVar(v1, _, _) => getActualEntry v1 varEnv pos
            )
            fun getWritable (v) (varEnv) (pos) = (
              case getActualEntry v varEnv pos of
                SOME(Env.VarEntry({ty, writable})) => (
                  case writable of
                    true => ()
                    | false => ErrorMsg.error pos "READONLY: Can't assign to loop iterator")
                | _ => (ErrorMsg.error pos "SCOPE: Attempting to assign to an unknown variable")
            )
          in
            getWritable var venv pos;
            checkMatchingTypes ((transVar venv tenv var), (trexp exp), (pos),
            ("TYPE: Attempting to assign to a varaible of a differnt type"));
            T.UNIT
          end 
      )
        (*May need to check that exp is T.UNIT, rather than just return that*)
      | trexp (A.IfExp{test, then', else', pos}) = (
        case (checkMatchingTypesNoMssg (trexp test) T.INT pos) of 
          true => 
           (case else' of 
              SOME(elseExp) => (
                case (checkMatchingTypes ((trexp then'), (trexp elseExp), (pos), 
                "TYPE: Types of then and else expression need to be the same")) of
                  true => (trexp then') 
                  | false => T.BOTTOM )
              | NONE => trexp then'; T.UNIT)
          | false => (ErrorMsg.error pos "TYPE: Expected expression of type int when evaluating test for if statement 1"; T.BOTTOM)
      )
      | trexp (A.WhileExp{test, body, pos}) = (
          case (trexp test) of
            T.INT => (incrementNestDepth (); trexp body; decrementNestDepth(); T.UNIT)
            | _ => (ErrorMsg.error pos "TYPE: Expected expression of type int when evaluating test for while statement"; T.BOTTOM)
      )
      | trexp (A.ForExp {var, escape, lo, hi, body, pos}) = (
          let
            val forVenv = Symbol.enter (venv, var, Env.VarEntry{ty=T.INT, writable=false}) 
          in
            checkInt pos (trexp lo);
            checkInt pos (trexp hi);
            incrementNestDepth();
            transExp forVenv tenv body;
            decrementNestDepth();
            T.UNIT
          end
      )
      | trexp (A.BreakExp(pos)) = (
          if !nestDepth = 0 
          then (ErrorMsg.error pos "MISPLACED: Break expression not in loop"; T.UNIT)
          else T.UNIT
      )
      | trexp (A.LetExp{decs, body, pos}) = (
          let
            val tempNestDepth = !nestDepth
            val _ = setNestDepth(0)
            val {venv=newVenv, tenv=newTenv} = transDec venv tenv decs
            val _ = setNestDepth(tempNestDepth)
            val ret = transExp newVenv newTenv body
          in
            ret
          end
      )
      | trexp (A.ArrayExp{typ, size, init, pos}) = (
          let
            fun getArrayType (SOME(ty)) = ty
            | getArrayType (NONE) = T.BOTTOM
            fun actualType t =
              case t of
                T.NAME(symb, tyRef) => actualType(getArrayType(Symbol.look(tenv, symb)))
                | someTy => someTy
          in
            (
            case Symbol.look(tenv, typ) of
              SOME(t1) => (
                case actualType t1 of
                  T.ARRAY(aType, unq) => (
                    let
                      fun getArrayType (SOME(ty)) = ty
                      | getArrayType (NONE) = T.BOTTOM
                      fun actualType t =
                        case t of
                          T.NAME(symb, tyRef) => actualType(getArrayType(Symbol.look(tenv, symb)))
                          | someTy => someTy                      
                    in
                      (checkInt pos (trexp size);
                      case checkMatchingTypes ((actualType aType), (trexp init), pos,
                       "TYPE: Attempting to initialize array to value of different type") of
                      true => (T.ARRAY(aType, unq))
                      | false => (T.BOTTOM))
                    end
                  )
                  | _ => (ErrorMsg.error pos "TYPE: Actual type of type alias is not actually an array"; T.BOTTOM)
              )
              | NONE => (ErrorMsg.error pos "scope: Attempting to use unknown type"; T.BOTTOM)
            )
          end
      )
    in
      trexp e
    end
  
  and transDec (venv : venv) (tenv : tenv) (d : A.dec list) : { venv : venv, tenv : tenv} =
    let
      fun trdec (varEnv) (typeEnv) (A.VarDec{name, escape, typ, init, pos}) = (
      (*getArrayType and actualType work to drill down through aliases to get a basic type*)
        let
           fun getArrayType (SOME(ty)) = ty
            | getArrayType (NONE) = (
              (ErrorMsg.error 0 "SCOPE: Type alias not recognized");
              T.BOTTOM)
            fun actualType t =
              case t of
                T.NAME(symb, tyRef) => (actualType(getArrayType(Symbol.look(tenv, symb))))
                | someTy => someTy
        in 
        (
          (*Get the type from the VarDec*)
          case typ of
            SOME(s, pos1) => (
              case Symbol.look(typeEnv, s) of
                SOME(ty1) => (
                  (checkMatchingTypes (actualType ty1, (transExp venv tenv init), pos1, 
                  "TYPE: Type used in variable declaration does not match type of init expression"));
                  {venv=Symbol.enter(varEnv, name, Env.VarEntry{ty=ty1, writable=true}), tenv=typeEnv}
                )
                | NONE => (ErrorMsg.error pos "SCOPE: Attempting to use unknown type1"; {venv=varEnv, tenv=typeEnv})
            )
            | NONE => (
              let
                val inferedType = transExp venv tenv init
              in
                (case (checkMatchingTypesNoMssg inferedType T.NIL pos) of
                  true => (ErrorMsg.error pos "TYPE: Can not initialize to nil without explicitly declaring type";
                  {venv=Symbol.enter(varEnv, name, Env.VarEntry{ty=T.BOTTOM, writable=true}), tenv=typeEnv})
                  | false => ({venv=Symbol.enter(varEnv, name, Env.VarEntry{ty=inferedType, writable=true}), tenv=typeEnv})) 
              end
            )
        )
        end
      )
      | trdec (varEnv) (typeEnv) (A.FunctionDec(fundeclist)) = (
          (*Takes a A.field record and checks the type is valid, the returns the type from the table and the name*)
          let 
            fun checkParams ({name, escape, typ, pos}) = (
              case Symbol.look(typeEnv, typ) of
                SOME(t) => (name, t)
                | NONE => (ErrorMsg.error pos "SCOPE: Type of parameter not recognized"; (name, T.BOTTOM))
            )
            (*Similar to above, takes a symbol and returns the type or error*)
            fun checkReturn retType pos = (
              case Symbol.look(typeEnv, retType) of
                SOME(rt) => rt
                | NONE => (ErrorMsg.error pos "SCOPE: Return type not recognized"; T.BOTTOM)
            )
            fun stripType (name, ty) = ty
            (*Takes a single funcdec and enters into the enviroment, needs to also deal with procedures*)
            (*Uses map on params, which should be a list of fields, and checkParams, which returns a type, to get a list of types*)
            fun enterFuncs ({name, params, result=SOME(rt, retPos), body, pos}, varEnvi) = (
                  Symbol.enter(varEnvi, name, Env.FunEntry({formals=(map stripType (map checkParams params)), result=(checkReturn rt retPos)})))
            | enterFuncs ({name, params, result=NONE, body, pos}, varEnvi) = (
                Symbol.enter(varEnvi, name, Env.FunEntry({formals=(map stripType (map checkParams params)), result=T.UNIT})))
            (*Here we start with the given var Enviroment, and the list of funcdecs, and have it return a new var Enviroment*)
            val newVarEnv = foldr enterFuncs varEnv fundeclist
            (*Get a fundec and determine if it is a procedure or not, check return type
            check all of the parameters, get a list of (name, type) tuples
            enter each parameter into a new var enviroment
            call transExp on body of the function and store its type
            *)
              fun checkfundec({name, params, result, body, pos}) = (
                let 
                  val resultType = 
                    (case result of
                      SOME(rt, resPos) => checkReturn rt resPos
                      | NONE => T.UNIT)
                  val checkedParams = map checkParams params
                  fun enterparam ((name, ty), venv1) = Symbol.enter(venv1, name, Env.VarEntry{ty=ty, writable=true})
                  val funcVenv = foldl enterparam newVarEnv checkedParams
                  val actualBodyType = transExp (funcVenv) (typeEnv) (body)
                in
                  case (checkMatchingTypesNoMssg actualBodyType resultType pos) of
                  true => ()
                  | false => (ErrorMsg.error pos "TYPE: Function returns the wrong type")
                end
              )
            (*Takes a fundec and unit and returns a unit, just for checking through the function declaration*)
            fun foldfundec (fundec, ()) = checkfundec fundec
            (*Checks through all the given funcdecs and if it is new adds it to the seenFuncs, if it is already
            in the seenFuncs it prints an error and moves on*)
            fun checkDuplicates({name, params, result, body, pos}, seenFuncs) = 
                case List.exists (fn y => String.compare(Symbol.name name, y) = EQUAL) seenFuncs of
                true => (ErrorMsg.error pos "DUPLICATE: two types of same name in mutually recursive fundec"; seenFuncs)
                | false => ((Symbol.name name)::seenFuncs)
          in
          (*List of seen funcs starts empty*)
            foldl checkDuplicates [] fundeclist;
            foldr foldfundec () fundeclist;
            {venv=newVarEnv, tenv=typeEnv}
          end
      )
      | trdec (varEnv) (typeEnv) (A.TypeDec(tyDecList)) = (
          (*Dummy function for empty decs to start*)
          let
            fun getEmptyDec (({name, ty, pos}), tenv1) = Symbol.enter(tenv1, name, T.BOTTOM)
            (*Get a new type enviroment with all the names of the types, but none of the actual types*)
            
            val tempTyEnv = foldl getEmptyDec typeEnv tyDecList
            (*Get the actual types filled in*)
            fun foldTyDec ({name, ty, pos}, (venv, tenv)) = (venv, Symbol.enter(tenv, name, (transTy tempTyEnv ty)))
            val newEnv = foldl foldTyDec (varEnv, typeEnv) tyDecList
            fun convertTupToRec (varEnv, tyEnv) = {venv=varEnv, tenv=tyEnv}
            fun stripTenv (venv, tenv) = tenv
            (*Looks for illegal cycles in a similar fashion to functions*)
            fun findIllegalCycle({name, ty, pos}, ()) = 
              let
                fun checkHelper(seenList, name) = (
                  (*Look to see if the name is in the tenv, if it is check to see if it is already seen
                  and if it is it is directly recursive. Function will eventually return unit*)
                  case Symbol.look((stripTenv newEnv), name) of
                    SOME(T.NAME(symb, _)) => (
                      case List.exists (fn y => String.compare(Symbol.name symb, Symbol.name y) = EQUAL) seenList of
                      true => (ErrorMsg.error pos "LOOP: directly mutually recursive types")
                      | false => (checkHelper(name::seenList, symb)))
                    | _ => ()
                  )
              in
                checkHelper([], name)
              end

              (*Just runs through to make sure you don't declare a type of the same name twice*)
              fun checkDuplicates({name, ty, pos}, seenList) = 
                  if List.exists (fn y => String.compare(Symbol.name name, y) = EQUAL) seenList
                  then (ErrorMsg.error pos "DUPLICATE: two types of same name in mutually recursive type declaration"; seenList)
                  else (Symbol.name name)::seenList

            in
              foldl checkDuplicates [] tyDecList;
              foldl findIllegalCycle () tyDecList;
              convertTupToRec newEnv
            end
      )
      and folddec (dec, ({venv, tenv})) = trdec (venv) (tenv) (dec)
    in
       foldl folddec {venv=venv, tenv=tenv} d
    end

  and transTy (tenv : tenv) (e : A.ty) : T.ty = 
    let 
      fun trty(tenv, A.NameTy(name, pos)) =
            (case Symbol.look(tenv, name) of
              SOME _ => T.NAME(name, ref(NONE))
              | NONE => (ErrorMsg.error pos ("SCOPE: Unrecognized name type: " ^ Symbol.name name); 
              T.NAME(name, ref(NONE)))
            )  
          | trty(tenv, A.RecordTy(fields)) =
          (*Check through all the fields to make sure thier types are in scope*)
                let 
                    fun fieldProcess {name, escape, typ, pos} =
                        case Symbol.look(tenv, typ) of
                          SOME(t) => (name, t)
                          | NONE => (ErrorMsg.error pos ("SCOPE: undefined type in rec: " ^ Symbol.name typ); 
                          (name, T.BOTTOM)) 
                    (*This is messed up*) 
                    fun listConcat(a, b) = (fieldProcess(a))::b
                    fun generateRec () = foldl listConcat [] fields
                in 
                    generateRec();
                    T.RECORD(generateRec(), ref ())
                end
          | trty(tenv, A.ArrayTy(sym, pos1)) =
                T.ARRAY(transTy (tenv) (A.NameTy(sym, pos1)), ref ())
        in
            trty(tenv, e)
        end


  fun transProg e = transExp Env.base_venv Env.base_tenv e
end

structure Main = 
struct
  fun comp fileName = Semant.transProg (Parse.parse fileName)
  fun compile (_, [fileName]) = (comp fileName; OS.Process.success)
    | compile _ =  OS.Process.failure
end