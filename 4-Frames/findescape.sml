structure Tr = Translate
structure A = Absyn
structure S = Symbol

structure FindEscape : sig val findEscape : Absyn.exp -> unit end =
struct
    type depth = int
    type escEnv = (depth * bool ref) Symbol.table

    (*Returns true if d1 is larger (deeper) than d2*)
    fun compareDepth (d1 : depth) (d2 : depth) : bool = (
        (d1) > d2
    )
    
    fun setFalse boolRef = boolRef := false

    fun traverseVar (env : escEnv) (d : depth) (v : A.var) : unit = (
        let
            fun travVar(A.SimpleVar(symb, pos) : A.var) : unit = (
               let
                val entry = S.look (env, symb)
               in
                case entry of
                    SOME(decDepth, escape) => (
                        case !escape of 
                            true => ()
                            | false => (escape := (compareDepth d decDepth))
                    )
                    | NONE => () (*Didn't find the var which is weird, semantic error probably*)
               end
            )
            | travVar(A.FieldVar(var, symb, pos)) = (
                travVar var
            )
            | travVar(A.SubscriptVar(var, e, pos)) = (
                travVar var;
                traverseExp env d e
            )
        in
            travVar v
        end
    )

    and traverseExp (env : escEnv) (d : depth) (e : A.exp) : unit = 
        let
            fun travExp (A.VarExp(var) : A.exp) : unit = (
                traverseVar env d var
            )
            | travExp(A.NilExp) = ()
            | travExp(A.IntExp(_)) = ()
            | travExp(A.StringExp(_,_)) = ()
            | travExp(A.CallExp{func, args, pos}) = (
                (app travExp args)
            )
            | travExp(A.OpExp{left, oper, right, pos}) = (
                travExp left; travExp right
            )
            | travExp(A.RecordExp{fields, typ, pos}) = (
                let
                    fun stripRecExp (s,e,pos) = (
                        travExp e
                    )
                in (app stripRecExp fields) end
            )
            | travExp(A.SeqExp(expList)) = (
                let
                    fun stripSeqExp (e, pos) = (
                        travExp e
                    )
                in (app stripSeqExp expList) end

            )
            | travExp(A.AssignExp{var, exp, pos}) = (
                traverseVar env d var; travExp exp
            )
            | travExp(A.IfExp{test, then', else', pos}) = (
                travExp test;
                travExp then';
                case else' of
                    SOME(e) => (travExp e)
                    | NONE => ()
            )
            | travExp(A.WhileExp{test, body, pos}) = (
                travExp test;
                travExp body
            )
            | travExp(A.ForExp{var, escape, lo, hi, body, pos}) = (
                (*Might need to enter var here into table and set escape to false*)
                let
                    val newEnv = S.enter (env, var, (d, escape));
                in
                    setFalse escape;
                    travExp lo;
                    travExp hi;
                    traverseExp newEnv d body
                end  
            )
            | travExp(A.BreakExp(_)) = ()
            | travExp(A.LetExp{decs, body, pos}) = (
                let
                    fun decHelper (singleDec, escapeEnv) = (
                        traverseDec escapeEnv d singleDec
                    )
                    val newEnv = foldl decHelper env decs
                in
                    traverseExp newEnv d body
                end
            )
            | travExp(A.ArrayExp{typ, size, init, pos}) = (
                travExp size;
                travExp init
            )
        in
          travExp e
        end
    and traverseDec (env : escEnv) (d : depth) (dec : A.dec) : escEnv = (
        let
            fun travDec(A.FunctionDec(funDecList) : A.dec) : escEnv = (
                let
                    fun funDecHelper ({name, params, result, body, pos} : A.fundec) : unit = (
                        let
                            fun paramHelper({name, escape, typ, pos}, escParamInstanceEnv) = (
                                escape := false;
                                S.enter(escParamInstanceEnv, name, ((d+1), escape))
                            )
                            val newEnv = foldl paramHelper env params
                        in
                            traverseExp newEnv (d+1) body                          
                        end
                    )
                in
                    app funDecHelper funDecList; 
                    env
                end
            )
            | travDec(A.VarDec{name, escape, typ, init, pos}) = (
                let
                    val _ = (escape := false)
                    val newEnv = S.enter(env, name, (d, escape))
                in
                    traverseExp env d init;
                    newEnv
                end                
            )
            | travDec(A.TypeDec(_)) = (env)
        in
            travDec dec
        end
    )
       
    (* (ErrorMsg.error 0 "todo"; raise ErrorMsg.Error) *)

    fun findEscape (prog : Absyn.exp) : unit =  traverseExp Symbol.empty 0 prog
end