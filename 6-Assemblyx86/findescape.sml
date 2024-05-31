structure Tr = Translate
structure A = Absyn
structure S = Symbol

structure FindEscape : sig val findEscape : Absyn.exp -> unit end =
struct
    type depth = int
    type escEnv = (depth * bool ref) Symbol.table

    fun traverseVar (env : escEnv) (d : depth) (v : A.var) : unit =
        let
          fun trvar (A.SimpleVar (s, _)) = 
              (case S.look(env, s)
                  of SOME ((d', b)) => if d > d' then b := true else ()
                  | NONE => ())
          | trvar (A.FieldVar(v, sym, _)) = trvar v
          | trvar (A.SubscriptVar(v, exp, _)) = (trvar v; traverseExp env d exp)
        in
        trvar v
        end

    and traverseExp (env : escEnv) (d : depth) (e : A.exp) : unit =
        let
          fun trexp (A.VarExp v) = traverseVar env d v
            | trexp A.NilExp = ()
            | trexp (A.IntExp _) = ()
            | trexp (A.StringExp _) = ()
            | trexp (A.CallExp { args, ...}) = app trexp args
            | trexp (A.OpExp { left, right, ...}) = (trexp left; trexp right)
            | trexp (A.RecordExp { fields, ...}) = app (fn (_, e, _) => trexp e) fields
            | trexp (A.SeqExp es) = app (fn (e, _) => trexp e) es
            | trexp (A.AssignExp {var, exp, ...}) = (traverseVar env d var; trexp exp)
            | trexp (A.IfExp {test, then', else', ...}) = (trexp test; trexp then'; Option.app trexp else')
            | trexp (A.WhileExp { test, body, ...}) = (trexp test; trexp body)
            | trexp (A.ForExp {var, escape, lo, hi, body, ...}) =
              let
                val env' = S.enter(env, var, (d, escape))
              in
              escape := false; traverseExp env' d lo; traverseExp env' d lo; traverseExp env' d hi; traverseExp env' d body
              end
            | trexp (A.BreakExp _) = ()
            | trexp (A.LetExp { decs, body, ...}) =
              let
                val env' = foldl (fn (dec, ev) => traverseDec ev d dec) env decs
              in
              traverseExp env' d body
              end
            | trexp (A.ArrayExp { size, init, ...}) = (trexp size; trexp init)
        in
        trexp e
        end
    and traverseDec (env : escEnv) (d : depth) (dec : A.dec) : escEnv =
        let
          fun trdec (A.FunctionDec fs) =
            let
              fun trfun ({params, body, ...} : A.fundec, ev) =
                foldl (fn ({name, escape, ...}, ev') => (escape := false; S.enter(ev', name, (d + 1, escape)))) ev params
              val env' = foldl trfun env fs

              val _ = map (fn { body, ...} => traverseExp env' (d + 1) body) fs
            in
            env'
            end
            | trdec (A.VarDec { name, escape, init, ...}) =
              (escape := false; traverseExp env d init; S.enter(env, name, (d, escape)))
            | trdec (A.TypeDec _) = env
        in
        trdec dec
        end

    fun findEscape (prog : Absyn.exp) : unit =  traverseExp Symbol.empty 0 prog
end