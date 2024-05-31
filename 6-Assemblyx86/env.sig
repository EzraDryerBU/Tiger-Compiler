signature ENV =
sig
   type access
   type ty

   (* Information about values.  A value level name can either be:
    - a variable, with an associated type
    - a function, with an associated list of arguments (formals) and a
      result/return type. *)
   datatype enventry = VarEntry of { access : Translate.access, ty : ty, readonly : bool }
                     | FunEntry of { level : Translate.level, label : Temp.label, formals : ty list, result : ty }
 
    (* builtin types *)
    val base_tenv : ty Symbol.table
 
    (* builtin functions *)
    val base_venv : enventry Symbol.table
end