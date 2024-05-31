structure Tr = Translate

structure Env : ENV =
struct
    type access = unit
    type ty = Types.ty

   datatype enventry = VarEntry of {ty : ty, readonly : bool, access : Tr.access}
                     | FunEntry of {formals : ty list, result : ty, level : Tr.level, label : Temp.label}
 
    val base_tenv = Symbol.enter
                  (Symbol.enter (Symbol.empty, Symbol.symbol "int", Types.INT)
                  , Symbol.symbol "string", Types.STRING)

    val base_venv = 
        let
            val baseFuncs = [
                (Symbol.symbol "print", FunEntry({formals = [Types.STRING], result = Types.UNIT, level=Tr.outermost, label=Temp.namedlabel("print")})),
                (Symbol.symbol "flush", FunEntry({formals = [], result = Types.UNIT, level=Tr.outermost, label=Temp.namedlabel("flush")})),
                (Symbol.symbol "getchar", FunEntry({formals = [], result = Types.STRING, level=Tr.outermost, label=Temp.namedlabel("getchar")})),
                (Symbol.symbol "ord", FunEntry({formals = [Types.STRING], result = Types.INT, level=Tr.outermost, label=Temp.namedlabel("ord")})),
                (Symbol.symbol "chr", FunEntry({formals = [Types.INT], result = Types.STRING, level=Tr.outermost, label=Temp.namedlabel("chr")})),
                (Symbol.symbol "size", FunEntry({formals = [Types.STRING], result = Types.INT, level=Tr.outermost, label=Temp.namedlabel("size")})),
                (Symbol.symbol "substring", FunEntry({formals = [Types.STRING, Types.INT, Types.INT], result = Types.STRING, level=Tr.outermost, label=Temp.namedlabel("substring")})),
                (Symbol.symbol "concat", FunEntry({formals = [Types.STRING, Types.STRING], result = Types.STRING, level=Tr.outermost, label=Temp.namedlabel("concat")})),
                (Symbol.symbol "not", FunEntry({formals = [Types.INT], result = Types.INT, level=Tr.outermost, label=Temp.namedlabel("not")})),
                (Symbol.symbol "exit", FunEntry({formals = [Types.INT], result = Types.UNIT, level=Tr.outermost, label=Temp.namedlabel("exit")}))
            ]
            fun constructTable ((symb, func), table) = Symbol.enter(table, symb, func)
        in
            foldr constructTable Symbol.empty baseFuncs
        end

end