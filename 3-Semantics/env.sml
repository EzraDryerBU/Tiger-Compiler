structure Env : ENV =
struct
    type access = unit
    type ty = Types.ty

   datatype enventry = VarEntry of { ty : ty, readonly : bool}
                     | FunEntry of { formals : ty list, result : ty }

    val base_tenv = Symbol.enter
                  (Symbol.enter (Symbol.empty, Symbol.symbol "int", Types.INT)
                  , Symbol.symbol "string", Types.STRING)

    val base_venv = 
        let
            val baseFuncs = [
                (Symbol.symbol "print", FunEntry({formals = [Types.STRING], result = Types.UNIT})),
                (Symbol.symbol "flush", FunEntry({formals = [], result = Types.UNIT})),
                (Symbol.symbol "getchar", FunEntry({formals = [], result = Types.STRING})),
                (Symbol.symbol "ord", FunEntry({formals = [Types.STRING], result = Types.INT})),
                (Symbol.symbol "chr", FunEntry({formals = [Types.INT], result = Types.STRING})),
                (Symbol.symbol "size", FunEntry({formals = [Types.STRING], result = Types.INT})),
                (Symbol.symbol "substring", FunEntry({formals = [Types.STRING, Types.INT, Types.INT], result = Types.STRING})),
                (Symbol.symbol "concat", FunEntry({formals = [Types.STRING, Types.STRING], result = Types.STRING})),
                (Symbol.symbol "not", FunEntry({formals = [Types.INT], result = Types.INT})),
                (Symbol.symbol "exit", FunEntry({formals = [Types.INT], result = Types.UNIT}))
            ]
            fun constructTable ((symb, func), table) = Symbol.enter(table, symb, func)
        in
            foldr constructTable Symbol.empty baseFuncs
        end

end