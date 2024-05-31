structure Env : ENV =
struct
    type access = unit
    type ty = Types.ty

   datatype enventry = VarEntry of { access : Translate.access, ty : ty, readonly : bool }
                     | FunEntry of { level : Translate.level, label : Temp.label, formals : ty list, result : ty }

    fun addSym ((s, t), table) = Symbol.enter(table, Symbol.symbol s, t)

    val base_tenv = foldr addSym Symbol.empty [("int", Types.INT),
                                               ("string", Types.STRING)]

    val base_venv = foldr addSym Symbol.empty
                        [ ("print", FunEntry { level = Translate.outermost, label = Temp.namedlabel "print", formals = [Types.STRING], result = Types.UNIT }),
                        ("flush", FunEntry { level = Translate.outermost, label = Temp.namedlabel "flush", formals = [], result = Types.UNIT }),
                        ("getchar", FunEntry { level = Translate.outermost, label = Temp.namedlabel "my_getchar", formals = [], result = Types.STRING }),
                        ("ord", FunEntry { level = Translate.outermost, label = Temp.namedlabel "ord", formals = [Types.STRING], result = Types.INT }),
                        ("chr", FunEntry { level = Translate.outermost, label = Temp.namedlabel "chr", formals = [Types.INT], result = Types.STRING }),
                        ("size", FunEntry { level = Translate.outermost, label = Temp.namedlabel "size", formals = [Types.STRING], result = Types.INT }),
                        ("substring", FunEntry { level = Translate.outermost, label = Temp.namedlabel "substring", formals = [Types.STRING, Types.INT, Types.INT], result = Types.STRING }),
                        ("concat", FunEntry { level = Translate.outermost, label = Temp.namedlabel "concat", formals = [Types.STRING, Types.STRING], result = Types.STRING }),
                        ("not", FunEntry { level = Translate.outermost, label = Temp.namedlabel "not", formals = [Types.INT], result = Types.INT }),
                        ("exit", FunEntry { level = Translate.outermost, label = Temp.namedlabel "exit", formals = [Types.INT], result = Types.UNIT })
                        ]
end