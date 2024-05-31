structure Types =
struct

  type unique = unit ref

  datatype ty = 
            RECORD of (Symbol.symbol * ty) list * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
          | NAME of Symbol.symbol * ty option ref
          | UNIT (* A type for expressions that do not produce a value. *)
          | BOTTOM (* BOTTOM is a placeholder, if we cannot figure out the type of an expression.
                      BOTTOM should only occur in the case of a type error. *)

fun compatType NIL (RECORD _) = true
  | compatType (RECORD _) NIL = true
  | compatType (RECORD (_, u1)) (RECORD (_, u2)) = u1 = u2
  | compatType NIL NIL = true
  | compatType INT INT = true
  | compatType STRING STRING = true
  | compatType (ARRAY (_, u1)) (ARRAY (_, u2)) = u1 = u2
  | compatType (NAME (sym1, _)) (NAME (sym2, _)) = sym1 = sym2
  | compatType UNIT UNIT = true
  | compatType BOTTOM BOTTOM = true
  | compatType _ _ = false

fun printType (RECORD (st, _)) =
    let
      val st_str = map (fn (s, t) => Symbol.name s ^ " : " ^ printType t ) st
    in
    "{" ^ String.concatWith ", " st_str   ^ "}"
    end
  | printType NIL = "nil"
  | printType INT = "int"
  | printType STRING = "string"
  | printType (ARRAY(t, u)) = "[" ^ printType t ^ "]"
  | printType (NAME (s, _)) = Symbol.name s
  | printType UNIT = "unit"
  | printType BOTTOM = "bottom"

end


