functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure A = Absyn
structure S = Symbol


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\225\000\005\000\225\000\007\000\225\000\009\000\225\000\
\\011\000\225\000\013\000\225\000\015\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\020\000\028\000\021\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\225\000\
\\026\000\225\000\030\000\225\000\031\000\225\000\034\000\225\000\
\\035\000\225\000\037\000\225\000\038\000\225\000\042\000\225\000\
\\043\000\225\000\044\000\225\000\000\000\
\\001\000\001\000\226\000\005\000\226\000\007\000\226\000\009\000\226\000\
\\011\000\226\000\013\000\226\000\015\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\019\000\226\000\021\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\226\000\
\\026\000\226\000\030\000\226\000\031\000\226\000\034\000\226\000\
\\035\000\226\000\037\000\226\000\038\000\226\000\042\000\226\000\
\\043\000\226\000\044\000\226\000\000\000\
\\001\000\001\000\227\000\005\000\227\000\007\000\227\000\009\000\227\000\
\\011\000\227\000\013\000\227\000\015\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\019\000\227\000\020\000\227\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\227\000\
\\026\000\227\000\030\000\227\000\031\000\227\000\034\000\227\000\
\\035\000\227\000\037\000\227\000\038\000\227\000\042\000\227\000\
\\043\000\227\000\044\000\227\000\000\000\
\\001\000\001\000\228\000\005\000\228\000\007\000\228\000\009\000\228\000\
\\011\000\228\000\013\000\228\000\015\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\019\000\228\000\020\000\228\000\
\\021\000\228\000\023\000\025\000\024\000\024\000\025\000\228\000\
\\026\000\228\000\030\000\228\000\031\000\228\000\034\000\228\000\
\\035\000\228\000\037\000\228\000\038\000\228\000\042\000\228\000\
\\043\000\228\000\044\000\228\000\000\000\
\\001\000\001\000\229\000\005\000\229\000\007\000\229\000\009\000\229\000\
\\011\000\229\000\013\000\229\000\015\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\019\000\229\000\020\000\229\000\
\\021\000\229\000\022\000\229\000\024\000\024\000\025\000\229\000\
\\026\000\229\000\030\000\229\000\031\000\229\000\034\000\229\000\
\\035\000\229\000\037\000\229\000\038\000\229\000\042\000\229\000\
\\043\000\229\000\044\000\229\000\000\000\
\\001\000\001\000\230\000\005\000\230\000\007\000\230\000\009\000\230\000\
\\011\000\230\000\013\000\230\000\015\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\019\000\230\000\020\000\230\000\
\\021\000\230\000\022\000\230\000\023\000\230\000\025\000\230\000\
\\026\000\230\000\030\000\230\000\031\000\230\000\034\000\230\000\
\\035\000\230\000\037\000\230\000\038\000\230\000\042\000\230\000\
\\043\000\230\000\044\000\230\000\000\000\
\\001\000\002\000\018\000\003\000\017\000\004\000\016\000\008\000\015\000\
\\009\000\050\000\016\000\014\000\029\000\013\000\032\000\012\000\
\\033\000\011\000\036\000\010\000\040\000\009\000\041\000\008\000\000\000\
\\001\000\002\000\018\000\003\000\017\000\004\000\016\000\008\000\015\000\
\\009\000\094\000\016\000\014\000\029\000\013\000\032\000\012\000\
\\033\000\011\000\036\000\010\000\040\000\009\000\041\000\008\000\000\000\
\\001\000\002\000\018\000\003\000\017\000\004\000\016\000\008\000\015\000\
\\016\000\014\000\029\000\013\000\032\000\012\000\033\000\011\000\
\\036\000\010\000\038\000\101\000\040\000\009\000\041\000\008\000\000\000\
\\001\000\002\000\018\000\003\000\017\000\004\000\016\000\008\000\015\000\
\\016\000\014\000\029\000\013\000\032\000\012\000\033\000\011\000\
\\036\000\010\000\040\000\009\000\041\000\008\000\000\000\
\\001\000\002\000\043\000\000\000\
\\001\000\002\000\047\000\003\000\017\000\004\000\016\000\008\000\015\000\
\\016\000\014\000\040\000\009\000\041\000\008\000\000\000\
\\001\000\002\000\055\000\000\000\
\\001\000\002\000\079\000\000\000\
\\001\000\002\000\080\000\000\000\
\\001\000\002\000\081\000\000\000\
\\001\000\002\000\090\000\000\000\
\\001\000\002\000\090\000\013\000\089\000\000\000\
\\001\000\002\000\122\000\012\000\121\000\028\000\120\000\000\000\
\\001\000\002\000\124\000\000\000\
\\001\000\002\000\146\000\000\000\
\\001\000\002\000\152\000\000\000\
\\001\000\002\000\156\000\000\000\
\\001\000\006\000\104\000\027\000\103\000\000\000\
\\001\000\006\000\139\000\000\000\
\\001\000\006\000\151\000\019\000\150\000\000\000\
\\001\000\008\000\105\000\000\000\
\\001\000\009\000\087\000\000\000\
\\001\000\009\000\117\000\000\000\
\\001\000\009\000\138\000\000\000\
\\001\000\011\000\095\000\000\000\
\\001\000\011\000\115\000\000\000\
\\001\000\011\000\130\000\000\000\
\\001\000\013\000\113\000\000\000\
\\001\000\013\000\147\000\000\000\
\\001\000\019\000\102\000\000\000\
\\001\000\019\000\114\000\000\000\
\\001\000\019\000\159\000\000\000\
\\001\000\027\000\082\000\000\000\
\\001\000\027\000\136\000\000\000\
\\001\000\030\000\084\000\000\000\
\\001\000\034\000\128\000\000\000\
\\001\000\035\000\083\000\000\000\
\\001\000\035\000\153\000\000\000\
\\001\000\037\000\078\000\000\000\
\\001\000\038\000\118\000\000\000\
\\001\000\039\000\134\000\000\000\
\\162\000\000\000\
\\163\000\042\000\042\000\043\000\041\000\044\000\040\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\167\000\042\000\042\000\043\000\041\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\043\000\041\000\044\000\040\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\044\000\040\000\000\000\
\\174\000\000\000\
\\175\000\000\000\
\\176\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\002\000\127\000\000\000\
\\180\000\005\000\137\000\000\000\
\\181\000\000\000\
\\182\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\042\000\042\000\000\000\
\\186\000\000\000\
\\187\000\000\000\
\\188\000\000\000\
\\189\000\008\000\053\000\010\000\052\000\012\000\051\000\000\000\
\\189\000\008\000\053\000\010\000\085\000\012\000\051\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\\191\000\039\000\132\000\000\000\
\\192\000\000\000\
\\193\000\007\000\086\000\000\000\
\\194\000\000\000\
\\195\000\005\000\116\000\000\000\
\\196\000\000\000\
\\197\000\000\000\
\\197\000\031\000\129\000\000\000\
\\198\000\000\000\
\\199\000\000\000\
\\200\000\000\000\
\\201\000\000\000\
\\202\000\000\000\
\\203\000\000\000\
\\204\000\000\000\
\\205\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\206\000\000\000\
\\207\000\000\000\
\\208\000\000\000\
\\209\000\000\000\
\\210\000\000\000\
\\211\000\010\000\021\000\014\000\020\000\027\000\019\000\000\000\
\\212\000\000\000\
\\213\000\000\000\
\\214\000\000\000\
\\215\000\000\000\
\\216\000\000\000\
\\217\000\000\000\
\\218\000\000\000\
\\219\000\000\000\
\\220\000\000\000\
\\221\000\000\000\
\\222\000\017\000\031\000\018\000\030\000\000\000\
\\223\000\017\000\031\000\018\000\030\000\000\000\
\\224\000\000\000\
\\231\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\000\000\
\\232\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\000\000\
\\233\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\234\000\000\000\
\\235\000\000\000\
\\236\000\005\000\143\000\000\000\
\\237\000\000\000\
\"
val actionRowNumbers =
"\010\000\100\000\094\000\087\000\
\\085\000\048\000\101\000\102\000\
\\049\000\011\000\010\000\010\000\
\\012\000\007\000\106\000\105\000\
\\075\000\012\000\013\000\010\000\
\\012\000\012\000\012\000\012\000\
\\012\000\012\000\012\000\012\000\
\\012\000\012\000\012\000\012\000\
\\071\000\056\000\049\000\059\000\
\\053\000\045\000\014\000\015\000\
\\016\000\039\000\043\000\041\000\
\\113\000\076\000\081\000\028\000\
\\104\000\018\000\010\000\008\000\
\\116\000\077\000\031\000\115\000\
\\114\000\006\000\005\000\004\000\
\\003\000\002\000\001\000\110\000\
\\109\000\112\000\111\000\072\000\
\\049\000\053\000\051\000\052\000\
\\060\000\056\000\049\000\050\000\
\\009\000\036\000\024\000\027\000\
\\010\000\010\000\010\000\010\000\
\\010\000\103\000\034\000\118\000\
\\037\000\032\000\083\000\029\000\
\\107\000\080\000\058\000\057\000\
\\054\000\055\000\046\000\088\000\
\\019\000\010\000\020\000\065\000\
\\042\000\097\000\091\000\086\000\
\\095\000\033\000\082\000\117\000\
\\010\000\079\000\010\000\108\000\
\\089\000\061\000\047\000\065\000\
\\062\000\069\000\040\000\066\000\
\\030\000\025\000\010\000\010\000\
\\078\000\119\000\010\000\084\000\
\\021\000\035\000\010\000\065\000\
\\026\000\022\000\044\000\096\000\
\\090\000\017\000\099\000\093\000\
\\064\000\063\000\070\000\067\000\
\\010\000\023\000\068\000\010\000\
\\120\000\073\000\038\000\098\000\
\\092\000\010\000\074\000\000\000"
val gotoT =
"\
\\014\000\005\000\015\000\004\000\016\000\003\000\018\000\002\000\
\\020\000\001\000\021\000\159\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\037\000\004\000\036\000\005\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\000\000\
\\000\000\
\\014\000\042\000\015\000\004\000\016\000\003\000\018\000\002\000\
\\020\000\001\000\000\000\
\\014\000\043\000\015\000\004\000\016\000\003\000\018\000\002\000\
\\020\000\001\000\000\000\
\\018\000\044\000\020\000\001\000\000\000\
\\012\000\047\000\014\000\046\000\015\000\004\000\016\000\003\000\
\\018\000\002\000\020\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\018\000\052\000\020\000\001\000\000\000\
\\000\000\
\\014\000\054\000\015\000\004\000\016\000\003\000\018\000\002\000\
\\020\000\001\000\000\000\
\\018\000\055\000\020\000\001\000\000\000\
\\018\000\056\000\020\000\001\000\000\000\
\\018\000\057\000\020\000\001\000\000\000\
\\018\000\058\000\020\000\001\000\000\000\
\\018\000\059\000\020\000\001\000\000\000\
\\018\000\060\000\020\000\001\000\000\000\
\\018\000\061\000\020\000\001\000\000\000\
\\018\000\062\000\020\000\001\000\000\000\
\\018\000\063\000\020\000\001\000\000\000\
\\018\000\064\000\020\000\001\000\000\000\
\\018\000\065\000\020\000\001\000\000\000\
\\018\000\066\000\020\000\001\000\000\000\
\\010\000\067\000\011\000\032\000\000\000\
\\003\000\070\000\004\000\069\000\005\000\035\000\009\000\068\000\000\000\
\\001\000\071\000\004\000\036\000\005\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\000\000\
\\004\000\072\000\005\000\035\000\000\000\
\\002\000\075\000\009\000\074\000\010\000\073\000\011\000\032\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\019\000\086\000\000\000\
\\014\000\089\000\015\000\004\000\016\000\003\000\018\000\002\000\
\\020\000\001\000\000\000\
\\013\000\091\000\014\000\090\000\015\000\004\000\016\000\003\000\
\\018\000\002\000\020\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\094\000\004\000\036\000\005\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\000\000\
\\002\000\095\000\009\000\074\000\010\000\073\000\011\000\032\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\096\000\004\000\069\000\005\000\035\000\009\000\068\000\000\000\
\\001\000\097\000\004\000\036\000\005\000\035\000\009\000\034\000\
\\010\000\033\000\011\000\032\000\000\000\
\\000\000\
\\012\000\098\000\014\000\046\000\015\000\004\000\016\000\003\000\
\\018\000\002\000\020\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\104\000\015\000\004\000\016\000\003\000\018\000\002\000\
\\020\000\001\000\000\000\
\\015\000\106\000\016\000\105\000\018\000\002\000\020\000\001\000\000\000\
\\014\000\108\000\015\000\107\000\016\000\003\000\018\000\002\000\
\\020\000\001\000\000\000\
\\014\000\109\000\015\000\004\000\016\000\003\000\018\000\002\000\
\\020\000\001\000\000\000\
\\012\000\110\000\014\000\046\000\015\000\004\000\016\000\003\000\
\\018\000\002\000\020\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\117\000\000\000\
\\014\000\121\000\015\000\004\000\016\000\003\000\018\000\002\000\
\\020\000\001\000\000\000\
\\000\000\
\\007\000\124\000\008\000\123\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\129\000\015\000\004\000\016\000\003\000\018\000\002\000\
\\020\000\001\000\000\000\
\\000\000\
\\013\000\131\000\014\000\090\000\015\000\004\000\016\000\003\000\
\\018\000\002\000\020\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\133\000\008\000\123\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\138\000\015\000\004\000\016\000\003\000\018\000\002\000\
\\020\000\001\000\000\000\
\\015\000\140\000\016\000\139\000\018\000\002\000\020\000\001\000\000\000\
\\000\000\
\\000\000\
\\015\000\143\000\016\000\142\000\018\000\002\000\020\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\146\000\015\000\004\000\016\000\003\000\018\000\002\000\
\\020\000\001\000\000\000\
\\007\000\147\000\008\000\123\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\019\000\152\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\153\000\015\000\004\000\016\000\003\000\018\000\002\000\
\\020\000\001\000\000\000\
\\000\000\
\\000\000\
\\015\000\156\000\016\000\155\000\018\000\002\000\020\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\158\000\015\000\004\000\016\000\003\000\018\000\002\000\
\\020\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 160
val numrules = 76
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string) | program of unit ->  (A.exp)
 | lvalue of unit ->  (A.var)
 | idexplist of unit ->  ( ( S.symbol * A.exp * pos )  list)
 | expb of unit ->  (A.exp) | uexp of unit ->  (A.exp)
 | mexp of unit ->  (A.exp) | exp of unit ->  (A.exp)
 | expcomma of unit ->  (A.exp list)
 | expseq of unit ->  ( ( A.exp * pos )  list)
 | fundec of unit ->  (A.fundec) | fundecs of unit ->  (A.fundec list)
 | vardec of unit ->  (A.dec) | tyfield of unit ->  (A.field)
 | tyfields of unit ->  (A.field list) | ty of unit ->  (A.ty)
 | tydec of unit ->  ({ name:S.symbol,ty:A.ty,pos:pos } )
 | tydecs of unit ->  ({ name:S.symbol,ty:A.ty,pos:pos }  list)
 | nonfundecs of unit ->  (A.dec list)
 | nontydecs of unit ->  (A.dec list) | decs of unit ->  (A.dec list)
end
type svalue = MlyValue.svalue
type result = A.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 31) => true | (T 32) => true | (T 33) => true | (T 39) => true
 | (T 35) => true | (T 36) => true | (T 37) => true | (T 41) => true
 | (T 42) => true | (T 43) => true | (T 27) => true | (T 28) => true
 | (T 29) => true | (T 30) => true | (T 34) => true | (T 38) => true
 | (T 40) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 29))::
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "TIMES"
  | (T 17) => "DIVIDE"
  | (T 18) => "EQ"
  | (T 19) => "NEQ"
  | (T 20) => "LT"
  | (T 21) => "LE"
  | (T 22) => "GT"
  | (T 23) => "GE"
  | (T 24) => "AND"
  | (T 25) => "OR"
  | (T 26) => "ASSIGN"
  | (T 27) => "ARRAY"
  | (T 28) => "IF"
  | (T 29) => "THEN"
  | (T 30) => "ELSE"
  | (T 31) => "WHILE"
  | (T 32) => "FOR"
  | (T 33) => "TO"
  | (T 34) => "DO"
  | (T 35) => "LET"
  | (T 36) => "IN"
  | (T 37) => "END"
  | (T 38) => "OF"
  | (T 39) => "BREAK"
  | (T 40) => "NIL"
  | (T 41) => "FUNCTION"
  | (T 42) => "VAR"
  | (T 43) => "TYPE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37)
 $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30)
 $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23)
 $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16)
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.program (fn _ => let val  (exp as exp1
) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 20, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.decs (fn _ => ([]))
 in ( LrTable.NT 0, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.nontydecs nontydecs1, _, nontydecs1right))
 :: ( _, ( MlyValue.tydecs tydecs1, tydecs1left, _)) :: rest671)) =>
 let val  result = MlyValue.decs (fn _ => let val  (tydecs as tydecs1)
 = tydecs1 ()
 val  (nontydecs as nontydecs1) = nontydecs1 ()
 in (A.TypeDec tydecs :: nontydecs)
end)
 in ( LrTable.NT 0, ( result, tydecs1left, nontydecs1right), rest671)

end
|  ( 3, ( ( _, ( MlyValue.nonfundecs nonfundecs1, _, nonfundecs1right)
) :: ( _, ( MlyValue.fundecs fundecs1, fundecs1left, _)) :: rest671))
 => let val  result = MlyValue.decs (fn _ => let val  (fundecs as 
fundecs1) = fundecs1 ()
 val  (nonfundecs as nonfundecs1) = nonfundecs1 ()
 in (A.FunctionDec fundecs :: nonfundecs)
end)
 in ( LrTable.NT 0, ( result, fundecs1left, nonfundecs1right), rest671
)
end
|  ( 4, ( ( _, ( MlyValue.decs decs1, _, decs1right)) :: ( _, ( 
MlyValue.vardec vardec1, vardec1left, _)) :: rest671)) => let val  
result = MlyValue.decs (fn _ => let val  (vardec as vardec1) = vardec1
 ()
 val  (decs as decs1) = decs1 ()
 in (vardec :: decs)
end)
 in ( LrTable.NT 0, ( result, vardec1left, decs1right), rest671)
end
|  ( 5, ( rest671)) => let val  result = MlyValue.nontydecs (fn _ => (
[]))
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 6, ( ( _, ( MlyValue.nonfundecs nonfundecs1, _, nonfundecs1right)
) :: ( _, ( MlyValue.fundecs fundecs1, fundecs1left, _)) :: rest671))
 => let val  result = MlyValue.nontydecs (fn _ => let val  (fundecs
 as fundecs1) = fundecs1 ()
 val  (nonfundecs as nonfundecs1) = nonfundecs1 ()
 in (A.FunctionDec fundecs :: nonfundecs)
end)
 in ( LrTable.NT 1, ( result, fundecs1left, nonfundecs1right), rest671
)
end
|  ( 7, ( ( _, ( MlyValue.decs decs1, _, decs1right)) :: ( _, ( 
MlyValue.vardec vardec1, vardec1left, _)) :: rest671)) => let val  
result = MlyValue.nontydecs (fn _ => let val  (vardec as vardec1) = 
vardec1 ()
 val  (decs as decs1) = decs1 ()
 in (vardec :: decs)
end)
 in ( LrTable.NT 1, ( result, vardec1left, decs1right), rest671)
end
|  ( 8, ( rest671)) => let val  result = MlyValue.nonfundecs (fn _ =>
 ([]))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 9, ( ( _, ( MlyValue.nontydecs nontydecs1, _, nontydecs1right))
 :: ( _, ( MlyValue.tydecs tydecs1, tydecs1left, _)) :: rest671)) =>
 let val  result = MlyValue.nonfundecs (fn _ => let val  (tydecs as 
tydecs1) = tydecs1 ()
 val  (nontydecs as nontydecs1) = nontydecs1 ()
 in (A.TypeDec tydecs :: nontydecs)
end)
 in ( LrTable.NT 2, ( result, tydecs1left, nontydecs1right), rest671)

end
|  ( 10, ( ( _, ( MlyValue.decs decs1, _, decs1right)) :: ( _, ( 
MlyValue.vardec vardec1, vardec1left, _)) :: rest671)) => let val  
result = MlyValue.nonfundecs (fn _ => let val  (vardec as vardec1) = 
vardec1 ()
 val  (decs as decs1) = decs1 ()
 in (vardec :: decs)
end)
 in ( LrTable.NT 2, ( result, vardec1left, decs1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.tydec tydec1, tydec1left, tydec1right)) :: 
rest671)) => let val  result = MlyValue.tydecs (fn _ => let val  (
tydec as tydec1) = tydec1 ()
 in ([tydec])
end)
 in ( LrTable.NT 3, ( result, tydec1left, tydec1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.tydecs tydecs1, _, tydecs1right)) :: ( _, (
 MlyValue.tydec tydec1, tydec1left, _)) :: rest671)) => let val  
result = MlyValue.tydecs (fn _ => let val  (tydec as tydec1) = tydec1
 ()
 val  (tydecs as tydecs1) = tydecs1 ()
 in (tydec :: tydecs)
end)
 in ( LrTable.NT 3, ( result, tydec1left, tydecs1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.ty ty1, _, ty1right)) :: ( _, ( _, EQleft,
 _)) :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, TYPE1left, _)) :: 
rest671)) => let val  result = MlyValue.tydec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (ty as ty1) = ty1 ()
 in ({ name = S.symbol ID, ty = ty, pos = EQleft})
end)
 in ( LrTable.NT 4, ( result, TYPE1left, ty1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.ty (fn _ => let val  (ID as 
ID1) = ID1 ()
 in (A.NameTy (S.symbol ID, IDleft))
end)
 in ( LrTable.NT 5, ( result, ID1left, ID1right), rest671)
end
|  ( 15, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.tyfields 
tyfields1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.ty (fn _ => let val  (tyfields as tyfields1) =
 tyfields1 ()
 in (A.RecordTy tyfields)
end)
 in ( LrTable.NT 5, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.ID ID1, IDleft, ID1right)) :: _ :: ( _, ( _
, ARRAY1left, _)) :: rest671)) => let val  result = MlyValue.ty (fn _
 => let val  (ID as ID1) = ID1 ()
 in (A.ArrayTy (S.symbol ID, IDleft))
end)
 in ( LrTable.NT 5, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 17, ( rest671)) => let val  result = MlyValue.tyfields (fn _ => (
[]))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 18, ( ( _, ( MlyValue.tyfield tyfield1, tyfield1left, 
tyfield1right)) :: rest671)) => let val  result = MlyValue.tyfields
 (fn _ => let val  (tyfield as tyfield1) = tyfield1 ()
 in ([tyfield])
end)
 in ( LrTable.NT 6, ( result, tyfield1left, tyfield1right), rest671)

end
|  ( 19, ( ( _, ( MlyValue.tyfields tyfields1, _, tyfields1right)) ::
 _ :: ( _, ( MlyValue.tyfield tyfield1, tyfield1left, _)) :: rest671))
 => let val  result = MlyValue.tyfields (fn _ => let val  (tyfield as 
tyfield1) = tyfield1 ()
 val  (tyfields as tyfields1) = tyfields1 ()
 in (tyfield :: tyfields)
end)
 in ( LrTable.NT 6, ( result, tyfield1left, tyfields1right), rest671)

end
|  ( 20, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: ( _, ( _, 
COLONleft, _)) :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) =>
 let val  result = MlyValue.tyfield (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in (
 { name = S.symbol ID1, escape = ref true, typ = S.symbol ID2, pos = COLONleft} 
)
end)
 in ( LrTable.NT 7, ( result, ID1left, ID2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
ASSIGNleft, _)) :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, 
VAR1left, _)) :: rest671)) => let val  result = MlyValue.vardec (fn _
 => let val  ID1 = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (
A.VarDec { name = S.symbol ID1
                                     , escape = ref true
                                     , typ = NONE
                                     , init = exp
                                     , pos = ASSIGNleft }
)
end)
 in ( LrTable.NT 8, ( result, VAR1left, exp1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
ASSIGNleft, _)) :: ( _, ( MlyValue.ID ID2, ID2left, _)) :: _ :: ( _, (
 MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) =>
 let val  result = MlyValue.vardec (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in (
A.VarDec { name = S.symbol ID1
                                              , escape = ref true
                                              , typ = SOME (S.symbol ID2, ID2left)
                                              , init = exp
                                              , pos = ASSIGNleft }
)
end)
 in ( LrTable.NT 8, ( result, VAR1left, exp1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.fundec fundec1, fundec1left, fundec1right))
 :: rest671)) => let val  result = MlyValue.fundecs (fn _ => let val 
 (fundec as fundec1) = fundec1 ()
 in ([fundec])
end)
 in ( LrTable.NT 9, ( result, fundec1left, fundec1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.fundecs fundecs1, _, fundecs1right)) :: ( _
, ( MlyValue.fundec fundec1, fundec1left, _)) :: rest671)) => let val 
 result = MlyValue.fundecs (fn _ => let val  (fundec as fundec1) = 
fundec1 ()
 val  (fundecs as fundecs1) = fundecs1 ()
 in (fundec :: fundecs)
end)
 in ( LrTable.NT 9, ( result, fundec1left, fundecs1right), rest671)

end
|  ( 25, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: _ :: ( _, 
( MlyValue.tyfields tyfields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, 
ID1left, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  
result = MlyValue.fundec (fn _ => let val  ID1 = ID1 ()
 val  (tyfields as tyfields1) = tyfields1 ()
 val  (exp as exp1) = exp1 ()
 in (
 { name = S.symbol ID1,
                                                       params = tyfields,
                                                       result = NONE,
                                                       body = exp,
                                                       pos = ID1left } 
)
end)
 in ( LrTable.NT 10, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, ID2left, _)) :: _ :: _ :: ( _, ( MlyValue.tyfields 
tyfields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: ( _,
 ( _, FUNCTION1left, _)) :: rest671)) => let val  result = 
MlyValue.fundec (fn _ => let val  ID1 = ID1 ()
 val  (tyfields as tyfields1) = tyfields1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in (
 { name = S.symbol ID1,
                                                                params = tyfields,
                                                                result = SOME (S.symbol ID2, ID2left),
                                                                body = exp,
                                                                pos = ID1left } 
)
end)
 in ( LrTable.NT 10, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.lvalue (fn _ => let val  (ID
 as ID1) = ID1 ()
 in (A.SimpleVar (S.symbol ID, IDleft))
end)
 in ( LrTable.NT 19, ( result, ID1left, ID1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( 
MlyValue.lvalue lvalue1, (lvalueleft as lvalue1left), _)) :: rest671))
 => let val  result = MlyValue.lvalue (fn _ => let val  (lvalue as 
lvalue1) = lvalue1 ()
 val  (ID as ID1) = ID1 ()
 in (A.FieldVar (lvalue, S.symbol ID, lvalueleft))
end)
 in ( LrTable.NT 19, ( result, lvalue1left, ID1right), rest671)
end
|  ( 29, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: 
rest671)) => let val  result = MlyValue.lvalue (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (A.SubscriptVar (A.SimpleVar (S.symbol ID, IDleft), exp, IDleft))

end)
 in ( LrTable.NT 19, ( result, ID1left, RBRACK1right), rest671)
end
|  ( 30, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: _ :: ( _, ( MlyValue.lvalue lvalue1, (lvalueleft as 
lvalue1left), _)) :: rest671)) => let val  result = MlyValue.lvalue
 (fn _ => let val  (lvalue as lvalue1) = lvalue1 ()
 val  (exp as exp1) = exp1 ()
 in (A.SubscriptVar (lvalue, exp, lvalueleft))
end)
 in ( LrTable.NT 19, ( result, lvalue1left, RBRACK1right), rest671)

end
|  ( 31, ( ( _, ( MlyValue.exp exp1, (expleft as exp1left), exp1right)
) :: rest671)) => let val  result = MlyValue.expseq (fn _ => let val 
 (exp as exp1) = exp1 ()
 in ([(exp, expleft)])
end)
 in ( LrTable.NT 11, ( result, exp1left, exp1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.expseq expseq1, _, expseq1right)) :: _ :: (
 _, ( MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671)) =>
 let val  result = MlyValue.expseq (fn _ => let val  (exp as exp1) = 
exp1 ()
 val  (expseq as expseq1) = expseq1 ()
 in ((exp, expleft) :: expseq)
end)
 in ( LrTable.NT 11, ( result, exp1left, expseq1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.expcomma (fn _ => let val  (exp as 
exp1) = exp1 ()
 in ([exp])
end)
 in ( LrTable.NT 12, ( result, exp1left, exp1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.expcomma expcomma1, _, expcomma1right)) ::
 _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val 
 result = MlyValue.expcomma (fn _ => let val  (exp as exp1) = exp1 ()
 val  (expcomma as expcomma1) = expcomma1 ()
 in (exp :: expcomma)
end)
 in ( LrTable.NT 12, ( result, exp1left, expcomma1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.mexp mexp1, mexp1left, mexp1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (mexp
 as mexp1) = mexp1 ()
 in (mexp)
end)
 in ( LrTable.NT 13, ( result, mexp1left, mexp1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.uexp uexp1, uexp1left, uexp1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (uexp
 as uexp1) = uexp1 ()
 in (uexp)
end)
 in ( LrTable.NT 13, ( result, uexp1left, uexp1right), rest671)
end
|  ( 37, ( ( _, ( _, _, END1right)) :: _ :: ( _, ( MlyValue.decs decs1
, _, _)) :: ( _, ( _, (LETleft as LET1left), _)) :: rest671)) => let
 val  result = MlyValue.mexp (fn _ => let val  (decs as decs1) = decs1
 ()
 in (A.LetExp { decs = decs, body = A.NilExp, pos = LETleft })
end)
 in ( LrTable.NT 14, ( result, LET1left, END1right), rest671)
end
|  ( 38, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.expseq expseq1,
 _, _)) :: _ :: ( _, ( MlyValue.decs decs1, _, _)) :: ( _, ( _, (
LETleft as LET1left), _)) :: rest671)) => let val  result = 
MlyValue.mexp (fn _ => let val  (decs as decs1) = decs1 ()
 val  (expseq as expseq1) = expseq1 ()
 in (A.LetExp { decs = decs, body = A.SeqExp expseq, pos = LETleft })

end)
 in ( LrTable.NT 14, ( result, LET1left, END1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.mexp mexp2, _, mexp2right)) :: _ :: ( _, ( 
MlyValue.mexp mexp1, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _))
 :: ( _, ( _, (IFleft as IF1left), _)) :: rest671)) => let val  result
 = MlyValue.mexp (fn _ => let val  (exp as exp1) = exp1 ()
 val  mexp1 = mexp1 ()
 val  mexp2 = mexp2 ()
 in (
A.IfExp { test = exp, then' = mexp1, else' = SOME mexp2, pos = IFleft }
)
end)
 in ( LrTable.NT 14, ( result, IF1left, mexp2right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.mexp mexp1, _, mexp1right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (WHILEleft as WHILE1left), _))
 :: rest671)) => let val  result = MlyValue.mexp (fn _ => let val  (
exp as exp1) = exp1 ()
 val  (mexp as mexp1) = mexp1 ()
 in (A.WhileExp { test = exp, body = mexp, pos = WHILEleft})
end)
 in ( LrTable.NT 14, ( result, WHILE1left, mexp1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.mexp mexp1, _, mexp1right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FORleft as FOR1left
), _)) :: rest671)) => let val  result = MlyValue.mexp (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  (mexp as mexp1) = mexp1 ()
 in (
A.ForExp { var = S.symbol ID, escape = ref true, lo = exp1, hi = exp2, body = mexp, pos = FORleft }
)
end)
 in ( LrTable.NT 14, ( result, FOR1left, mexp1right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.mexp mexp1, _, mexp1right)) :: _ :: _ :: (
 _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (
IDleft as ID1left), _)) :: rest671)) => let val  result = 
MlyValue.mexp (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 val  (mexp as mexp1) = mexp1 ()
 in (
A.ArrayExp { typ = S.symbol ID, size = exp, init = mexp, pos = IDleft}
)
end)
 in ( LrTable.NT 14, ( result, ID1left, mexp1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.expb expb1, expb1left, expb1right)) :: 
rest671)) => let val  result = MlyValue.mexp (fn _ => let val  (expb
 as expb1) = expb1 ()
 in (expb)
end)
 in ( LrTable.NT 14, ( result, expb1left, expb1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (IFleft as IF1left), _)) :: 
rest671)) => let val  result = MlyValue.uexp (fn _ => let val  exp1 = 
exp1 ()
 val  exp2 = exp2 ()
 in (A.IfExp { test = exp1, then' = exp2, else' = NONE, pos = IFleft }
)
end)
 in ( LrTable.NT 15, ( result, IF1left, exp2right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.uexp uexp1, _, uexp1right)) :: _ :: ( _, ( 
MlyValue.mexp mexp1, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _))
 :: ( _, ( _, (IFleft as IF1left), _)) :: rest671)) => let val  result
 = MlyValue.uexp (fn _ => let val  (exp as exp1) = exp1 ()
 val  (mexp as mexp1) = mexp1 ()
 val  (uexp as uexp1) = uexp1 ()
 in (
A.IfExp { test = exp, then' = mexp, else' = SOME uexp, pos = IFleft })

end)
 in ( LrTable.NT 15, ( result, IF1left, uexp1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.uexp uexp1, _, uexp1right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (WHILEleft as WHILE1left), _))
 :: rest671)) => let val  result = MlyValue.uexp (fn _ => let val  (
exp as exp1) = exp1 ()
 val  (uexp as uexp1) = uexp1 ()
 in (A.WhileExp { test = exp, body = uexp, pos = WHILEleft})
end)
 in ( LrTable.NT 15, ( result, WHILE1left, uexp1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.uexp uexp1, _, uexp1right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FORleft as FOR1left
), _)) :: rest671)) => let val  result = MlyValue.uexp (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  (uexp as uexp1) = uexp1 ()
 in (
A.ForExp { var = S.symbol ID, escape = ref true, lo = exp1, hi = exp2, body = uexp, pos = FORleft }
)
end)
 in ( LrTable.NT 15, ( result, FOR1left, uexp1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.uexp uexp1, _, uexp1right)) :: _ :: _ :: (
 _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (
IDleft as ID1left), _)) :: rest671)) => let val  result = 
MlyValue.uexp (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 val  (uexp as uexp1) = uexp1 ()
 in (
A.ArrayExp { typ = S.symbol ID, size = exp, init = uexp, pos = IDleft}
)
end)
 in ( LrTable.NT 15, ( result, ID1left, uexp1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.lvalue lvalue1, lvalue1left, lvalue1right))
 :: rest671)) => let val  result = MlyValue.expb (fn _ => let val  (
lvalue as lvalue1) = lvalue1 ()
 in (A.VarExp lvalue)
end)
 in ( LrTable.NT 17, ( result, lvalue1left, lvalue1right), rest671)

end
|  ( 50, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.expb (fn _ => (A.NilExp))
 in ( LrTable.NT 17, ( result, NIL1left, NIL1right), rest671)
end
|  ( 51, ( ( _, ( _, (BREAKleft as BREAK1left), BREAK1right)) :: 
rest671)) => let val  result = MlyValue.expb (fn _ => (
A.BreakExp BREAKleft))
 in ( LrTable.NT 17, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 52, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expseq 
expseq1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val 
 result = MlyValue.expb (fn _ => let val  (expseq as expseq1) = 
expseq1 ()
 in (A.SeqExp expseq)
end)
 in ( LrTable.NT 17, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 53, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( _, LPAREN1left, _))
 :: rest671)) => let val  result = MlyValue.expb (fn _ => (A.SeqExp []
))
 in ( LrTable.NT 17, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 54, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.expb (fn _ => let val  (INT as INT1) =
 INT1 ()
 in (A.IntExp (INT))
end)
 in ( LrTable.NT 17, ( result, INT1left, INT1right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.STRING STRING1, (STRINGleft as STRING1left)
, STRING1right)) :: rest671)) => let val  result = MlyValue.expb (fn _
 => let val  (STRING as STRING1) = STRING1 ()
 in (A.StringExp (STRING, STRINGleft))
end)
 in ( LrTable.NT 17, ( result, STRING1left, STRING1right), rest671)

end
|  ( 56, ( ( _, ( _, _, RPAREN1right)) :: _ :: ( _, ( MlyValue.ID ID1,
 (IDleft as ID1left), _)) :: rest671)) => let val  result = 
MlyValue.expb (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.CallExp { func = S.symbol ID, args = [], pos = IDleft })
end)
 in ( LrTable.NT 17, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 57, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expcomma 
expcomma1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left),
 _)) :: rest671)) => let val  result = MlyValue.expb (fn _ => let val 
 (ID as ID1) = ID1 ()
 val  (expcomma as expcomma1) = expcomma1 ()
 in (A.CallExp { func = S.symbol ID, args = expcomma, pos = IDleft })

end)
 in ( LrTable.NT 17, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.expb expb2, _, expb2right)) :: ( _, ( _, 
TIMESleft, _)) :: ( _, ( MlyValue.expb expb1, expb1left, _)) :: 
rest671)) => let val  result = MlyValue.expb (fn _ => let val  expb1 =
 expb1 ()
 val  expb2 = expb2 ()
 in (
A.OpExp { left = expb1, oper = A.TimesOp, right = expb2, pos = TIMESleft }
)
end)
 in ( LrTable.NT 17, ( result, expb1left, expb2right), rest671)
end
|  ( 59, ( ( _, ( MlyValue.expb expb2, _, expb2right)) :: ( _, ( _, 
DIVIDEleft, _)) :: ( _, ( MlyValue.expb expb1, expb1left, _)) :: 
rest671)) => let val  result = MlyValue.expb (fn _ => let val  expb1 =
 expb1 ()
 val  expb2 = expb2 ()
 in (
A.OpExp { left = expb1, oper = A.DivideOp, right = expb2, pos = DIVIDEleft }
)
end)
 in ( LrTable.NT 17, ( result, expb1left, expb2right), rest671)
end
|  ( 60, ( ( _, ( MlyValue.expb expb2, _, expb2right)) :: ( _, ( _, 
PLUSleft, _)) :: ( _, ( MlyValue.expb expb1, expb1left, _)) :: rest671
)) => let val  result = MlyValue.expb (fn _ => let val  expb1 = expb1
 ()
 val  expb2 = expb2 ()
 in (
A.OpExp { left = expb1, oper = A.PlusOp, right = expb2, pos = PLUSleft }
)
end)
 in ( LrTable.NT 17, ( result, expb1left, expb2right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.expb expb2, _, expb2right)) :: ( _, ( _, 
MINUSleft, _)) :: ( _, ( MlyValue.expb expb1, expb1left, _)) :: 
rest671)) => let val  result = MlyValue.expb (fn _ => let val  expb1 =
 expb1 ()
 val  expb2 = expb2 ()
 in (
A.OpExp { left = expb1, oper = A.MinusOp, right = expb2, pos = MINUSleft }
)
end)
 in ( LrTable.NT 17, ( result, expb1left, expb2right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.expb expb1, _, expb1right)) :: ( _, ( _, (
MINUSleft as MINUS1left), _)) :: rest671)) => let val  result = 
MlyValue.expb (fn _ => let val  (expb as expb1) = expb1 ()
 in (
A.OpExp { left = A.IntExp 0, oper = A.MinusOp, right = expb, pos = MINUSleft }
)
end)
 in ( LrTable.NT 17, ( result, MINUS1left, expb1right), rest671)
end
|  ( 63, ( ( _, ( MlyValue.expb expb2, _, expb2right)) :: ( _, ( _, 
EQleft, _)) :: ( _, ( MlyValue.expb expb1, expb1left, _)) :: rest671))
 => let val  result = MlyValue.expb (fn _ => let val  expb1 = expb1 ()
 val  expb2 = expb2 ()
 in (
A.OpExp { left = expb1, oper = A.EqOp, right = expb2, pos = EQleft })

end)
 in ( LrTable.NT 17, ( result, expb1left, expb2right), rest671)
end
|  ( 64, ( ( _, ( MlyValue.expb expb2, _, expb2right)) :: ( _, ( _, 
NEQleft, _)) :: ( _, ( MlyValue.expb expb1, expb1left, _)) :: rest671)
) => let val  result = MlyValue.expb (fn _ => let val  expb1 = expb1
 ()
 val  expb2 = expb2 ()
 in (
A.OpExp { left = expb1, oper = A.NeqOp, right = expb2, pos = NEQleft }
)
end)
 in ( LrTable.NT 17, ( result, expb1left, expb2right), rest671)
end
|  ( 65, ( ( _, ( MlyValue.expb expb2, _, expb2right)) :: ( _, ( _, 
LTleft, _)) :: ( _, ( MlyValue.expb expb1, expb1left, _)) :: rest671))
 => let val  result = MlyValue.expb (fn _ => let val  expb1 = expb1 ()
 val  expb2 = expb2 ()
 in (
A.OpExp { left = expb1, oper = A.LtOp, right = expb2, pos = LTleft })

end)
 in ( LrTable.NT 17, ( result, expb1left, expb2right), rest671)
end
|  ( 66, ( ( _, ( MlyValue.expb expb2, _, expb2right)) :: ( _, ( _, 
LEleft, _)) :: ( _, ( MlyValue.expb expb1, expb1left, _)) :: rest671))
 => let val  result = MlyValue.expb (fn _ => let val  expb1 = expb1 ()
 val  expb2 = expb2 ()
 in (
A.OpExp { left = expb1, oper = A.LeOp, right = expb2, pos = LEleft })

end)
 in ( LrTable.NT 17, ( result, expb1left, expb2right), rest671)
end
|  ( 67, ( ( _, ( MlyValue.expb expb2, _, expb2right)) :: ( _, ( _, 
GTleft, _)) :: ( _, ( MlyValue.expb expb1, expb1left, _)) :: rest671))
 => let val  result = MlyValue.expb (fn _ => let val  expb1 = expb1 ()
 val  expb2 = expb2 ()
 in (
A.OpExp { left = expb1, oper = A.GtOp, right = expb2, pos = GTleft })

end)
 in ( LrTable.NT 17, ( result, expb1left, expb2right), rest671)
end
|  ( 68, ( ( _, ( MlyValue.expb expb2, _, expb2right)) :: ( _, ( _, 
GEleft, _)) :: ( _, ( MlyValue.expb expb1, expb1left, _)) :: rest671))
 => let val  result = MlyValue.expb (fn _ => let val  expb1 = expb1 ()
 val  expb2 = expb2 ()
 in (
A.OpExp { left = expb1, oper = A.GeOp, right = expb2, pos = GEleft })

end)
 in ( LrTable.NT 17, ( result, expb1left, expb2right), rest671)
end
|  ( 69, ( ( _, ( MlyValue.expb expb2, _, expb2right)) :: ( _, ( _, 
ANDleft, _)) :: ( _, ( MlyValue.expb expb1, expb1left, _)) :: rest671)
) => let val  result = MlyValue.expb (fn _ => let val  expb1 = expb1
 ()
 val  expb2 = expb2 ()
 in (
A.IfExp { test = expb1, then' = expb2, else' = SOME (A.IntExp 0), pos = ANDleft } 
)
end)
 in ( LrTable.NT 17, ( result, expb1left, expb2right), rest671)
end
|  ( 70, ( ( _, ( MlyValue.expb expb2, _, expb2right)) :: ( _, ( _, 
ORleft, _)) :: ( _, ( MlyValue.expb expb1, expb1left, _)) :: rest671))
 => let val  result = MlyValue.expb (fn _ => let val  expb1 = expb1 ()
 val  expb2 = expb2 ()
 in (
A.IfExp { test = expb1, then' = A.IntExp 1, else' = SOME expb2, pos = ORleft } 
)
end)
 in ( LrTable.NT 17, ( result, expb1left, expb2right), rest671)
end
|  ( 71, ( ( _, ( MlyValue.expb expb1, _, expb1right)) :: ( _, ( _, 
ASSIGNleft, _)) :: ( _, ( MlyValue.lvalue lvalue1, lvalue1left, _)) ::
 rest671)) => let val  result = MlyValue.expb (fn _ => let val  (
lvalue as lvalue1) = lvalue1 ()
 val  (expb as expb1) = expb1 ()
 in (A.AssignExp { var = lvalue, exp = expb, pos = ASSIGNleft })
end)
 in ( LrTable.NT 17, ( result, lvalue1left, expb1right), rest671)
end
|  ( 72, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.idexplist 
idexplist1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left)
, _)) :: rest671)) => let val  result = MlyValue.expb (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  (idexplist as idexplist1) = idexplist1 ()
 in (
A.RecordExp { typ = S.symbol ID, fields = idexplist, pos = IDleft })

end)
 in ( LrTable.NT 17, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 73, ( ( _, ( _, _, RBRACE1right)) :: _ :: ( _, ( MlyValue.ID ID1,
 (IDleft as ID1left), _)) :: rest671)) => let val  result = 
MlyValue.expb (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.RecordExp { typ = S.symbol ID, fields = [], pos = IDleft })
end
)
 in ( LrTable.NT 17, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 74, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val  
result = MlyValue.idexplist (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in ([(S.symbol ID, exp, IDleft)])
end)
 in ( LrTable.NT 18, ( result, ID1left, exp1right), rest671)
end
|  ( 75, ( ( _, ( MlyValue.idexplist idexplist1, _, idexplist1right))
 :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID 
ID1, (IDleft as ID1left), _)) :: rest671)) => let val  result = 
MlyValue.idexplist (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 val  (idexplist as idexplist1) = idexplist1 ()
 in ((S.symbol ID, exp, IDleft) :: idexplist)
end)
 in ( LrTable.NT 18, ( result, ID1left, idexplist1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
end
end
