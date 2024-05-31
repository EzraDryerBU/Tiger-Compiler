type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val cNested = ref 0;
val stringBuilder = ref ""
val initMode = ref true;
val finString = ref ""

exception openStringOrComment

fun eof() = let 
                val pos = hd(!linePos) 
            in 
                if !initMode then (Tokens.EOF(pos, pos)) else (print "Error"; raise openStringOrComment)
            end

fun asciiCode str =
    let val subStr = String.substring(str, 1, 3)
        val intVal = valOf(Int.fromString(subStr))
        val charVal = chr intVal
    in Char.toString charVal end

%%
%structure TigerLexFun
%s COMMENT STRING;
%%

<INITIAL> \r?\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> [\t\r] => (continue());
<INITIAL> "\ "  => (continue());
<INITIAL> "\t"  => (continue());

<INITIAL> "type" => (Tokens.TYPE(yypos, yypos + 4));
<INITIAL> "var" => (Tokens.VAR(yypos, yypos + 3));
<INITIAL> "function" => (Tokens.FUNCTION(yypos, yypos + 8));
<INITIAL> "break" => (Tokens.BREAK(yypos, yypos + 5));
<INITIAL> "of" => (Tokens.OF(yypos, yypos + 2));
<INITIAL> "end" => (Tokens.END(yypos, yypos + 3));
<INITIAL> "in" => (Tokens.IN(yypos, yypos + 2));
<INITIAL> "nil" => (Tokens.NIL(yypos, yypos + 3));
<INITIAL> "let" => (Tokens.LET(yypos, yypos + 3));
<INITIAL> "do" => (Tokens.DO(yypos, yypos + 2));
<INITIAL> "to" => (Tokens.TO(yypos, yypos + 2));
<INITIAL> "for" => (Tokens.FOR(yypos, yypos + 3));
<INITIAL> "while" => (Tokens.WHILE(yypos, yypos + 5));
<INITIAL> "else" => (Tokens.ELSE(yypos, yypos + 4));
<INITIAL> "then" => (Tokens.THEN(yypos, yypos + 4));
<INITIAL> "if" => (Tokens.IF(yypos, yypos + 2));
<INITIAL> "array" => (Tokens.ARRAY(yypos, yypos + 5));

<INITIAL> ":=" => (Tokens.ASSIGN(yypos, yypos + 2));
<INITIAL> "|" => (Tokens.OR(yypos, yypos + 1));
<INITIAL> "&" => (Tokens.AND(yypos, yypos + 1));
<INITIAL> ">=" => (Tokens.GE(yypos, yypos + 2));
<INITIAL> ">" => (Tokens.GT(yypos, yypos + 1));
<INITIAL> "<=" => (Tokens.LE(yypos, yypos + 2));
<INITIAL> "<" => (Tokens.LT(yypos, yypos + 1));
<INITIAL> "<>" => (Tokens.NEQ(yypos, yypos + 2));
<INITIAL> "=" => (Tokens.EQ(yypos, yypos + 1));
<INITIAL> "+" => (Tokens.PLUS(yypos, yypos + 1));
<INITIAL> "-" => (Tokens.MINUS(yypos, yypos + 1));
<INITIAL> "*" => (Tokens.TIMES(yypos, yypos + 1));
<INITIAL> "/" => (Tokens.DIVIDE(yypos, yypos + 1));
<INITIAL> "." => (Tokens.DOT(yypos, yypos + 1));

<INITIAL> "{" => (Tokens.LBRACE(yypos, yypos + 1));
<INITIAL> "}" => (Tokens.RBRACE(yypos, yypos + 1));
<INITIAL> "[" => (Tokens.LBRACK(yypos, yypos + 1));
<INITIAL> "]" => (Tokens.RBRACK(yypos, yypos + 1));
<INITIAL> ")" => (Tokens.RPAREN(yypos, yypos + 1));
<INITIAL> "(" => (Tokens.LPAREN(yypos, yypos + 1));

<INITIAL> ";" => (Tokens.SEMICOLON(yypos, yypos + 1));
<INITIAL> ":" => (Tokens.COLON(yypos, yypos + 1));
<INITIAL> "," => (Tokens.COMMA(yypos, yypos + 1));



<INITIAL> "/*" => (YYBEGIN COMMENT; initMode := false; continue ());
<COMMENT> "/*" => (cNested := !cNested+1; continue());
<COMMENT> . => (continue());
<COMMENT> [(\r?\n)\t] => (continue());
<COMMENT> "*/" => (if !cNested = 0 then (YYBEGIN INITIAL; initMode := true; continue())
                   else (cNested := !cNested-1; continue()));



<INITIAL> "\"" => (YYBEGIN STRING; initMode := false; continue());
<STRING>  "\"" => (YYBEGIN INITIAL; initMode := true; 
                    finString := !stringBuilder;
                    stringBuilder :=  "";
                    Tokens.STRING(!finString, yypos - String.size (!finString), yypos));

<STRING> . => (stringBuilder := !stringBuilder^yytext; continue());
<STRING> \\\\ => (stringBuilder := !stringBuilder^"\\"; continue());
<STRING> \\\" => (stringBuilder := !stringBuilder^"\""; continue());
<STRING> \\n  => (stringBuilder := !stringBuilder^"\n"; continue());
<STRING> \\t  => (stringBuilder := !stringBuilder^"\t"; continue());
<STRING> \\\^[a-z]  => (stringBuilder := !stringBuilder^"\\^c"; continue());
<STRING>\\[0-9][0-9][0-9] => (stringBuilder := !stringBuilder ^ asciiCode(yytext); continue());
<STRING> \\[\n\t \f]+\\ => (continue()); 




<INITIAL> [A-Za-z]([A-Za-z0-9_])* => (Tokens.ID(yytext, yypos, yypos + String.size yytext));
<INITIAL> [0-9]+ => (Tokens.INT(valOf(Int.fromString yytext), yypos, yypos + String.size yytext)); 
