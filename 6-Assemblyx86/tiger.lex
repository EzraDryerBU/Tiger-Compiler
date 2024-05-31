
val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val commNest = ref 0
val string = ref ""
val instring = ref false

fun eof() = if !commNest = 0
                then if not (!instring)
                     then let val pos = hd(!linePos) in Tokens.EOF(pos, pos) end
                     else (ErrorMsg.error 0 "unclosed string"; raise ErrorMsg.Error)
                else (ErrorMsg.error 0 "unclosed comment"; raise ErrorMsg.Error)

type svalue = Tokens.svalue
type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

%%
%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
%s COMMENT STRING STRINGWHITE;
%%
<INITIAL> \n	    => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> [\ \t\r]+   => (continue ());
<INITIAL> type      => (Tokens.TYPE(yypos, yypos + 4));
<INITIAL> var  	    => (Tokens.VAR(yypos, yypos + 3));
<INITIAL> function  => (Tokens.FUNCTION(yypos, yypos + 8));
<INITIAL> break     => (Tokens.BREAK(yypos, yypos + 5));
<INITIAL> of        => (Tokens.OF(yypos, yypos + 2));
<INITIAL> end       => (Tokens.END(yypos, yypos + 3));
<INITIAL> in        => (Tokens.IN(yypos, yypos + 2));
<INITIAL> nil       => (Tokens.NIL(yypos, yypos + 3));
<INITIAL> let       => (Tokens.LET(yypos, yypos + 3));
<INITIAL> do        => (Tokens.DO(yypos, yypos + 2));
<INITIAL> to        => (Tokens.TO(yypos, yypos + 2));
<INITIAL> for       => (Tokens.FOR(yypos, yypos + 3));
<INITIAL> while     => (Tokens.WHILE(yypos, yypos + 5));
<INITIAL> else      => (Tokens.ELSE(yypos, yypos + 4));
<INITIAL> then      => (Tokens.THEN(yypos, yypos + 4));
<INITIAL> if        => (Tokens.IF(yypos, yypos + 2));
<INITIAL> array     => (Tokens.ARRAY(yypos, yypos + 5));
<INITIAL> ":="      => (Tokens.ASSIGN(yypos, yypos + 2));
<INITIAL> "|"       => (Tokens.OR(yypos, yypos + 1));
<INITIAL> "&"       => (Tokens.AND(yypos, yypos + 1));
<INITIAL> ">="      => (Tokens.GE(yypos, yypos + 2));
<INITIAL> ">"       => (Tokens.GT(yypos, yypos + 1));
<INITIAL> "<="      => (Tokens.LE(yypos, yypos + 2));
<INITIAL> "<"       => (Tokens.LT(yypos, yypos + 1));
<INITIAL> "<>"      => (Tokens.NEQ(yypos, yypos + 2));
<INITIAL> "="       => (Tokens.EQ(yypos, yypos + 1));
<INITIAL> "/"       => (Tokens.DIVIDE(yypos, yypos + 1));
<INITIAL> "*"       => (Tokens.TIMES(yypos, yypos + 1));
<INITIAL> "-"       => (Tokens.MINUS(yypos, yypos + 1));
<INITIAL> "+"       => (Tokens.PLUS(yypos, yypos + 1));
<INITIAL> "."       => (Tokens.DOT(yypos, yypos + 1));
<INITIAL> "}"       => (Tokens.RBRACE(yypos, yypos + 1));
<INITIAL> "{"       => (Tokens.LBRACE(yypos, yypos + 1));
<INITIAL> "]"       => (Tokens.RBRACK(yypos, yypos + 1));
<INITIAL> "["       => (Tokens.LBRACK(yypos, yypos + 1));
<INITIAL> ")"       => (Tokens.RPAREN(yypos, yypos + 1));
<INITIAL> "("       => (Tokens.LPAREN(yypos, yypos + 1));
<INITIAL> ";"	    => (Tokens.SEMICOLON(yypos, yypos + 1));
<INITIAL> ":"	    => (Tokens.COLON(yypos, yypos + 1));
<INITIAL> ","	    => (Tokens.COMMA(yypos, yypos + 1));
<INITIAL> "/*"      => (YYBEGIN COMMENT; commNest := 1; continue());
<INITIAL> \"        => (YYBEGIN STRING; instring := true; string := ""; continue());
<INITIAL> [0-9]+    => (Tokens.INT(valOf (Int.fromString yytext), yypos, yypos + String.size yytext));
<INITIAL> [a-zA-Z][a-zA-Z0-9_]* => (Tokens.ID(yytext, yypos, yypos + String.size yytext));
<INITIAL> .         => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

<STRING> \\n       => (string := !string ^ "\n"; continue());
<STRING> \\t       => (string := !string ^ "\t"; continue());

<STRING> \\([0-9]{3}) => (let val code = case String.explode yytext of _ :: yy => String.implode yy
                              val ch = Char.chr (valOf (Int.fromString code)) in
                          string := !string ^ Char.toString ch end;
                          continue());


<STRING> \\\"      => (string := !string ^ "\""; continue());
<STRING> \\\\      => (string := !string ^ "\\"; continue());
<STRING> \"        => (YYBEGIN INITIAL; instring := false; Tokens.STRING(!string, yypos, yypos + 2 + String.size (!string)));
<STRING> \\        => (YYBEGIN STRINGWHITE; continue());
<STRING> .         => (string := !string ^ yytext; continue());

<STRINGWHITE> \n	     => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<STRINGWHITE> [\ \t\r]   => (continue());
<STRINGWHITE> \\         => (YYBEGIN STRING; continue());

<COMMENT> "/*"      => (commNest := !commNest + 1; continue());
<COMMENT> "*/"      => (if !commNest = 1 then (commNest := 0; YYBEGIN INITIAL; continue())
                                         else (commNest := !commNest - 1; continue()));
<COMMENT> \n        => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<COMMENT> .         => (continue());
