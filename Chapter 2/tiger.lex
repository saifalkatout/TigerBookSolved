type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


fun replaceAll (source, target, replacement) =
    let
        val lenTarget = String.size target
        fun replaceHelper s =
            if String.size s < lenTarget then s
            else if String.substring (s, 0, lenTarget) = target then
                replacement ^ replaceHelper (String.substring (s, lenTarget, String.size s - lenTarget))
            else
                String.str (String.sub (s, 0)) ^ replaceHelper (String.extract (s, 1, NONE))
    in
        replaceHelper source
    end;

(*   ))) *)
fun cleanString (source): string  = (replaceAll (replaceAll (replaceAll (replaceAll (source, "\\t", "\t"), "\\n", "\n"), "\\\"", "\""), "\\\\", "\\"));

(* "/*" => (YYBEGIN COMMENT; continue());
<COMMENT>"*/" => (YYBEGIN INITIAL; continue());
<COMMENT>.  => (continue()); *)
%% 
%s COMMENT;
COMMENT=\/\*(.|\n)*\*\/ ;
%%

\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
","	=> (Tokens.COMMA(yypos,yypos+1));
[0-9]+	=> (Tokens.INT(Option.getOpt (Int.fromString yytext, 0),yypos,yypos+ size yytext));
[a-z]([a-z0-9]|_)*  => (Tokens.ID(yytext, yypos, yypos + size yytext));
\".*\"  => (Tokens.STRING(cleanString yytext, yypos, yypos + size yytext));
"=" => (Tokens.ASSIGN(yypos, yypos + 1));
(" "|\t|\f) => (continue());

"+"     => (Tokens.PLUS(yypos,yypos+1));
"-"     => (Tokens.MINUS(yypos,yypos+1));
"*"     => (Tokens.TIMES(yypos,yypos+1));
"/"     => (Tokens.DIVIDE(yypos,yypos+1));
"," 	=> (Tokens.COMMA(yypos,yypos+1));
"&"		=> (Tokens.AND(yypos,yypos+1));
"|"     => (Tokens.OR(yypos,yypos+1));
"="     => (Tokens.EQ(yypos,yypos+1));
">="	=> (Tokens.GE(yypos,yypos+2));
">"		=> (Tokens.GT(yypos,yypos+1));
"<="	=> (Tokens.LE(yypos,yypos+2));
"<"		=> (Tokens.LT(yypos,yypos+1));
"<>"	=> (Tokens.NEQ(yypos,yypos+2));
"."		=> (Tokens.DOT(yypos,yypos+1));
":"     => (Tokens.COLON(yypos,yypos+1));
";"     => (Tokens.SEMICOLON(yypos,yypos+1));
":="	=> (Tokens.ASSIGN(yypos,yypos+2));
"{"     => (Tokens.LBRACE(yypos,yypos+1));
"}"     => (Tokens.RBRACE(yypos,yypos+1));
"("     => (Tokens.LPAREN(yypos,yypos+1));
")"     => (Tokens.RPAREN(yypos,yypos+1));
"["     => (Tokens.LBRACK(yypos,yypos+1));
"]"     => (Tokens.RBRACK(yypos,yypos+1));

var  	 => (Tokens.VAR(yypos,yypos+3));
function => (Tokens.FUNCTION(yypos,yypos+8));
break    => (Tokens.BREAK(yypos,yypos+5));
of       => (Tokens.OF(yypos,yypos+2));
end      => (Tokens.END(yypos,yypos+3));
in       => (Tokens.IN(yypos,yypos+2));
nil      => (Tokens.NIL(yypos,yypos+3));
let      => (Tokens.LET(yypos,yypos+3));
do       => (Tokens.DO(yypos,yypos+2));
to       => (Tokens.TO(yypos,yypos+2));
for      => (Tokens.FOR(yypos,yypos+3));
while    => (Tokens.WHILE(yypos,yypos+5));
else     => (Tokens.ELSE(yypos,yypos+4));
then     => (Tokens.THEN(yypos,yypos+4));
if       => (Tokens.IF(yypos,yypos+2));
array    => (Tokens.ARRAY(yypos,yypos+5));
type     => (Tokens.TYPE(yypos,yypos+4));
{COMMENT}    => (continue());
"/*" => (ErrorMsg.error yypos ("Unclosed comment "); continue());

.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
