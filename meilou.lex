structure T = Tokens

type pos = int
type svalue = T.svalue
type ('a, 'b) token = ('a, 'b) T.token
type lexresult = (svalue, pos) T.token

val lineno = ref 1
fun eof () = T.EOF (0, 0)

fun println s = print (s ^ "\n")
%%

%header (functor MeiLouLexFun (structure Tokens: MeiLou_TOKENS));

%s COMMENT;

letter = [a-zA-Z];
digit  = [0-9];
symbol = [!%&$#+\-/:<=>?@\\~`\^|*];
ws     = [ \t];
eol    = "\013\n" | "\n" | "\013";

int    = "~"?{digit}+;
id     = {letter}({letter}|{digit}|"'"|"_")*;

%%
<INITIAL>{ws}+     => ( continue () );
<INITIAL>{eol}     => ( lineno := !lineno + 1; continue () );

<INITIAL>"andalso" => ( println yytext; T.ANDALSO (yypos, yypos + size yytext) );
<INITIAL>"else"    => ( println yytext; T.ELSE (yypos, yypos + size yytext) );
<INITIAL>"end"     => ( println yytext; T.END (yypos, yypos + size yytext) );
<INITIAL>"fun"     => ( println yytext; T.FUN (yypos, yypos + size yytext) );
<INITIAL>"if"      => ( println yytext; T.IF (yypos, yypos + size yytext) );
<INITIAL>"in"      => ( println yytext; T.IN (yypos, yypos + size yytext) );
<INITIAL>"let"     => ( println yytext; T.LET (yypos, yypos + size yytext) );
<INITIAL>"orelse"  => ( println yytext; T.ORELSE (yypos, yypos + size yytext) );
<INITIAL>"then"    => ( println yytext; T.THEN (yypos, yypos + size yytext) );
<INITIAL>"val"     => ( println yytext; T.VAL (yypos, yypos + size yytext) );

<INITIAL>"("       => ( println yytext; T.LPAREN (yypos, yypos + size yytext) );
<INITIAL>")"       => ( println yytext; T.RPAREN (yypos, yypos + size yytext) );
<INITIAL>";"       => ( println yytext; T.SEMICOLON (yypos, yypos + size yytext) );
<INITIAL>"<"       => ( println yytext; T.LT (yypos, yypos + size yytext) );
<INITIAL>"<="      => ( println yytext; T.LE(yypos, yypos + size yytext) );
<INITIAL>">"       => ( println yytext; T.GT (yypos, yypos + size yytext) );
<INITIAL>">="      => ( println yytext; T.GE (yypos, yypos + size yytext) );
<INITIAL>"="       => ( println yytext; T.EQ (yypos, yypos + size yytext) );
<INITIAL>"<>"      => ( println yytext; T.NEQ (yypos, yypos + size yytext) );
<INITIAL>"+"       => ( println yytext; T.PLUS (yypos, yypos + size yytext) );
<INITIAL>"-"       => ( println yytext; T.MINUS (yypos, yypos + size yytext) );
<INITIAL>"*"       => ( println yytext; T.TIMES (yypos, yypos + size yytext) );
<INITIAL>"div"     => ( println yytext; T.DIV (yypos, yypos + size yytext) );
<INITIAL>"not"     => ( println yytext; T.NOT (yypos, yypos + size yytext) );

<INITIAL>"true"    => ( println yytext; T.BOOL (true, yypos, yypos + size yytext) );
<INITIAL>"false"   => ( println yytext; T.BOOL (false, yypos, yypos + size yytext) );
<INITIAL>{int}     => ( println yytext; T.INT (valOf (Int.fromString yytext),
                               yypos,
                               yypos + size yytext) );
<INITIAL>{id}      => ( println yytext; T.ID (yytext, yypos, yypos + size yytext) );



