import java_cup.runtime.Symbol;
import java_cup.runtime.ComplexSymbolFactory;
import java_cup.runtime.ComplexSymbolFactory.Location;

%%
%public
%class Lexer
%cup
%char
%line
%column


%{
    StringBuffer string = new StringBuffer();

    ComplexSymbolFactory symbolFactory = new ComplexSymbolFactory();

    private Symbol symbol(String name, int sym) {
        Location left = new Location(yyline+1, yycolumn+1, yychar);
        Location right = new Location(yyline+1, yycolumn+yylength(), yychar+yylength());
        return symbolFactory.newSymbol(name, sym, left, right);
    }

    private Symbol symbol(String name, int sym, Object val) {
        Location left = new Location(yyline+1, yycolumn+1, yychar);
        Location right = new Location(yyline+1, yycolumn+yylength(), yychar+yylength());
        return symbolFactory.newSymbol(name, sym, left, right, val);
    }

    private Symbol symbol(String name, int sym, Object val, int buflength) {
        Location left = new Location(yyline+1, yycolumn+yylength()-buflength, yychar+yylength()-buflength);
        Location right = new Location(yyline+1, yycolumn+yylength(), yychar+yylength());
        return symbolFactory.newSymbol(name, sym, left, right, val);
    }

    private void error(String message) {
        System.out.println("Error at line " + (yyline+1) + ", column " + (yycolumn+1) + " : " + message);
    }
%}

%eofval{
     return symbolFactory.newSymbol("EOF", sym.EOF, new Location(yyline+1,yycolumn+1,yychar), new Location(yyline+1,yycolumn+1,yychar+1));
%eofval}

/* main character classes */
LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]
WhiteSpace = {LineTerminator} | [ \t\f]

/* comments */
Comment = {MultiLineComment} | {SingleLineComment}

MultiLineComment = "/#" [^#] ~"#/" | "/#" "#"+ "/"
SingleLineComment = "#" {InputCharacter}* {LineTerminator}?

/* identifiers */
Identifier = [a-zA-Z][a-zA-Z0-9_]*

/* Literal Components */
IntLiteral = 0 | {PInt}
FloatLiteral  = [0-9]+ \. [0-9]+
RatLiteral = {PInt} "_" {PInt} "/" {PInt} | {PInt} "/" {PInt}
BoolLiteral = "T" | "F"
NullLiteral = "null"

PInt = [1-9][0-9]*

/* string and character literals */
StringCharacter = [^\r\n\"\\]
SingleCharacter = [^\r\n\'\\]

%state STRING, CHAR

%%

<YYINITIAL> {

  /* Keywords for Standard IO */
  "read"                        { return symbol("read", sym.READ); }
  "print"                       { return symbol("print", sym.PRINT); }

  /* keywords for Control Flow*/
  "loop"                        { return symbol("loop", sym.LOOP); }
  "pool"                        { return symbol("pool", sym.POOL); }
  "break"                       { return symbol("break", sym.BREAK); }
  "if"                          { return symbol("if", sym.IF); }
  "fi"                          { return symbol("fi", sym.FI); }
  "then"                        { return symbol("then", sym.THEN); }
  "else"                        { return symbol("else", sym.ELSE); }
  "return"                        { return symbol("return", sym.RETURN); }

  /* Keywords for Datatypes*/
  "top"                         { return symbol("top", sym.TOP); }
  "int"                         { return symbol("int", sym.INT); }
  "rat"                         { return symbol("rat", sym.RAT); }
  "float"                       { return symbol("float", sym.FLOAT); }
  "boolean"                     { return symbol("bool", sym.BOOLEAN); }
  "string"                        { return symbol("string", sym.STRING); }
  "char"                        { return symbol("char", sym.CHAR); }
  "seq"                         { return symbol("seq", sym.SEQ); }
  "dict"                        { return symbol("dict", sym.DICT); }


  /* LITERALS */

  /* boolean literals */
  {BoolLiteral} { return symbol("Boolconst", sym.BOOLEAN_LITERAL, yytext()); }

  /* Integer Literal */
  {IntLiteral} { return symbol("Intconst", sym.INTEGER_LITERAL, yytext()); }

  /* Null Literal */
  {NullLiteral} { return symbol("Null", sym.NULL_LITERAL, yytext()); }

  /* Rational Literal*/
  {RatLiteral} { return symbol("Ratconst", sym.RATIONAL_LITERAL, yytext()); }

  /* Float Literal */
  {FloatLiteral} { return symbol("Floatconst", sym.FLOATING_POINT_LITERAL, yytext()); }

  /* separators & assignment */
  "("                            { return symbol("(", sym.LPAREN); }
  ")"                            { return symbol(")", sym.RPAREN); }
  "{"                            { return symbol("{", sym.LBRACE); }
  "}"                            { return symbol("}", sym.RBRACE); }
  "["                            { return symbol("[", sym.LBRACK); }
  "]"                            { return symbol("]", sym.RBRACK); }
  ";"                            { return symbol("semicolon", sym.SEMICOLON); }
  ","                            { return symbol("comma", sym.COMMA); }
  ":="                            { return symbol(":=", sym.ASSIGN); }

  /* OPERATORS */
  /* Comparative Operators */
  ">"                            { return symbol("gt", sym.GT ); }
  "<"                            { return symbol("lt", sym.LT ); }
  "=="                           { return symbol("eq", sym.EQ ); }
  "<="                           { return symbol("leq", sym.LEQ ); }
  ">="                           { return symbol("geq", sym.GEQ ); }
  "!="                           { return symbol("neq", sym.NEQ ); }

  /*Function predicate operator*/
  "?"                            { return symbol("?", sym.QMARK); }

  "main"                         { return symbol("main", sym.MAIN); }

  /* Logical Operators */
  "!"                            { return symbol("not", sym.BUNOP); }
  "&&"                           { return symbol("and", sym.BBINOP, new Integer( sym.AND )); }
  "||"                           { return symbol("or", sym.BBINOP, new Integer( sym.OR )); }
  "=>"                           { return symbol("implies", sym.BBINOP, new Integer( sym.IMPLICATION)); }

  /* Binary Operators */
  "+"                            { return symbol("plus", sym.BINOP, new Integer( sym.PLUS )); }
  "-"                            { return symbol("minus", new Integer( sym.MINUS )); }
  "*"                            { return symbol("mult", sym.BINOP, new Integer( sym.MULT )); }
  "/"                            { return symbol("div", sym.BINOP, new Integer( sym.DIV )); }
  "^"                            { return symbol("exp", sym.BINOP, new Integer( sym.EXP )); }

  /* Sequence & Dictionary Syntax */
  "in"                           { return symbol("in", sym.IN); }
  "::"                           { return symbol("::", sym.COLONCOLON); }
  ":"                            { return symbol(":", sym.COLON); }

  /* Type aliasing and type definition */
  "tdef"                         { return symbol("tdef", sym.TDEF); }
  "fdef"                         { return symbol("fdef", sym.FDEF); }
  "alias"                        { return symbol("alias", sym.ALIAS); }

  /* string literal */
  \"                             { yybegin(STRING); string.setLength(0); }

  /* character literal */
  \'                             { yybegin(CHAR); }

  /* identifiers */
  {Identifier}                   { return symbol("Identifier", sym.IDENTIFIER, yytext()); }

  /* comments */
  {Comment}                      { /* ignore */ }

  /* whitespace */
  {WhiteSpace}                   { /* ignore */ }
}

<STRING> {
  \"                             { yybegin(YYINITIAL); return symbol("string", sym.STRING_LITERAL, string.toString(), string.length()); }

  {StringCharacter}+             { string.append( yytext() ); }

  /* escape sequences */
  "\\b"                          { string.append( '\b' ); }
  "\\t"                          { string.append( '\t' ); }
  "\\n"                          { string.append( '\n' ); }
  "\\f"                          { string.append( '\f' ); }
  "\\r"                          { string.append( '\r' ); }
  "\\\""                         { string.append( '\"' ); }
  "\\'"                          { string.append( '\'' ); }
  "\\\\"                         { string.append( '\\' ); }


  /* error cases */
  \\.                            { throw new RuntimeException("Illegal escape sequence \""+yytext()+"\""); }
  {LineTerminator}               { throw new RuntimeException("Unterminated string at end of line"); }
}

<CHAR> {
  {SingleCharacter}\'            { yybegin(YYINITIAL); return symbol("Charconst", sym.CHARACTER_LITERAL, yytext().charAt(0)); }

  /* escape sequences */
  "\\b"\'                        { yybegin(YYINITIAL); return symbol("Charconst", sym.CHARACTER_LITERAL, '\b');}
  "\\t"\'                        { yybegin(YYINITIAL); return symbol("Charconst", sym.CHARACTER_LITERAL, '\t');}
  "\\n"\'                        { yybegin(YYINITIAL); return symbol("Charconst", sym.CHARACTER_LITERAL, '\n');}
  "\\f"\'                        { yybegin(YYINITIAL); return symbol("Charconst", sym.CHARACTER_LITERAL, '\f');}
  "\\r"\'                        { yybegin(YYINITIAL); return symbol("Charconst", sym.CHARACTER_LITERAL, '\r');}
  "\\\""\'                       { yybegin(YYINITIAL); return symbol("Charconst", sym.CHARACTER_LITERAL, '\"');}
  "\\'"\'                        { yybegin(YYINITIAL); return symbol("Charconst", sym.CHARACTER_LITERAL, '\'');}
  "\\\\"\'                       { yybegin(YYINITIAL); return symbol("Charconst", sym.CHARACTER_LITERAL, '\\'); }

  /* error cases */
  \\.                            { throw new RuntimeException("Illegal escape sequence \""+yytext()+"\""); }
  {LineTerminator}               { throw new RuntimeException("Unterminated character literal at end of line"); }
}

/* error fallback */
[^]                           { throw new RuntimeException("Illegal character \""+yytext()+"\" at line "+yyline+", column "+yycolumn); }

