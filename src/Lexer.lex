import java_cup.runtime.Symbol;
import java_cup.runtime.ComplexSymbolFactory;
import java_cup.runtime.ComplexSymbolFactory.Location;

%%
%public
%class Lexer
%cup
%implements sym
%unicode
%line
%char
%column


%{
    StringBuffer string = new StringBuffer();
    public Lexer(java.io.Reader in, ComplexSymbolFactory sf){
        this(in);
        symbolFactory = sf;
    }

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

    private Symbol symbol(String name, sym, Object val, int buflength) {
        Location left = new Location(yyline+1, yycolumn+yylength()-buflength, yychar+yylength()-buflength);
        Location right = new Location(yyline+1, yycolumn+yylength(), yychar+yylength());
        return symbolFactory.newSymbol(name, sym, left, right, val);
    }

    private void error(String message) {
        System.out.println("Error at line " + (yyline+1) + ", column " + (yycolumn+1) + " : " + message);
    }

  /**
   * assumes correct representation of a long value for
   * specified radix in scanner buffer from <code>start</code>
   * to <code>end</code>
   */
  private long parseRat(int start, int end, int radix) {
    long result = 0;
    long digit;

    for (int i = start; i < end; i++) {
      digit  = Character.digit(yycharat(i),radix);
      result*= radix;
      result+= digit;
    }

    return result;
  }

  private bool parseBool(String text) {
    if (text.equals("T")) {
        return true;
    } else {
        return false;
    }
  }
%}

%eofval{
     return symbolFactory.newSymbol("EOF", EOF, new Location(yyline+1,yycolumn+1,yychar), new Location(yyline+1,yycolumn+1,yychar+1));
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
Identifier = [a-zA-Z_][a-zA-Z0-9_]*

/* Number Literals */
IntLiteral = 0 | {PInt} | "-" {PInt}
FloatLiteral  = -? [0-9]+ \. [0-9]+
RatLiteral = -? {PInt} "_" {PInt} "/" {PInt} | -? {PInt} "/" {PInt}
BoolLiteral = "T" | "F"
NullLiteral = "null"

PInt = [1-9][0-9]*

/* string and character literals */
StringCharacter = [^\r\n\"\\]
SingleCharacter = [^\r\n\'\\]

%state STRING, CHARLITERAL

%%

<YYINITIAL> {

  /* Keywords for Standard IO */
  "read"                        { return symbol("read", READ); }
  "print"                       { return symbol("print", PRINT); }

  /* keywords for Control Flow*/
  "loop"                        { return symbol("loop", LOOP); }
  "pool"                        { return symbol("pool", POOL); }
  "break"                       { return symbol("break", BREAK); }
  "if"                          { return symbol("if", IF); }
  "fi"                          { return symbol("fi", FI); }
  "else"                        { return symbol("else", ELSE); }

  /* Keywords for Datatypes*/
  "top"                         { return symbol("top", TYPE, new Integer( TOP )); }
  "int"                         { return symbol("int", TYPE, new Integer( INT )); }
  "rat"                         { return symbol("rat", TYPE, new Integer( RAT )); }
  "float"                       { return symbol("float", TYPE, new Integer( FLOAT )); }
  "bool"                        { return symbol("bool", TYPE, new Integer( BOOL )); }
  "char"                        { return symbol("char", TYPE, new Integer( CHAR )); }
  "seq"                         { return symbol("seq", TYPE, new Integer( SEQ )); }
  "dict"                        { return symbol("dict", TYPE, new Integer( DICT )); }

  /* LITERALS */
  /* identifiers */
  {Identifier} { return symbol("Identifier", IDENTIFIER, yytext()); }
  /* boolean literals */
  {BoolLiteral} { return symbol("Boolconst", BOOLCONST, new Boolean(parseBool(yytext()))); }
  /* Integer Literal */
  {IntLiteral} { return symbol("Intconst", INTCONST, new Integer(Integer.parseInt(yytext()))); }

  {NullLiteral} { return symbol("null", NULL); }

  /* HELP ! */
//  {RatLiteral} { return symbol("Ratconst", RATCONST, new Rational}

  /* Float Literal */
  {FloatLiteral} { return symbol(FLOATING_POINT_LITERAL, new Float(yytext().substring(0,yylength()-1))); }

  /* separators & assignment */
  "("                            { return symbol("(", LPAREN); }
  ")"                            { return symbol(")", RPAREN); }
  "{"                            { return symbol("{", LBRACE); }
  "}"                            { return symbol("}", RBRACE); }
  "["                            { return symbol("[", LBRACK); }
  "]"                            { return symbol("]", RBRACK); }
  ";"                            { return symbol("semiconlon", SEMICOLON); }
  ","                            { return symbol("comma", COMMA); }
  ":="                            { return symbol(":=", ASSIGN); }

  /* OPERATORS */
  /* Comparative Operators */
  ">"                            { return symbol("gt", COMP, new Integer( GT )); }
  "<"                            { return symbol("lt", COMP, new Integer( LT )); }
  "=="                           { return symbol("eq", COMP, new Integer( EQ )); }
  "<="                           { return symbol("leq", COMP, new Integer( LEQ )); }
  ">="                           { return symbol("geq", COMP, new Integer( GEQ )); }
  "!="                           { return symbol("neq", COMP, new Integer( NEQ )); }
  "?"                            { return symbol("?", COMP, new Integer( QMARK )); }
  "main"                         { return symbol("main", MAIN); }

  /* Logical Operators */
  "!"                            { return symbol("not", BUNOP); }
  "&&"                           { return symbol("and", BBINOP, new Integer( AND )); }
  "||"                           { return symbol("or", BBINOP, new Integer( OR )); }
  "=>"                           { return symbol("implies", BBINOP, new Integer( IMPLICATION)); }

  /* Binary Operators */
  "+"                            { return symbol("plus", BINOP, new Integer( PLUS )); }
  "-"                            { return symbol("minus", BINOP, new Integer( MINUS )); }
  "*"                            { return symbol("mult", BINOP, new Integer( MULT )); }
  "/"                            { return symbol("div", BINOP, new Integer( DIV )); }
  "^"                            { return symbol("exp", BINOP, new Integer( EXP )); }

  /* Sequence & Dictionary Syntax */
  "in"                           { return symbol("in", IN); }
  "::"                           { return symbol("::", COLONCOLON); }
  ":"                            { return symbol(":", COLON); }

  /* Type aliasing and type definition */
  "tdef"                         { return symbol("tdef", TDEF); }
  "alias"                        { return symbol("alias", ALIAS); }

  /* string literal */
  \"                             { yybegin(STRING); string.setLength(0); }

  /* character literal */
  \'                             { yybegin(CHARLITERAL); }

  /* comments */
  {Comment}                      { /* ignore */ }

  /* whitespace */
  {WhiteSpace}                   { /* ignore */ }
}

<STRING> {
  \"                             { yybegin(YYINITIAL); return symbol(STRING_LITERAL, string.toString()); }

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

<CHARLITERAL> {
  {SingleCharacter}\'            { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, yytext().charAt(0)); }

  /* escape sequences */
  "\\b"\'                        { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\b');}
  "\\t"\'                        { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\t');}
  "\\n"\'                        { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\n');}
  "\\f"\'                        { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\f');}
  "\\r"\'                        { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\r');}
  "\\\""\'                       { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\"');}
  "\\'"\'                        { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\'');}
  "\\\\"\'                       { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\\'); }

  /* error cases */
  \\.                            { throw new RuntimeException("Illegal escape sequence \""+yytext()+"\""); }
  {LineTerminator}               { throw new RuntimeException("Unterminated character literal at end of line"); }
}

/* error fallback */
.|\n                             { throw new RuntimeException("Illegal character \""+yytext()+
                                                              "\" at line "+yyline+", column "+yycolumn); }

