import java_cup.runtime.*;
import java.io.*;

public class TestLexer{
    public static void main (String[] args) throws IOException {
        Lexer lexer = new Lexer(new FileReader(args[0]));
        Symbol sym;
        for (sym = lexer.next_token(); sym.sym != 0; sym = lexer.next_token()) {
            System.out.println("Token " + sym +
                    ", with value = " + sym.value +
                    "; at line " + sym.left + ", column " + sym.right);

        }
    }

}