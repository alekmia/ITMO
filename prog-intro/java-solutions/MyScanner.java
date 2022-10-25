import java.io.*;
import java.nio.charset.StandardCharsets;

class MyScanner {
    private Reader in;
    char[] buffer = new char[1024];
    String word;
    String line;
    String nextNumber;
    String nextWord;
    int amount = 0; 
    int pos = 0; 
    int inLine = 0;
    boolean hex = false;
    boolean canHex = false;

    public MyScanner(File input) throws IOException {
        this.in = new FileReader(input);
    }

    public MyScanner(InputStream input) throws IOException {
        this.in = new InputStreamReader(input);
    }

    public MyScanner(String input) throws IOException {
        this.in = new StringReader(input);
        this.line = input;
        buffer();
    }

    public MyScanner(File file, String a) throws IOException {
        this.in = new InputStreamReader(new FileInputStream(file), StandardCharsets.UTF_8.name());
    }

    public void switchHex()
    {
        canHex = true;
        return;
    }

    private boolean buffer() throws IOException {
        if (pos == amount) {                             
            amount = in.read(buffer);                
            pos = 0;                                  
        }                                               
        return amount != -1;
    }

//////////////////////////////////////////////////////////////////////////

//                       TEACHERS COMMENTS

// 

    public static boolean isGoodChar(char c, String type) {
        switch (type) {
            case("word"):
                return c == '\'' || Character.isLetter(c) || Character.getType(c) == Character.DASH_PUNCTUATION;
            case("int"):
                return !Character.isWhitespace(c);
        }
        return false;
    }
    
////            NEXT LINE          

    public boolean hasNextLine() throws IOException { 
        return buffer();
    }

    public String nextLine() throws IOException {
        StringBuilder str = new StringBuilder();
        while (buffer() && (buffer[pos] != '\n')) {
            str.append(buffer[pos++]);
        }
        pos++;

        inLine = 0;
        line = str.toString();

        return line;
    }

///////////                 NEXT INT

    public boolean hasNextInt() throws IOException {
        return hasNext("int");
    }

    public String nextInt() throws IOException {
        if (canHex && !hex) {
            return "0x" + Integer.toHexString(Integer.parseInt(nextNumber));
        }
        return nextNumber;
    }

////                       NEXT WORD

    public boolean hasNextWord() throws IOException {
        return hasNext("word");
    }

    public String nextWord() throws IOException {
        return nextWord;
    }


//                        NEXT (FOR BOTH INTs AND WORDs)


    public boolean hasNext(String type) {
        for (; inLine < line.length(); inLine++) {
            if (isGoodChar(line.charAt(inLine), type))
            {
                int temp = inLine;
                while (inLine < line.length() && isGoodChar(line.charAt(inLine), type)) {
                    inLine++;
                }
                String tempString = line.substring(temp, inLine);
                if (type == "int"){
                    hex = false;
                    if (inLine - temp >= 2) {
                        if (tempString.charAt(0) == '0' && Character.toLowerCase(tempString.charAt(1)) == 'x') {
                            hex = true;
                        } 
                    } 
                    nextNumber = tempString;
                } else {
                    nextWord = tempString;
                }
                return true;
            }
            
        }
        return false;
    }
    
    // public String next() {
        
    //     return "0";
    // }
    
    public void close() throws IOException {
        in.close();
    }
}