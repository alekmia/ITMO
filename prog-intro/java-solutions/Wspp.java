import java.util.*;
import java.io.*;

public class Wspp{
    public static boolean isGoodChar(char c) {
        return c == '\'' || Character.isLetter(c) || Character.getType(c) == Character.DASH_PUNCTUATION;
    }

    public static void main(String[] args){
        try {
            MyScanner in = new MyScanner(new File(args[0]), "UTF-8");
            Map<String, IntList> bigBadArray = new LinkedHashMap<String, IntList>();
            int wordCounter = 0;
            IntList temp = new IntList();

            while (in.hasNextLine()) {
                MyScanner words = new MyScanner(in.nextLine());
                try {
                    while (words.hasNextWord())
                    {
                        String omg = words.nextWord().toLowerCase();
                        if (bigBadArray.get(omg) == null)
                        {
                            temp = new IntList();
                        } else {
                            temp = bigBadArray.get(omg);
                        }
                        temp.addInt(wordCounter + 1);
                        bigBadArray.put(omg, temp);
                        wordCounter++;
                    }
                } catch (FileNotFoundException e) {
                    System.out.println("file not found " + e.getMessage());
                } catch (IOException e) {
                    System.out.println("ioexception lol " + e.getMessage());
                } finally {
                    words.close();
                }
            }


            in.close();

            BufferedWriter out = new BufferedWriter(
                    new OutputStreamWriter(
                            new FileOutputStream(args[1]), "utf8"
                    )
            );

            for (Map.Entry<String, IntList> mapElement : bigBadArray.entrySet()) {

                String key = mapElement.getKey();
                IntList value = mapElement.getValue();
                out.write(key + " " + value.size());
                for (int i = 0; i < value.size(); i++)
                {
                    out.write(" " + value.getInt(i));
                }
                out.write("\n");

            }

            out.close();

        } catch (FileNotFoundException e) {
            System.out.println("file not found " + e.getMessage());
        } catch (IOException e) {
            System.out.println("ioexception lol " + e.getMessage());
        }
    }
}






















































/*import java.util.*;
import java.io.*;

public class Wspp{
    public static boolean isGoodChar(char c) {
        return c == '\'' || Character.isLetter(c) || Character.getType(c) == Character.DASH_PUNCTUATION;
    }

    public static void main(String[] args){
        try {
            MyScanner in = new MyScanner(System.in);
            //MyScanner in = new MyScanner(new File(args[0]), "UTF-8");
            Map<String, IntList> bigBadArray = new LinkedHashMap<String, IntList>();
            int wordCounter = 0;
            IntList temp = new IntList();
            
            while (in.hasNextLine()) {
                MyScanner words = new MyScanner(in.nextLine());
                try {
                    while(words.hasNextWord())
                    {
                        String omg = words.nextWord().toLowerCase();
                        if (bigBadArray.get(omg) == null)
                        {
                            temp = new IntList();
                        } else {
                            temp = bigBadArray.get(omg);
                        }
                        temp.addInt(wordCounter + 1);
                        bigBadArray.put(omg, temp);
                        wordCounter++;  
                    }
                } catch (FileNotFoundException e) {
                    System.out.println("file not found " + e.getMessage());
                } catch (IOException e) {
                    System.out.println("ioexception lol " + e.getMessage());
                } finally {
                    words.close();
                }
            }

            in.close();
            
            // BufferedWriter out = new BufferedWriter(
            //     new OutputStreamWriter(
            //         new FileOutputStream(args[1]), "utf8"
            //     )
            // );

            for (Map.Entry<String, IntList> mapElement : bigBadArray.entrySet()) {
  
                String key = mapElement.getKey();
                IntList value = mapElement.getValue();
                System.out.print(key);
                for(int i = 0; i <= value.size(); i++)
                {
                    System.out.print(" " + value.getInt(i));
                }
                System.out.print("\n");

            }

            //out.close();

        } catch (FileNotFoundException e) {
            System.out.println("file not found " + e.getMessage());
        } catch (IOException e) {
            System.out.println("ioexception lol " + e.getMessage());
        } 
    }
}*/