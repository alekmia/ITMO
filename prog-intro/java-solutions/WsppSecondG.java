import java.util.*;
import java.io.*;

public class WsppSecondG {
    public static boolean isGoodChar(char c) {
        return c == '\'' || Character.isLetter(c) || Character.getType(c) == Character.DASH_PUNCTUATION;
    }

    public static void main(String[] args){
        try{
            MyScanner in = new MyScanner(new File(args[0]), "UTF-8");
            Map<String, IntList> modMap = new LinkedHashMap<String, IntList>();
            Map<String, Integer> allCounter = new HashMap<String, Integer>();
            int wordCounter = 0;
            IntList temp = new IntList();

            while (in.hasNextLine()) {
                MyScanner words = new MyScanner(in.nextLine());
                Map<String, Integer> lineEvenCounter = new HashMap<String, Integer>();
                try {
                    while (words.hasNextWord()) {
                        String nextLowercaseWord = words.nextWord().toLowerCase();
                        if (lineEvenCounter.containsKey(nextLowercaseWord)) {
                            lineEvenCounter.put(nextLowercaseWord, lineEvenCounter.get(nextLowercaseWord) + 1);
                        } else {
                            lineEvenCounter.put(nextLowercaseWord, 1);
                        }

                        if (!modMap.containsKey(nextLowercaseWord))
                        {
                            allCounter.put(nextLowercaseWord, 1);
                            temp = new IntList();
                        } else {
                            allCounter.put(nextLowercaseWord, allCounter.get(nextLowercaseWord) + 1);
                            temp = modMap.get(nextLowercaseWord);
                        }
                        
                        //temp.set(0, temp.getInt(0) + 1);

                       if (lineEvenCounter.get(nextLowercaseWord) % 2 == 0) {
                            temp.addInt(wordCounter + 1);
                        }

                        modMap.put(nextLowercaseWord, temp);

                        wordCounter++;
                    }
                } catch (FileNotFoundException e) {
                    System.out.println("file not found " + e.getMessage());
                } catch (IOException e) {
                    System.out.println("ioexception lol1 " + e.getMessage());
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
            try {
                for (Map.Entry<String, IntList> mapElement : modMap.entrySet()) {

                    String key = mapElement.getKey();
                    IntList value = mapElement.getValue();
                    out.write(key + " " + allCounter.get(key));
                    for (int i = 0; i < value.size(); i++)
                    {
                        out.write(" " + value.getInt(i));
                    }
                    out.write("\n");

                }
            }catch (FileNotFoundException e) {
                System.out.println("file not found " + e.getMessage());
            } catch (IOException e) {
                System.out.println("ioexception lol2 " + e.getMessage());
            } finally {
                out.close();
            }
        } catch (FileNotFoundException e) {
            System.out.println("file not found " + e.getMessage());
        } catch (IOException e) {
            System.out.println("ioexception lol3 " + e.getMessage());
        }
    }
}





//                    MyScanner words = new MyScanner(in.nextLine());
//                    for (Map.Entry<String, IntList> mapElement : modMap.entrySet()) {
//                        IntList value = mapElement.getValue();
//                        value.set(0, 0);
//                    }
//                    try {
//                        while (words.hasNextWord()) {
//                            String omg = words.nextWord().toLowerCase();
//                            if (bigBadArray.get(omg) == null) {
//                                temp = new IntList();
//                            } else {
//                                temp = bigBadArray.get(omg);
//                            }
//                            temp.addInt(wordCounter + 1);
//                            bigBadArray.put(omg, temp);
//                            wordCounter++;
//                        }
//                    } catch (FileNotFoundException e) {
//                        System.out.println("file not found " + e.getMessage());
//                    } catch (IOException e) {
//                        System.out.println("ioexception lol " + e.getMessage());
//                    } finally {
//                        words.close();
//                    }

//    public static void main(String[] args){
//        try {
//            MyScanner in = new MyScanner(new File(args[0]), "UTF-8");
//            Map<String, IntList> modMap = new LinkedHashMap<String, IntList>();
//            int wordCounter = 1;
//            String nextLowercaseWord;
//            // String slashN = new String("\n");
//
//            // while (in.hasNextLine()) {
//            // :NOTE: Out-of-memory
//            while(in.hasNextWord()) {
//                try {
//                    nextLowercaseWord = in.nextWord().toLowerCase();
//                    if (!nextLowercaseWord.equals(System.lineSeparator())) {
//                        for (Map.Entry<String, IntList> mapElement : modMap.entrySet()) {
//                            IntList value = mapElement.getValue();
//                            value.set(0, 0);
//                        }
//                        System.err.println(nextLowercaseWord + " ");
//                        IntList temp = (!modMap.containsKey(nextLowercaseWord)) ? new IntList() : modMap.get(nextLowercaseWord);
//                        if (temp.getInt(0) % 2 == 1) {
//                            temp.addInt(wordCounter);
//                        } else {
//                            temp.sizeChange();
//                        }
//                        temp.set(0, temp.getInt(0) + 1);
//                        modMap.put(nextLowercaseWord, temp);
//                        wordCounter++;
//                    }
//                } catch (FileNotFoundException e) {
//                    System.out.println("file not found " + e.getMessage());
//                } catch (IOException e) {
//                    System.out.println("ioexception lol " + e.getMessage());
//                } finally {
//                    in.close();
//                }
//            }
//            try {
//                BufferedWriter out = new BufferedWriter(
//                        new OutputStreamWriter(
//                                new FileOutputStream(args[1]), "utf8"
//                        )
//                );
//                for (Map.Entry<String, IntList> mapElement : modMap.entrySet()) {
//
//                    String key = mapElement.getKey();
//                    IntList value = mapElement.getValue();
//                    out.write(key + " " + value.size());
//                    for (int i = 1; i < value.getCount(); i++) {
//                        out.write(" " + value.getInt(i));
//                    }
//                    out.write("\n");
//
//                }
//                out.close();
//            } catch (FileNotFoundException e) {
//                System.out.println("file not found " + e.getMessage());
//            } catch (IOException e) {
//                System.out.println("ioexception lol " + e.getMessage());
//            }
//        } catch (FileNotFoundException e) {
//            System.out.println("file not found " + e.getMessage());
//        } catch (IOException e) {
//            System.out.println("ioexception lol " + e.getMessage());
//        }
//    }