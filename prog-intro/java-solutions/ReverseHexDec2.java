import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.io.*;

public class ReverseHexDec2 {
    public static void main(String[] args) {
        try {
            MyScanner scMain = new MyScanner(System.in);
            scMain.switchHex();
            List<String[]> matrix = new ArrayList<String[]>();
            int i = 0;
            int j = 0;
            while (scMain.hasNextLine()) {
                MyScanner scEach = new MyScanner(scMain.nextLine());
                scEach.switchHex();
                try {
                    j = 0;
                    String[] temp = new String[1];
                    while (scEach.hasNextInt()) {
                        if (j >= temp.length) {
                            temp = Arrays.copyOf(temp, (j * 3) / 2  + 1);
                        }

                        temp[j] = scEach.nextInt();
                        
                        j++;
                    }
                    temp = Arrays.copyOf(temp, j);
                    matrix.add(temp); 
                } catch (FileNotFoundException e) {
                    System.out.println("file not found " + e.getMessage());
                } catch (IOException e) {
                    System.out.println("ioexception lol " + e.getMessage());
                } finally {
                    scEach.close();
                }
            } 
            for (int x = matrix.size() - 1; x >= 0; x--) {
                for (int y = matrix.get(x).length - 1; y >= 0; y--) {
                    System.out.print(matrix.get(x)[y] + " "); 
                }
                System.out.println();
            }
            scMain.close();
        } 
        catch (FileNotFoundException e) {
            System.out.println("file not found " + e.getMessage());
        } catch (IOException e) {
            System.out.println("ioexception lol " + e.getMessage());
        }
    } 
}






























// import java.util.List;
// import java.util.ArrayList;
// import java.util.Arrays;
// import java.io.*;

// public class ReverseHexDec2 {
//     public static void main(String[] args) {
//         try {
//             MyScanner scmain = new MyScanner(System.in);
//             List<String[]> matrix = new ArrayList<String[]>();
//             int i = 0;
//             int j = 0;
//             while (scmain.hasNextLine()) {
//                 MyScanner sceach = new MyScanner(scmain.nextLine());
//                 try {
//                     j = 0;
//                     String[] temp = new String[1];
//                     while (sceach.hasAmount()) {
//                         if (j >= temp.length) {
//                             temp = Arrays.copyOf(temp, j * 3);
//                         }
//                         String che = sceach.next();
//                         if (!che.isEmpty()) {
//                             if (!sceach.isHex()) {
//                                 temp[j] = "0x" + Integer.toHexString(Integer.parseInt(che));
//                             } else {
//                                 temp[j] = che;
//                             }
//                         }
//                         j++;
//                     }
//                     temp = Arrays.copyOf(temp, j);
//                     matrix.add(temp); 
//                 } catch (FileNotFoundException e) {
//                     System.out.println("file not found " + e.getMessage());
//                 } catch (IOException e) {
//                     System.out.println("ioexception lol " + e.getMessage());
//                 } finally {
//                     sceach.close();
//                 }
//             } 
//             for (int x = matrix.size() - 1; x >= 0; x--) {
//                 for (int y = matrix.get(x).length - 2; y >= 0; y--) {
//                     System.out.print(matrix.get(x)[y] + " "); 
//                 }
//                 System.out.println();
//             }
//             scmain.close();
//         } 
//         catch (FileNotFoundException e) {
//             System.out.println("file not found " + e.getMessage());
//         } catch (IOException e) {
//             System.out.println("ioexception lol " + e.getMessage());
//         }
//     } 
// }