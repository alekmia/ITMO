import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.io.*;

public class Reverse{
    public static void main(String[] args){
        try{
            MyScanner scmain = new MyScanner(System.in);
            List<int[]> matrix = new ArrayList<int[]>();
            int i = 0;
            int j = 0;
            while(scmain.hasNextLine()){
                MyScanner sceach = new MyScanner(scmain.nextLine());
                j = 0;
                int[] temp = new int[1];
                while(sceach.hasNextInt()) {
                    if(j >= temp.length) {
                        temp = Arrays.copyOf(temp, j * 3);
                    }
                    String che = sceach.nextInt();
                    if(!che.isEmpty()) {
                        temp[j] = Integer.parseInt(che);
                    }
                    j++;
                }
                temp = Arrays.copyOf(temp, j);
                matrix.add(temp);  
            } 

            for (int x = matrix.size() - 1; x >= 0; x--) {
                for (int y = matrix.get(x).length - 1; y >= 0; y--) {
                    System.out.print(matrix.get(x)[y] + " "); 
                }
                System.out.println();
            }
            
        } 
        catch (FileNotFoundException e) {
            System.out.println("file not found " + e.getMessage());
        } catch (IOException e) {
            System.out.println("ioexception lol " + e.getMessage());
        } 

    } 
}



/*public class Reverse{
    public static void main(String[] args){
        try{
            MyScanner scmain = new MyScanner(System.in);
            List<int[]> matrix = new ArrayList<int[]>();
            int i = 0;
            int j = 0;
            while(scmain.hasNextLine()){
                MyScanner sceach = new MyScanner(scmain.nextLine());
                j = 0;
                int[] temp = new int[1];
                while(sceach.hasNextInt()) {
                    if(j >= temp.length) {
                        temp = Arrays.copyOf(temp, j * 3);
                    }
                    String che = sceach.next();
                    if(!che.isEmpty()) {
                        temp[j] = Integer.parseInt(che);
                    }
                    j++;
                    
                }
                temp = Arrays.copyOf(temp, j);
                matrix.add(temp);  
            } 

            for (int x = matrix.size() - 1; x >= 0; x--) {
                for (int y = matrix.get(x).length - 2; y >= 0; y--) {
                    System.out.print(matrix.get(x)[y] + " "); 
                }
                System.out.println();
            }
            
        } 
        catch (FileNotFoundException e) {
            System.out.println("file not found " + e.getMessage());
        } catch (IOException e) {
            System.out.println("ioexception lol " + e.getMessage());
        } 

    } 
}*/