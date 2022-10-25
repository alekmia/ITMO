import java.io.*;
import java.util.*;

class IntList {
	int[] arrayOfInts = new int[10];
	int count = 0;
	int arrSize = 0;

	public IntList() throws IOException {
    }

	public void addInt(int a) {
		if (count == arrayOfInts.length)
		{	
			arrayOfInts = Arrays.copyOf(arrayOfInts, (count * 3) / 2  + 1);
		}
		arrayOfInts[count] = a;
		count++;
		arrSize++;
	}

	public int size() {
		return arrSize;
	}
	
	public int getCount(){
		return count;
	}

	public int getInt(int n){
		return arrayOfInts[n];
	}

	public void set(int index, int a) {
		arrayOfInts[index] = a;
		return;
	}
}