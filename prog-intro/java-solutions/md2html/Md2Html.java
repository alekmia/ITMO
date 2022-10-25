package md2html;

import java.util.*;
import java.io.*;

public class Md2Html {

    static int arrSize = 8;
    static int[] indexs = new int[arrSize];
    static int[] lengths = {1, 1, 2, 2, 2, 1, 0, 0, 3};
    static int headerNumber = 0;
    static boolean inHeader = false;
    static boolean isSlashN = false;
    static boolean flag = false;
    static boolean lastWasClear = true;
    static int inSB = 0;
    static int inLine;
    static String line;
    static boolean emphasisOrStrong = false;
    static boolean noFormatting = false;
    static StringBuilder output = new StringBuilder();

    static void doHtml2(int type, String before, String after) {
        int offset = before.length() - lengths[type];
        output.replace(indexs[type], indexs[type] + lengths[type], before);
        for (int y = 0; y < 9; y++ ) {
            if (indexs[y] != -1 && indexs[y] > indexs[type]) {
                indexs[y] += offset;
            }
        }
        inSB += offset;
        output.replace(inSB - lengths[type] + 1, inSB + 1, after);
        inSB += offset + 1;
        indexs[type] = -1;
    }

    private static void setEmphasisOrStrong(final char c, final int smallPar, final int bigPar) {
        if (!noFormatting && line.charAt(inLine) == c) {
            if (inLine < line.length() - 1 && line.charAt(inLine + 1) == c) {
                inLine++;
                output.append(line.charAt(inLine));
                inSB++;
                if (indexs[bigPar] == -1) {
                    indexs[bigPar] = inSB - 1;
                } else {
                    if (c == '*') {
                        doStrong1();
                    } else {
                        doStrong2();
                    }
                }
            } else {
                if (indexs[smallPar] == -1) {
                    indexs[smallPar] = inSB;
                } else {
                    if (c == '*') {
                        doEmphasis1();
                    } else {
                        doEmphasis2();
                    }
                }
            }
        }
    }

    static void doPreOrCode() {
        if (line.charAt(inLine) == '`') {
            if (inLine < line.length() - 2 && line.charAt(inLine + 1) == '`' && line.charAt(inLine + 2) == '`') {
                inLine++;
                output.append(line.charAt(inLine));
                inLine++;
                output.append(line.charAt(inLine));
                inSB += 2;
                if (indexs[8] == -1) {
                    indexs[8] = inSB - 2;
                    noFormatting = true;
                } else {
                    doPre();
                    noFormatting = false;
                }
            } else if (!noFormatting) {

                if (indexs[5] == -1) {
                    indexs[5] = inSB;
                } else {
                    doCode();
                }
            }
        }
    }

    static void doPre() {
        doHtml2(8, "<pre>", "</pre>");
    }

    static void doCode() {
        doHtml2(5, "<code>", "</code>");
    }

    static void doStrike() {
        if (!noFormatting && line.charAt(inLine) == '-') {
            if (inLine > 0) {
                if (line.charAt(inLine - 1) == '-') {
                    if (indexs[4] == -1) {
                        indexs[4] = inSB - 1;
                    } else {
                        doHtml2(4, "<s>", "</s>");
                    }
                }
            }
        }
    }

    static void markParagraph() {
//      doHtml2(6, "<p>", "</p>");
        output.insert(indexs[6], "<p>");
        for (int y = 0; y < 9; y++) {
            if (indexs[y] != -1 && indexs[y] > indexs[6]) {
                indexs[y] += 3;
            }
        }
        inSB += 3;
        output.insert(inSB - 1, "</p>");
        inSB += 4;
    }

    static void doHeader() {
        output.delete(indexs[7] - 1, indexs[7] + headerNumber);
        output.insert(indexs[7] - 1, "<h" + headerNumber + ">");
        for (int y = 0; y < 9; y++) {
            if (indexs[y] > indexs[7]) {
                indexs[y] += 3 - headerNumber;
            }
        }

        inSB += 3 - headerNumber;
        output.insert(inSB - 1, "</h" + headerNumber + ">");
        inSB += 5;
        indexs[7] = inSB;
        headerNumber = 0;
    }

    static void doStrong1() {
        doHtml2(2, "<strong>", "</strong>");
    }

    static void doEmphasis1() {
        doHtml2(0, "<em>", "</em>");
    }

    static void doStrong2() {
        doHtml2(3, "<strong>", "</strong>");
    }

    static void doEmphasis2() {
        doHtml2(1, "<em>", "</em>");
    }

    static void specialSymbol(String s, int len) {
        output.deleteCharAt(inSB);
        output.append(s);
        inSB += (len - 1);
    }

    static void forSpecSymbols() {
        switch (line.charAt(inLine)) {
            case '<':
                specialSymbol("&lt;", 4);
                break;
            case '>':
                specialSymbol("&gt;", 4);
                break;
            case '&':
                specialSymbol("&amp;", 5);
                break;
            case '\\':
                output.deleteCharAt(inSB);
                if (inLine < line.length() - 1) {
                    output.append(line.charAt(inLine + 1));
                }
                inLine++;
                break;
        }
    }

    private static void getHeaderLevel() {
        if (!noFormatting && headerNumber == 0 && line.charAt(0) == '#') {
            indexs[7] = inSB + 1;
            while (line.charAt(inLine) == '#') {
                inLine++;
                headerNumber++;
                output.append(line.charAt(inLine));
                inSB++;
            }
            int temp = headerNumber;
            headerNumber = 0;
            if (inLine < line.length() - 1) {
                if (line.charAt(inLine) == ' ') {
                    headerNumber = temp;
                }
            }
        }
    }

    public static void main(String[] args) {
        try {
            BufferedReader in = new BufferedReader(
                    new InputStreamReader(
                            new FileInputStream(args[0]), "utf8"
                    )
            );
            arrSize = 9;
            indexs = new int[arrSize];
            headerNumber = 0;
            inHeader = false;
            isSlashN = false;
            flag = false;
            inSB = 0;
            emphasisOrStrong = false;
            output = new StringBuilder();
            for (int i = 0; i < arrSize; i++) {
                indexs[i] = -1;
            }
            indexs[6] = 0;
            lastWasClear = true;
            //header
            //0 - emphasis * length = 1
            //1 - emphasis _ length = 1
            //2 - strong emphasis ** length = 2
            //3 - strong emphasis __ length = 2
            //4 - strikeout -- length = 2
            //5 - code ` length = 1
            //6 - paragraph \n
            //7 - header # length = xz
            //8 - Pre ``` length = 3

            while (true) {
                line = in.readLine();
                if (line == null) {
                    break;
                }

                // IF ENCOUNTERED A EMPTY LINE
                if (line.length() == 0) {
                    for (int i = 0; i < 6; i++) {
                        indexs[i] = -1;
                    }
                    if (lastWasClear) {
                        continue;
                    }
                    if (headerNumber == 0) {
                        markParagraph();
                    } else {
                        doHeader();
                    }
                    indexs[6] = inSB;
                    lastWasClear = true;
                    continue;
                }

                lastWasClear = false;

                for (inLine = 0; inLine < line.length(); inLine++) {
                    output.append(line.charAt(inLine));
                    getHeaderLevel();
                    doStrike();
                    setEmphasisOrStrong('*', 0, 2);
                    setEmphasisOrStrong('_', 1, 3);
                    doPreOrCode();
                    forSpecSymbols();
                    inSB++;
                }
                output.append("\n");
                inSB++;
            }
            if (headerNumber != 0) {
                doHeader();
            } else {
                markParagraph();
            }
            in.close();
            BufferedWriter out = new BufferedWriter(
                    new OutputStreamWriter(
                            new FileOutputStream(args[1]), "utf8"
                    )
            );
            try {
                out.write(output.toString());
            } catch (IOException e) {
                System.out.println("ioexception lol " + e.getMessage());
            } finally {
                out.close();
            }
        } catch (FileNotFoundException e) {
            System.out.println("file not found " + e.getMessage());
        } catch (IOException e) {
            System.out.println("ioexception lol " + e.getMessage());
        }
    }
}