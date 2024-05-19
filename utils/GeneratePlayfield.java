import java.io.*;
import java.util.*;

public class GeneratePlayfield {

    private static class PlayfieldData {

        private ArrayList<Character> pfd = new ArrayList<Character>();

        private PlayfieldData(String pfdata) {
            for (int i=0; i<20; i++) {
                pfd.add(pfdata.charAt(i));
            }
        }

        private void shiftRight() {
            Character c = pfd.remove(0);
            pfd.add(c);
        }

        private void dumpPf0() {
            StringBuffer sb = new StringBuffer();
            sb.append("        .byte #%");
            for (int i=3; i>=0; i--) {
                sb.append(pfd.get(i));
            }
            sb.append("0000");
            System.out.println(sb.toString());
        }

        private void dumpPf1() {
            StringBuffer sb = new StringBuffer();
            sb.append("        .byte #%");
            for (int i=4; i<=11; i++) {
                sb.append(pfd.get(i));
            }
            System.out.println(sb.toString());
        }

        private void dumpPf2() {
            StringBuffer sb = new StringBuffer();
            sb.append("        .byte #%");
            for (int i=19; i>=12; i--) {
                sb.append(pfd.get(i));
            }
            System.out.println(sb.toString());
        }
    }

    public static void main(String args[]) {

        ArrayList<PlayfieldData> pfdata = new ArrayList<PlayfieldData>();

        try {

            // read each line into an array.
            BufferedReader br = new BufferedReader(
                new FileReader(args[0]));
            String nextLine;
            while ((nextLine = br.readLine()) != null)   {
                if (nextLine.length() == 20) {
                    PlayfieldData pd = new PlayfieldData(nextLine);
                    pfdata.add(pd);
                }
            }

            System.out.println("PFData0");
            for (int i=0; i<20; i++) {
                for (PlayfieldData p : pfdata) {
                    p.dumpPf0();
                    p.shiftRight();
                }
            }
            System.out.println("PFData1");
            for (int i=0; i<20; i++) {
                for (PlayfieldData p : pfdata) {
                    p.dumpPf1();
                    p.shiftRight();
                }
            }
            System.out.println("PFData2");
            for (int i=0; i<20; i++) {
                for (PlayfieldData p : pfdata) {
                    p.dumpPf2();
                    p.shiftRight();
                }
            }
        }
        catch (Throwable e) {
            e.printStackTrace();
        }
    }
}




