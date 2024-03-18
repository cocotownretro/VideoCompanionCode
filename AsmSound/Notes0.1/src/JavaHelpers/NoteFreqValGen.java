import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

// Print to standard out FCC & FDB lines suitable for including into
// NOTES.ASM, to define note frequency values.  Be sure to adjust
// FILENAME to point to your input file of musical note frequencies
// (e.g., defining A4 as 400Hz, etc.)
public class NoteFreqValGen 
{
	private final static String FILENAME = "c:/coco/asm/NoteFrequencies.txt";
	
	public static void main(String[] args) throws FileNotFoundException 
	{
		Scanner in = new Scanner(new File(FILENAME));
		
		while (in.hasNext())
		{
			String note = in.next();
			String freq = in.next();
			double freqD = Double.parseDouble(freq);
			
			if (note.length() == 2)
			{
				note = note.substring(0, 1) + " " + note.substring(1, 2);
			}
			else if (note.length() > 3)
			{
				note = note.substring(0, 3);
			}
			
			System.out.println("                FCC     \"" + note + "\"");
			
			final int hsPerSecond = 5240;
			double fullValDbl = freqD * 256 / hsPerSecond;
			int valMsb = (int) fullValDbl;
			long valLsb = Math.round((fullValDbl - valMsb) * 256);
			System.out.println("                FDB     $" + 
					String.format("%1$02X%2$02X", valMsb, (int)valLsb) +
					"\t\t; " + freq + " * 256 / " + hsPerSecond);
		}		
	}

}
