// ---------------------------------------------------------------------------
// WARNING!  WARNING!  WARNING!  WARNING!  WARNING!  WARNING!  WARNING!  
/*
 * This code is incomplete and buggy.  You should not modify, run, or
 * even look at it.  May cause eyes to burn.  Can induce nausea.
 */
// WARNING!  WARNING!  WARNING!  WARNING!  WARNING!  WARNING!  WARNING!  
// ---------------------------------------------------------------------------












































/*
 * You're still here, for some reason, so take heed.  This is based on
 * an old project of mine to convert MIDI files to another format.  I repurposed
 * it to generate DATA statements for use in a BASIC driver for the background
 * NOTES player on the COCO 1/2.
 * 
 * I found it to be good enough to give me a startingpoint for new music.
 * Probably not good enough for anyone else.
 * 
 * Other, similar projects exist, and are surely better (though would need
 * tweaking to generate this exact format).  For example:
 * https://github.com/nowhereman999/MIDI-2-CoCo-Converter
 * 
 * Modify MIDI_FILE below to point to a TYPE *0* Standard MIDI File, and
 * DATA statements will be printed to stdout that you can paste into
 * BASIC.
 * 
 * - MIDI file must be type 0
 * - There is a trailing comma you must remove.
 * - It will spew errors along the way.  Maybe they're ignorable, maybe not.
 * - Most MIDI files will produce garbage.  You must ensure no more than
 *   4 notes are played at a time.  I have only ever tested with MIDI files
 *   I generated myself from a DAW, not with random MIDIs I found on the web
 * 
 * This is completely unsupported.  If you have issues, I sympathize.
 */

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import javax.sound.midi.InvalidMidiDataException;
import javax.sound.midi.MidiEvent;
import javax.sound.midi.MidiMessage;
import javax.sound.midi.MidiSystem;
import javax.sound.midi.Sequence;
import javax.sound.midi.ShortMessage;
import javax.sound.midi.Track;

public class MidiToAsmSound
{
	private static final String MIDI_FILE = "c:/coco/asm/MusicBox.mid";
	
    private static final int STATUSMASK_NOTEOFF = 0x80;
    private static final int STATUSMASK_NOTEON = 0x90;

    private static final int STATUS_OTHER = 0;
    private static final int STATUS_NOTEOFF = 1;
    private static final int STATUS_NOTEON = 2;
    
    private static ArrayList<Chord> chords = new ArrayList<Chord>();

    // Represents a set of one or more notes, with a single common duration
    private static class Chord
    {
        private ArrayList<Integer> noteNumbers;
        private long tickStart;
        private long tickEndFromNoteOff;
        
        public Chord(int noteNumberP, long tickStartP)
        {
            noteNumbers = new ArrayList<Integer>();
            noteNumbers.add(noteNumberP);
            tickStart = tickStartP;
            tickEndFromNoteOff = -1;
        }
        
        public boolean contains(int noteNumber)
        {
            return noteNumbers.contains(noteNumber);
        }
        
        public void addNoteNumber(int noteNumber)
        {
            if (noteNumbers.size() == 4)
            {
            	System.err.println("ERROR: Chords object about to exceed 4 note limit");
            	return;
            }
            noteNumbers.add(noteNumber);
        }
        
        public void addCurrentlyOnNotes(ArrayList<Integer> currentlyOnNotes)
        {
        	for (int note : currentlyOnNotes)
        	{
        		addNoteNumber(note);
        	}
        }
        
        public String toString()
        {
            return getNoteLetters();
        }
        
        private String getNoteLetters()
        {
            String ret = "";
            for (int noteNumber : noteNumbers)
            {
                ret += getNoteLetter(noteNumber);
            }
            
            return ret;
        }
        
        private String getNoteLetter(int noteNumber)
        {
            int octave = noteNumber / 12 - 1;
            int lowNoteNum = noteNumber % 12;
            String[] numToLetterSharps = {"C ", "C#", "D ", "D#", "E ", "F ", "F#", "G ", "G#", "A ", "A#", "B " };
            String noteLetter = numToLetterSharps[lowNoteNum];
            return noteLetter + octave;
        }    
        
    }

    public static void main(String[] args)
    {
        convertMIDIFile(MIDI_FILE, 80, true);
    }
    
    public static void convertMIDIFile(String filename, int chordDurationThreshold, boolean useSharpsP)
    {
        try
        {
            convertMIDIFileThrowing(filename, chordDurationThreshold, useSharpsP);
        }
        catch(InvalidMidiDataException e)
        {
            System.out.println("ERROR: InvalidMidiDataException: " + e);
        }
        catch(IOException e)
        {
            System.out.println("ERROR: IOException: " + e);
        }
    }
    
    private static void convertMIDIFileThrowing(String filename, int chordDurationThreshold, boolean useSharpsP) throws InvalidMidiDataException, IOException
    {    
        Sequence sequence = MidiSystem.getSequence(new File(filename));
        Track[] tracks = sequence.getTracks();
        ArrayList<Integer> currentlyOnNotes = new ArrayList<Integer>();
        for (Track track : tracks)
        {
            int cEvents = track.size();
            for (int iEvent = 0; iEvent < cEvents; iEvent++)
            {
                MidiEvent event = track.get(iEvent);
                long tick = event.getTick();
                MidiMessage msg = event.getMessage();
                int statusByte = msg.getStatus();
                int status = getMidiMessageStatus(statusByte);
                switch (status)
                {
                case STATUS_NOTEON:
                    processNoteOn((ShortMessage) msg, tick, chordDurationThreshold, currentlyOnNotes);
                    break;

                case STATUS_NOTEOFF:
                    processNoteOff((ShortMessage) msg, tick, currentlyOnNotes);
                    break;

                default:
                    break;
                }
            }
        }
        
        int lineNumber = 4000;
        final int MAX_CHORDS_PER_LINE = 10;
        printLineStart(lineNumber);
        int chordNumber = 0;
        for (Chord nn : chords)
        {
            chordNumber++;
            if (chordNumber > MAX_CHORDS_PER_LINE)
            {
                chordNumber = 1;
                lineNumber += 10;
                printLineStart(lineNumber);
            }
            System.out.print(nn);
            if (chordNumber < MAX_CHORDS_PER_LINE)
            {
                System.out.print(",");
            }
        }
    }
    
    private static void printLineStart(int lineNumber)
    {
        System.out.print("\n" + lineNumber + " DATA ");
    }
    
    private static int getMidiMessageStatus(int statusByte)
    {
        if ((statusByte & 0xF0) == STATUSMASK_NOTEOFF)
        {
            return STATUS_NOTEOFF;
        }
        
        if ((statusByte & 0xF0) == STATUSMASK_NOTEON)
        {
            return STATUS_NOTEON;
        }
        
        return STATUS_OTHER;
    }
    
    private static void processNoteOn(ShortMessage msg, long tick, int chordDurationThreshold, ArrayList<Integer> currentlyOnNotes)
    {
        // Most MIDI files use a Note On with velocity == 0 in lieu of a proper Note off.
        if (msg.getData2() == 0)
        {
            processNoteOff(msg, tick, currentlyOnNotes);
            return;
        }
        
        int noteNumber = msg.getData1();
        
        System.out.println("NOTE ON:");
        System.out.println(tick + ": " + noteNumber + ", " + msg.getData2());
        if (currentlyOnNotes.contains(noteNumber))
        {
            System.err.println("ERROR: adding duplicate note on for " + noteNumber);
            return;
        }
        
        boolean noteAdded = false;
        
        if (!chords.isEmpty())
        {
            Chord lastChord = chords.get(chords.size() - 1); 
            long lastTick = lastChord.tickStart;
            if (tick < lastTick)
            {
                System.err.println("ERROR: adding " + tick + " after " + lastTick);
            }

            // Is this part of the most recent chord?
            if (Math.abs(tick - lastTick) <= chordDurationThreshold)
            {
                lastChord.addNoteNumber(noteNumber);
                noteAdded = true;
            }
        }
        if (!noteAdded)
        {
        	Chord newChord = new Chord(msg.getData1(), tick);
        	newChord.addCurrentlyOnNotes(currentlyOnNotes);
        	chords.add(newChord);
        }
    
        currentlyOnNotes.add(noteNumber);
        if (currentlyOnNotes.size() > 4)
        {
            System.err.println("ERROR: currentlyOnNotes size now " + currentlyOnNotes.size());
            return;        	
        }
    
    }
    
    private static void processNoteOff(ShortMessage msg, long tick, ArrayList<Integer> currentlyOnNotes)
    {
    	int noteNumber = msg.getData1();
        System.out.println("NOTE OFF:");
        System.out.println(tick + ": " + noteNumber + ", " + msg.getData2());
        if (!currentlyOnNotes.contains(noteNumber))
        {
            System.err.println("ERROR: processNoteOff called for note not in currentlyOnNotes");
            return;
        }
        currentlyOnNotes.remove((Object) noteNumber);
        
        // Find most recent matching note number
        for (int i=chords.size() - 1; i >= 0; i--)
        {
            Chord cn = chords.get(i);
            if (cn.contains(msg.getData1()))
            {
                if (cn.tickEndFromNoteOff != -1)
                {
                    // Already set this chord's duration
                    return;
                }
                cn.tickEndFromNoteOff = tick;
                return;
            }
        }
        
        // Still here?!
        System.out.println("ERROR: Couldn't find matching NOTE ON.");
    }
}

