package dev.pacwomen.audio;



import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Clip;
import java.io.File;

public class MusicPlayer {
    public static final String MIITHEME = "/sounds/miitheme.wav";
    public static final String PURPLEDISCO = "/sounds/purpledisco.wav";
    private Clip clip;
    public MusicPlayer(String path){
        try {
            File f = new File(MusicPlayer.class.getResource(path).getFile());
            AudioInputStream audioInput = AudioSystem.getAudioInputStream(f);
            clip = AudioSystem.getClip();
            clip.open(audioInput);
        }
        catch (Exception e){
            e.printStackTrace();
        }

    }
    public void play(){
        clip.start();
        clip.loop(clip.LOOP_CONTINUOUSLY);
    }
    public void pause(){
        clip.stop();
    }
    public static void main(String[] args){
    }



}
