package dev.pacwomen.game;


import dev.pacwomen.audio.MusicPlayer;

public class Launcher {
    public static final int DEFAULT_WIDTH = 640, DEFAULT_HEIGHT = 640;
    private static final String TITLE = "Pac-Women";

    public Launcher() {

    }

    public static void main(String[] args){
        if(args.length == 3) {
            Game g = new Game(TITLE, DEFAULT_WIDTH, DEFAULT_HEIGHT, args[0], args[1], args[2]);
            g.start();
        }
        else {
            System.out.println("Bad arguments!\nExpected 3 arguments but got " + args.length);
        }
    }
}


