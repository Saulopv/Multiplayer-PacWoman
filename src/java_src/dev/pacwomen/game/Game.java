package dev.pacwomen.game;

import java.awt.*;
import java.awt.image.BufferStrategy;

import dev.pacwomen.audio.MusicPlayer;
import dev.pacwomen.display.Display;
import dev.pacwomen.gfx.Assets;
import dev.pacwomen.input.KeyManager;
import dev.pacwomen.state.*;
import dev.pacwomen.communication.Mailbox;
import static dev.pacwomen.tiles.Tile.TILEHEIGHT;
import static dev.pacwomen.tiles.Tile.TILEWIDTH;
import com.ericsson.otp.erlang.*;

public class Game implements Runnable {

    private Display display;
    private int width,height; //in tiles
    private String title;

    private boolean running;
    private Thread thread;

    private BufferStrategy bs;
    private Graphics g;
    private KeyManager keyManager;

    private State state;
    private Handler handler;

    private MusicPlayer titleMusic;
    private MusicPlayer gameMusic;

    public Mailbox mailbox;

    public Game(String title, int width, int height, String ID, String localIP, String serverIP){
        this.width = width;
        this.height = height;
        this.title = title;
        this.keyManager = new KeyManager();
        this.handler = new Handler(this);
        this.mailbox = new Mailbox(ID, localIP, serverIP);
        this.titleMusic = new MusicPlayer(MusicPlayer.MIITHEME);
        this.gameMusic = new MusicPlayer(MusicPlayer.PURPLEDISCO);
    }
    /**
     * Initializes the game by setting up the display and the assets.
     */
    private void init(){
	OtpErlangTuple tuple = (OtpErlangTuple) mailbox.initiateMap();
	OtpErlangTuple dimension = (OtpErlangTuple) tuple.elementAt(1);
	OtpErlangLong widthValue = (OtpErlangLong) dimension.elementAt(0);
	OtpErlangLong heightValue = (OtpErlangLong) dimension.elementAt(1);
	this.width = 0;
	this.height = 0;
	try {
	    this.width = (int) widthValue.intValue();
	    this.height = (int) heightValue.intValue();
	}
	catch (OtpErlangRangeException e)
	    {
		System.out.println("it forced me to. But error if you arrive here");
	    }
        display = new Display(title,width*TILEWIDTH,height*TILEHEIGHT);
        display.addKeyListener(keyManager);
        Assets.init();
	this.state = new StartScreen(handler, width * TILEWIDTH, height * TILEHEIGHT);
	titleMusic.play();
    }
    /**
     * Updates the current gamestate and receives current input.
     */
    private void tick(){

	state.tick(this.mailbox);
	if(startOrEndScreen()){
	    changeStateOnKeypress();
	}else{
	    keyManager.tick(this.mailbox);
	}
    }
    
    private boolean startOrEndScreen(){
	if(this.state.status == 5 || this.state.status == -1 ||
	   this.state.status == 0){
	    return true;
	} else return false;
    }
    
    private void changeStateOnKeypress(){
	int keypress = keyManager.tick();
	if(keypress == 1){
            titleMusic.pause();
            gameMusic.play();
	    this.state = new GameState(handler, this.mailbox);
	} else if(keypress == -1){
	    System.exit(0);
	}
    }
	
    /**
     * Renders out the game according to current status.
     */
    private void render(){
        bs = display.getCanvas().getBufferStrategy();
        if(bs == null){
            display.getCanvas().createBufferStrategy(3);
            return;
        }
        g = bs.getDrawGraphics();
        state.render(g);
        bs.show();
        g.dispose();
    }
    /**
     * Main loop of the game, updates and then renders at a maximum of 60 frames per second.
     */
    public void run(){
        init();
        int fps = 30;
        double timePerTick = 1000000000 / fps;
        double delta = 0;
        long now;
        long lastTime = System.nanoTime();
        long timer = 0;
        int ticks = 0;

        while(running){
            now = System.nanoTime();
            delta += (now - lastTime) / timePerTick;
            timer += now - lastTime;
            lastTime = now;
            if(delta >= 1){
                tick();
		if(this.state.status == 5 || this.state.status == -1){
		    gameMusic.pause();
		    titleMusic.play();
		    this.state = new EndScreen(this.handler, width * TILEWIDTH, height * TILEHEIGHT, this.state.status, this.state.getPoints());
		}
		render();
                ticks++;
                delta--;
	    }

	    if(timer >= 1000000000){
                ticks = 0;
                timer = 0;
            }
        }
        stop();
    }

    /**
     * Starts the thread up.
     */
    synchronized void start() {
        if(running) return;
        running = true;
        thread = new Thread(this);
        thread.start();
    }

    /**
     * Stops the game.
     */
    private synchronized void stop(){
        if(!running)
            return;

        try {
            thread.join();
        } catch (InterruptedException e){
            e.printStackTrace();
        }

    }

    /**
     * @return The keymanager.
     */
    public KeyManager getKeyManager() {
        return keyManager;
    }
}
