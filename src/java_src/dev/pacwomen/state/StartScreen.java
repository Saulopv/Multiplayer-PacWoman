package dev.pacwomen.state;
import dev.pacwomen.gfx.Assets;
import dev.pacwomen.communication.Mailbox;
import dev.pacwomen.game.Handler;
import dev.pacwomen.game.Launcher;

import java.awt.*;

public class StartScreen extends State{
    private int width, height;
    private int trumpWidth, trumpHeight, trumpX, trumpY;
    private int topTextX, topTextY;
    private int startTextX, startTextY;
    private int quitTextX, quitTextY;
    private int frame, tick;

    public StartScreen(Handler handler, int winWidth, int winHeight){
	super(handler);
        width = winWidth;
        height = winHeight;
        frame = 0;
        tick = 0;
        calculatePositions(winWidth, winHeight);
	this.status = 0;
    }

    public boolean startOrEndScreen() {
	return true;
    }

    private void calculatePositions(int winWidth, int winHeight) {
        trumpWidth = winWidth;
        trumpHeight = winHeight;
        trumpX = winWidth / 2 - trumpWidth / 2;
        trumpY = winHeight / 2 - trumpHeight / 2;

        topTextX = (int)(winWidth * 0.38);
        topTextY = (int)(winHeight * 0.1);
        startTextX = (int)(winWidth * 0.08);
        startTextY = (int)(winHeight * 0.91);
	quitTextX = (int)(winWidth * 0.08);
	quitTextY = (int)(winHeight * 0.97);
    }

    public void tick(Mailbox mailbox){
        int numFrames = 10;
        int ticksPerFrame = 2;
        tick ++;
        if(tick % ticksPerFrame == 0) {
            frame = (frame + 1) % numFrames;
            tick = 0;
        }
    }

    public void render(Graphics g){
        Toolkit.getDefaultToolkit().sync();
	
        g.setColor(Color.black);
        g.fillRect(0, 0, width, height);
	g.drawImage(Assets.TRUMP_IDLE_DOWN[frame], trumpX, trumpY, trumpWidth, trumpHeight, null);

        g.setFont(new Font("TimesRoman", Font.PLAIN, 40)); 
        g.setColor(Color.pink);
        g.drawString("Welcome!", topTextX, topTextY);
        g.drawString("Press r to start!", startTextX, startTextY);
	g.drawString("Press q to quit!", quitTextX, quitTextY);
    }
}
