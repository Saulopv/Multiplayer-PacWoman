package dev.pacwomen.state;
import dev.pacwomen.gfx.Assets;
import dev.pacwomen.communication.Mailbox;
import dev.pacwomen.game.Handler;
import dev.pacwomen.game.Launcher;

import java.awt.*;

public class EndScreen extends State{
    private int points;
    private int width, height;
    private int trumpWidth, trumpHeight, trumpX, trumpY;
    private int topTextX, topTextY;
    private int scoreTextX, scoreTextY;
    private int restartTextX, restartTextY;
    private int quitTextX, quitTextY;
    private int frame, tick;
    private int state;

    public EndScreen(Handler handler, int winWidth, int winHeight, int state, int points){
	super(handler);
        width = winWidth;
        height = winHeight;
	this.status = 0;
	this.state = state;
	this.points = points;
        frame = 0;
        tick = 0;
        calculatePositions(winWidth, winHeight);
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
        scoreTextX = (int)(winWidth * 0.08);
        scoreTextY = (int)(winHeight * 0.85);
        restartTextX = (int)(winWidth * 0.08);
        restartTextY = (int)(winHeight * 0.91);
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
	if(this.state == 5){
	    renderWin(g);
	} else if (this.state == -1){
	    renderLose(g);
	} else {
	    System.out.println("bad status!");
	    System.exit(0);
	}
    }

    private void renderWin(Graphics g){
        Toolkit.getDefaultToolkit().sync();

        g.setColor(Color.black);
        g.fillRect(0, 0, width, height);
	g.drawImage(Assets.TRUMP_IDLE_DOWN[frame], trumpX, trumpY, trumpWidth, trumpHeight, null);

        g.setFont(new Font("TimesRoman", Font.PLAIN, 40)); 
        g.setColor(Color.pink);
        g.drawString("You Won!", topTextX, topTextY);
        g.drawString("Score: " + points, scoreTextX, scoreTextY);
        g.drawString("Press r to restart!", restartTextX, restartTextY);
	g.drawString("Press q to quit!", quitTextX, quitTextY);
    }

    private void renderLose(Graphics g){
        Toolkit.getDefaultToolkit().sync();

        g.setColor(Color.black);
        g.fillRect(0, 0, width, height);
	g.drawImage(Assets.TRUMP_IDLE_UP[frame], trumpX, trumpY, trumpWidth, trumpHeight, null);

        g.setFont(new Font("TimesRoman", Font.PLAIN, 40)); 
        g.setColor(Color.pink);
        g.drawString("You Lost!", topTextX, topTextY);
        g.drawString("Score: " + points, scoreTextX, scoreTextY);
        g.drawString("Press r to restart!", restartTextX, restartTextY);
	g.drawString("Press q to quit!", quitTextX, quitTextY);
    }
}
