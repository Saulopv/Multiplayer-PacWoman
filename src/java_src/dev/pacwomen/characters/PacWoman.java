package dev.pacwomen.characters;

import dev.pacwomen.game.Handler;
import dev.pacwomen.gfx.Assets;
import dev.pacwomen.communication.Mailbox;

import java.awt.*;
import java.awt.image.BufferedImage;

public class PacWoman extends Character {
    private Handler handler;

    private int cycle, frames, direction, lastdirection;

    private static final int IDLE = 0, UP = 1, DOWN = 2, LEFT = 3, RIGHT = 4,
	WON = 5, DEAD = -1;
    private int points = 0;

    /**
     * This method is used to add two integers. This is
     * a the simplest form of a class method, just to
     * show the usage of various javadoc Tags.
     * @param handler, the handler
     * @param x position
     * @param y position
     * @param width, of pacwoman
     * @param height, of pacwoman 
   */
    public PacWoman(Handler handler, float x, float y, int width, int height){
        super(x, y, width, height);
        cycle = 0;
        frames = 0;
        this.handler = handler;
        direction = 0;
        lastdirection = 2;
    }

    /**
     * @return Returns true if the player has won.
     */
    public boolean won(){
	return (this.direction == WON);
    }

    /**
     * @return Returns true if the player has been eaten.
     */
    public boolean dead(){
	return (this.direction == DEAD);
    }

    public int getPoints(){
	return this.points;
    }
    
    @Override
    public void tick(Mailbox mailbox) {	
        getInput(mailbox);
        move();
    }
    
    private void getInput(Mailbox mailbox){
        int[] reply = mailbox.pacWomanReceive();
	int movement = reply[0];
	this.points += reply[1];
	if(movement == 5){
	    direction = WON;
	    return;
	}
	if(movement == -1){
	    direction = DEAD;
	    return;
	}
        if(movement == 1)
            {
                yMove = -speed;
                xMove = 0;
                direction = 1;
                lastdirection = UP;
                return;
            }
        else if(movement == 2)
            {
                yMove = speed;
                xMove = 0;
                direction = 2;
                lastdirection = DOWN;
                return;
            }
        else if(movement == 3)
            {
                xMove = -speed;
                yMove = 0;
                direction = 3;
                lastdirection = LEFT;
                return;
            }
        else if(movement == 4)
            {
                xMove = speed;
                yMove = 0;
                direction = 4;
                lastdirection = RIGHT;
                return;
            }
        direction = 0;

    }

    @Override
    public void render(Graphics g) {
        trump(g);
	g.setFont(new Font("TimesRoman", Font.PLAIN, 20)); 
        g.setColor(Color.darkGray);
        g.fillRect(1, 1, 94 + ((int)(this.points / 10) * 12), 24);
        g.setColor(Color.lightGray);
        g.drawString("Points: " + this.points, 5,21);
    }

    /**
     * Renders out trump in a animated manner.
     * @param g Graphics to render out to.
     */
    private void trump(Graphics g){
        g.setColor(Color.blue);
        g.drawRoundRect((int) xPos + width / 4, (int) yPos + height - 20, width / 2, height / 4, width, height);
	g.drawRoundRect((int) xPos + width / 4 + 1, (int) yPos + height - 20 + 1, width / 2 - 2, height / 4 - 2, width, height);
	g.drawRoundRect((int) xPos + width / 4 + 2, (int) yPos + height - 20 + 2, width / 2 - 4, height / 4 - 4, width, height);

        g.drawImage(chooseTrumps(),(int) xPos, (int) yPos,width,height, null);

        frames ++;
        if(frames == 3) {
            cycle ++;
            frames = 0;
        }
        if(cycle == 10)
            cycle = 0;
    }

    /**
     *
     * @return Returns a image depending on the current cycle, directon and last direction;
     */
    private BufferedImage chooseTrumps(){
        switch (direction){
            case IDLE:
                switch (lastdirection) {
                    case UP:
                        return Assets.TRUMP_IDLE_UP[cycle];
                    case DOWN:
                        return Assets.TRUMP_IDLE_DOWN[cycle];
                    case LEFT:
                        return Assets.TRUMP_IDLE_LEFT[cycle];
                    case RIGHT:
                        return Assets.TRUMP_IDLE_RIGHT[cycle];
            }
            case UP:
                return Assets.TRUMP_WALK_UP[cycle];
            case DOWN:
                return Assets.TRUMP_WALK_DOWN[cycle];
            case LEFT:
                return Assets.TRUMP_WALK_LEFT[cycle];
            case RIGHT:
                return Assets.TRUMP_WALK_RIGHT[cycle];
            default:
                return Assets.trump;
        }

    }

}
