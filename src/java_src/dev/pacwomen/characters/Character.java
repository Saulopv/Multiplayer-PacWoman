package dev.pacwomen.characters;

import java.awt.*;
import dev.pacwomen.communication.Mailbox;

public abstract class Character {
    protected static final int DEFAULT_SPEED = 5;
    public static final int DEFAULT_WIDTH = 30,DEFAULT_HEIGHT = 30;

    protected float xPos, yPos;
    protected int xCoord, yCoord;
    protected int width, height;
    protected int xMove, yMove;
    protected int speed;

    public Character(float x, float y) {
        this.width = DEFAULT_WIDTH;
        this.height = DEFAULT_HEIGHT;
        xPos = x;
        yPos = y;
        speed = DEFAULT_SPEED;
    }

    public Character(float x, float y, int width, int height) {
        this.width = width;
        this.height = height;
        xPos = x;
        yPos = y;
        speed = DEFAULT_SPEED;
    }

     /**
      * This is the main method which makes use of addNum method.
      * Changes xPos by xMove and yPos by yMove;
      */
    protected void move(){
        xPos += xMove;
        xMove = 0;
        yPos += yMove;
        yMove = 0;
    }

    public int getyCoord() {
        return yCoord;
    }

    public int getxCoord() {
        return xCoord;
    }

    /**
     * @return yPos, the current y-position of the character;
     */
    public float getyPos() {
        return yPos;
    }
    /**
     * @return xPos, the current x-position of the character;
     */
    public float getxPos() {
        return xPos;
    }

    /**
     * Updates the characters status according to specific implementation.
     * @param mailbox, the mailbox connected to the character
     */
    public abstract void tick(Mailbox mailbox);
    /**
     * Renders out the character according to current status.
     * @param g Graphics to render to.
     */
    public abstract void render(Graphics g);
}
