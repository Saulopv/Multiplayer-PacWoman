package dev.pacwomen.characters;

import dev.pacwomen.gfx.Assets;

import java.awt.*;
import java.awt.image.BufferedImage;
import dev.pacwomen.communication.Mailbox;
import static dev.pacwomen.tiles.Tile.TILEWIDTH;

public class Ghost extends Character {
    private static int direction;
    private static final int DEFAULT_GHOST_WIDTH = 30, DEFAULT_GHOST_HEIGHT = 30;
    private static final int UP = 1, DOWN = 2, LEFT = 3, RIGHT = 4;
    private BufferedImage[] ghostSprites;
    private int wait;

    private int cycle;
    private int frames;
    public Ghost(int xC, int yC, float x, float y, BufferedImage[] ghostSprites) {
        super(x, y);
        direction = 0;
        speed = 3;
	xCoord = xC;
	yCoord = yC;
	this.ghostSprites = ghostSprites;
	this.wait = calculateWait();
        cycle = 0;
        frames = 0;
    }

    @Override
    public void tick(Mailbox mailbox){
	if (this.wait == 0) {
	    getDirection(mailbox);
	    this.wait = calculateWait();
	}
	this.xPos += xMove;
	this.yPos += yMove;
	this.wait--;
    }

    private void getDirection(Mailbox mailbox){
	int[] cordinates = mailbox.ghostReceive();
	int xOldCoord = xCoord;
	int yOldCoord = yCoord;
	xCoord = cordinates[0];
	yCoord = cordinates[1];
        if(yCoord == yOldCoord && xCoord == xOldCoord){
	    yMove = 0; // TODO: Temporary lines that can be removed
	    xMove = 0; // once ghosts cannot get stuck
	    return;
	}
        else if(yCoord < yOldCoord){
            yMove = -speed;
            xMove = 0;
            direction = UP;
	    return;
        }
        else if(yCoord > yOldCoord){
            yMove = speed;
            xMove = 0;
            direction = DOWN;
            return;
        }
        else if(xCoord < xOldCoord){
            xMove = -speed;
            yMove = 0;
            direction = LEFT;
            return;
        }
        else if(xCoord > xOldCoord){
            xMove = speed;
            yMove = 0;
            direction = RIGHT;
            return;
        }
	return;
    }

    @Override
    public void render(Graphics g) {
        renderGhost(g);
    }

    /**
     * Renders a ghost according to current status;
     */
    private void renderGhost(Graphics g){
        g.drawImage(chooseGhost(),(int) xPos, (int) yPos,DEFAULT_GHOST_WIDTH,DEFAULT_GHOST_HEIGHT, null);
    }

    /**
     * @return Returns a ghost looking at an appropriate direction.
     */
    private BufferedImage chooseGhost(){
        cycle();
        switch (direction){
            case UP:
                return this.ghostSprites[0 + cycle];
            case DOWN:
                return this.ghostSprites[2 + cycle];
            case LEFT:
                return this.ghostSprites[4 + cycle];
            case RIGHT:
                return this.ghostSprites[6 + cycle];
            default:
                return null;
        }
    }
    private void cycle(){
        frames++;
        if(frames == 3) {
            frames = 0;
            if (cycle == 1) {
                cycle = 0;
            } else {
                cycle++;
            }
        }
    }

    /**
     * @returns Returns how many ticks it takes for a ghost to
     * travel a whole tile.
     * TILEWIDTH and speed should be divisible.
     * Assumes TILEWIDTH and TILEHEIGHT are the same.
     */
    private int calculateWait(){
	return (TILEWIDTH / speed);
    }
}
