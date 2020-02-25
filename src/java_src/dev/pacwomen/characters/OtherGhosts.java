package dev.pacwomen.characters;

import dev.pacwomen.game.Handler;
import dev.pacwomen.gfx.Assets;
import dev.pacwomen.utils.Tuple;
import dev.pacwomen.communication.Mailbox;
import com.ericsson.otp.erlang.*;
import static dev.pacwomen.tiles.Tile.TILEWIDTH;
import static dev.pacwomen.tiles.Tile.TILEHEIGHT;


import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.Arrays;

public class OtherGhosts extends Character{

    private static int direction;
    private static final int DEFAULT_GHOST_WIDTH = 30, DEFAULT_GHOST_HEIGHT = 30;
    private static final int UP = 1, DOWN = 2, LEFT = 3, RIGHT = 4;
    private BufferedImage[] ghostSprites;
    private int wait;
    private Tuple[] coordinates;

    private int cycle;
    private int frames;
    public OtherGhosts(int xC, int yC, float x, float y, BufferedImage[] ghostSprites) {
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
	    getDirection(mailbox);
    }

    private void getDirection(Mailbox mailbox){
        this.coordinates = mailbox.OtherGhostsReceive();
    }

    @Override
    public void render(Graphics g) {
        renderGhost(g);
    }

    /**
     * Renders a ghost according to current status;
     */
    private void renderGhost(Graphics g){
	if (this.coordinates == null)
	    {
		return;
	    }
	for(int counter = 0;counter < this.coordinates.length;counter++)
            {
                if(coordinates[counter] == null) continue;
                Tuple Position = coordinates[counter];
                OtpErlangLong PosX = (OtpErlangLong) Position.getX();
		OtpErlangLong PosY = (OtpErlangLong) Position.getY();
		int xOldCoord = xCoord;
		int yOldCoord = yCoord;

		try {
		    xCoord = PosX.intValue();
		    yCoord = PosY.intValue();
		}
		catch (OtpErlangRangeException e)
		    {
			System.out.println(e.toString());
			System.exit(-1);
		    }
		if(yCoord == yOldCoord && xCoord == xOldCoord){
		    yMove = 0; // TODO: Temporary lines that can be removed
		    xMove = 0; // once ghosts cannot get stuck
		}
		else if(yCoord < yOldCoord){
		    yMove = -speed;
		    xMove = 0;
		    direction = UP;
		}
		else if(yCoord > yOldCoord){
		    yMove = speed;
		    xMove = 0;
		    direction = DOWN;
		}
		else if(xCoord < xOldCoord){
		    xMove = -speed;
		    yMove = 0;
		    direction = LEFT;
		}
		else if(xCoord > xOldCoord){
		    xMove = speed;
		    yMove = 0;
		    direction = RIGHT;
		}
		
		g.drawImage(chooseGhost(),(int) xCoord * TILEWIDTH , (int) yCoord * TILEHEIGHT,DEFAULT_GHOST_WIDTH,DEFAULT_GHOST_HEIGHT, null);
	    }
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
