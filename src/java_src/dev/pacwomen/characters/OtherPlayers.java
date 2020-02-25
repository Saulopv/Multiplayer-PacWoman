package dev.pacwomen.characters;

import dev.pacwomen.game.Handler;
import dev.pacwomen.gfx.Assets;
import dev.pacwomen.utils.Tripple;
import dev.pacwomen.communication.Mailbox;
import com.ericsson.otp.erlang.*;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.Arrays;

public class OtherPlayers extends Character{

    private Tripple[] players;
    private int cycle, frames, direction, lastdirection;
    private static final int IDLE = 0, UP = 1, DOWN = 2, LEFT = 3, RIGHT = 4;

    public OtherPlayers(float x, float y, int width, int height){
	super(x, y, width, height);
	this.players = null;
	cycle = 0;
        frames = 0;
        direction = 0;
        lastdirection = 2;
    }

    @Override
    public void tick(Mailbox mailbox) {
	getPlayers(mailbox);
    }

    public void render(Graphics g) {
        trump(g);
    }

    /**
     * Asks the mailbox for the positions of all other players.
     * Receives it in the form of a list of Tupples with logical 
     * coordinate, pixel position, and direction.
     * @param mailbox the mailbox to ask.
     */
    public void getPlayers(Mailbox mailbox){
	this.players = mailbox.OtherPlayersReceive();
    }
    
    private void trump(Graphics g){	
	if (this.players == null)
	    {
		return;
	    }
	for(int counter = 0;counter < this.players.length;counter++)
            {
                if(players[counter] == null) continue;
                Tripple Position = players[counter];
                OtpErlangTuple pixelPos = (OtpErlangTuple) Position.getY();
		OtpErlangLong dir = (OtpErlangLong) Position.getZ();
		OtpErlangLong xCord = (OtpErlangLong)pixelPos.elementAt(0);
		OtpErlangLong yCord = (OtpErlangLong)pixelPos.elementAt(1);
		int xPos = 0;
		int yPos = 0;
		int direct = 0;
		try {
		    xPos = xCord.intValue();
		    yPos = yCord.intValue();
		    direct = dir.intValue();
		    this.lastdirection = direction;
		    this.direction = direct;
		}
		catch (OtpErlangRangeException e)
		    {
			System.out.println(e.toString());
			System.exit(-1);
		    }
		
                g.drawImage(chooseTrumps(),(int) xPos, (int) yPos,width,height, null);
                frames ++;
                if(frames == 3) {
                    cycle ++;
                    frames = 0;
                }
                if(cycle == 10)
                    cycle = 0;
            }
    }
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
