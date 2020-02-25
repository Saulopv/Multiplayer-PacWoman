package dev.pacwomen.communication;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.List;
import dev.pacwomen.utils.Tripple;
import com.ericsson.otp.erlang.*;
import dev.pacwomen.utils.Tuple;


import static dev.pacwomen.tiles.Tile.TILEHEIGHT;
import static dev.pacwomen.tiles.Tile.TILEWIDTH;

public class Mailbox {
    private String cookie = "bitter";
    private String clientName;
    private String serverName = "pacServer";
    private String serverHost;
    private OtpNode node;
    private OtpMbox mbox;
    private OtpErlangLong xPos,yPos;
    private int GUIx;
    private int lastGUIx;
    private int GUIy;
    private int lastGUIy;
    private int GUIdirection;
    private boolean tileswitch;
    private OtpErlangPid ghostPid;
    private int keypress;
    
    public Mailbox(String ID, String localIP, String serverIP){
	try {
            this.clientName = "client" + ID;
	    this.serverHost = serverName + "@" + serverIP;
	    this.node = new OtpNode(clientName, cookie);
	    this.mbox = this.node.createMbox();
	} catch(Exception OtpErlangExit)
	    {
	        OtpErlangExit.printStackTrace();
		System.exit(-1);
	    }
    }

    /**
     * @return Returns a map from the server and its dimensions.
     */
    public OtpErlangTuple initiateMap(){
        OtpErlangObject[] msg = new OtpErlangObject[2];
        msg[0] = new OtpErlangAtom("getMap");
        msg[1] = this.mbox.self();
        OtpErlangTuple tuple = new OtpErlangTuple(msg);
        this.mbox.send(this.serverName, this.serverHost, tuple);
        try {
            OtpErlangTuple reply = (OtpErlangTuple)this.mbox.receive(); 
            return reply;
        }
        catch(Exception OtpErlangExit)
            {
                System.out.println("Map died: " + OtpErlangExit.getMessage());
                OtpErlangExit.printStackTrace();
                System.exit(-1);
            }

        //Should never get here but java forces me to return something
        OtpErlangObject[] error = new OtpErlangObject[1];
        error[0] = new OtpErlangLong(-1);
        OtpErlangTuple reply = new OtpErlangTuple(error);
        return reply;
    }

    /**
     * Sends a message to the server to initiate the player.
     * The server will also spawn a ghost process.
     * @return The logical coordinates of the player followed by
     * the logical coordinates of the player's associated ghost.
     */
    public int[] initiatePlayer(){
        try {
            OtpErlangObject[] msg = new OtpErlangObject[2];
            msg[0] = new OtpErlangAtom("initiatePlayer");
            msg[1] = this.mbox.self();
            OtpErlangTuple tuple = new OtpErlangTuple(msg);
            this.mbox.send(this.serverName, this.serverHost, tuple);

            OtpErlangTuple reply = (OtpErlangTuple)this.mbox.receive();
            OtpErlangTuple player = (OtpErlangTuple)reply.elementAt(0);
            OtpErlangTuple ghost = (OtpErlangTuple)reply.elementAt(1);
      	    this.ghostPid = (OtpErlangPid)reply.elementAt(2);
            OtpErlangLong value = (OtpErlangLong)reply.elementAt(3);


            OtpErlangLong xCord = (OtpErlangLong)player.elementAt(0);
            OtpErlangLong yCord = (OtpErlangLong)player.elementAt(1);
            OtpErlangLong x = (OtpErlangLong)ghost.elementAt(0);
            OtpErlangLong y = (OtpErlangLong)ghost.elementAt(1);
            this.xPos = xCord;
            this.yPos = yCord;
            this.GUIx = xCord.intValue() * TILEWIDTH  - 15;
            this.GUIy = yCord.intValue() * TILEHEIGHT - 15;
            this.lastGUIx = 0;
            this.lastGUIy = 0;
            this.GUIdirection = 0;
            this.tileswitch = false;


            int[] result = {xCord.intValue(), yCord.intValue(),
                            x.intValue(), y.intValue(), value.intValue()};
            return result;
        } catch(Exception OtpErlangExit)
            {
                System.out.println("Player died: " + OtpErlangExit.getMessage());
                OtpErlangExit.printStackTrace();
                System.exit(-1);
            }

        //Should never get here but java forces me to return something
        int[] result = {-1};
        return result;
    }

    /**
     * Sends a message to the server asking to move in a direction.
     * @param dir, A number 0-4 representing a direction
     */
    public void sendKeyManager(int dir){
	this.keypress = dir;
    }

    /**
     * Sends a message to the server asking to move in a direction
     * and receives on indicating the direction is has been approved to
     * move in.
     * @return A number 0-4 if the player is to stand still/move.
     * 5 if the player has won, -1 if its been eaten.
     */
    public int[] pacWomanReceive(){    
       	OtpErlangObject[] msg = new OtpErlangObject[7];
        msg[0] = new OtpErlangAtom("keypress");
        msg[1] = this.mbox.self();
        OtpErlangObject[] position = new OtpErlangObject[2];
        position[0] = this.xPos;
        position[1] = this.yPos;
        msg[2] = new OtpErlangTuple(position);
        msg[3] = new OtpErlangLong(keypress);
        int xMove;
        int yMove;
        GUIdirection = keypress;
        if (keypress == 0)
            {
                xMove = 0;
                yMove = 0;
            }
        else if(keypress == 1)//UP
            {
                yMove = -5;
                xMove = 0;
            }
        else if(keypress == 2)//DOWN
            {
                yMove = 5;
                xMove = 0;
            }
        else if(keypress == 3)//LEFT
            {
                yMove = 0;
                xMove = -5;
            }
        else//RIGHT
            {
                yMove = 0;
                xMove = 5;
            }
        this.lastGUIx = this.GUIx;
        this.lastGUIy = this.GUIy;
        this.GUIx = GUIx + xMove;
        this.GUIy = GUIy + yMove;
        OtpErlangObject[] pixelposition = new OtpErlangObject[2];
	OtpErlangLong longguix = new OtpErlangLong(this.GUIx);
	OtpErlangLong longguiy = new OtpErlangLong(this.GUIy);
        pixelposition[0] = (OtpErlangObject) longguix;
        pixelposition[1] = (OtpErlangObject) longguiy;
        msg[4] = new OtpErlangTuple(pixelposition);
        int bottomX = 0;
        int bottomY = 0;
        try
            {
                bottomX = xPos.intValue();
                bottomY = yPos.intValue();
            }
        catch(Exception OtpErlangExit)
            {
                OtpErlangExit.printStackTrace();
                System.exit(-1);
            }
        if ((bottomX * 30 - 30 <= GUIx && GUIx <= bottomX * 30) && (bottomY * 30 - 30 <= GUIy && GUIy <= bottomY * 30))
            {
                msg[5] = new OtpErlangAtom(false);
                this.tileswitch = false;
            }
        else
            {
                msg[5] = new OtpErlangAtom(true);
                this.tileswitch = true;
            }
	msg[6] = this.ghostPid;
        OtpErlangTuple tuple = new OtpErlangTuple(msg);
        mbox.send(this.serverName, this.serverHost, tuple);

        try
            {
		OtpErlangTuple reply = (OtpErlangTuple)this.mbox.receive();
		OtpErlangLong dir = (OtpErlangLong) reply.elementAt(0);
		if(dir.intValue() == -1){
		    return new int[] {dir.intValue(), 0};
		}
		else if(!this.tileswitch){	
		    return new int[] {GUIdirection, 0};
		}
                OtpErlangTuple pos = (OtpErlangTuple) reply.elementAt(1);
                this.xPos = (OtpErlangLong) pos.elementAt(0);
                this.yPos = (OtpErlangLong) pos.elementAt(1);
	        OtpErlangLong point =(OtpErlangLong)reply.elementAt(2);
                if(dir.intValue() == 0)
                    {
                        this.GUIx = this.lastGUIx;
                        this.GUIy = this.lastGUIy;
                    }
                return new int[] {dir.intValue(), point.intValue()};
            }
        catch(Exception OtpErlangExit)
            {
                OtpErlangExit.printStackTrace();
                System.exit(-1);
            }

        //Should never get here but java forces me to return something
        return new int[] {-1, 0};
    }
    
    /**
     * Sends a message to the server asking where the ghost chasing it is.
     * @return Coordinates of the ghost.
     */
    public int[] ghostReceive(){
        OtpErlangObject[] msg = new OtpErlangObject[3];
        msg[0] = new OtpErlangAtom("ghostPos");
        msg[1] = this.mbox.self();
	msg[2] = this.ghostPid;
        OtpErlangTuple tuple = new OtpErlangTuple(msg);
        mbox.send(this.serverName, this.serverHost, tuple);

        try {
	    OtpErlangTuple reply = (OtpErlangTuple)this.mbox.receive();
            if(reply.arity() == 0) {
                int[] result = {-1, -1};
                return result;
            }
            OtpErlangLong x = (OtpErlangLong) reply.elementAt(0);
            OtpErlangLong y = (OtpErlangLong) reply.elementAt(1);
            int[] result = {x.intValue(), y.intValue()};
            return result;
        }
        catch(Exception OtpErlangExit)
            {
                OtpErlangExit.printStackTrace();
                System.exit(-1);
            }
        //Should never get here but java forces me to return something
        int[] result = {-1};
        return result;
    }

    public Tripple[] OtherPlayersReceive()
    {
	Tripple tripple[] = new Tripple[30];
        OtpErlangObject[] msg = new OtpErlangObject[3];
        msg[0] = new OtpErlangAtom("otherPlayers");
        msg[1] = this.mbox.self();
        msg[2] = this.mbox.self();
        OtpErlangTuple tuple = new OtpErlangTuple(msg);
        mbox.send(this.serverName, this.serverHost, tuple);

        try {
            OtpErlangMap reply = (OtpErlangMap)this.mbox.receive();
            if(reply == null) return null;
	    if (reply.arity() == 0)
		{
		    return null;
		}
            OtpErlangObject[] list = reply.values();
	    int listLength = list.length;
	    for(int i = 0; i < listLength; ++i)
		{
		    OtpErlangTuple pixelPosDir = (OtpErlangTuple) list[i];
		    tripple[i] = new Tripple(pixelPosDir.elementAt(0),pixelPosDir.elementAt(1),pixelPosDir.elementAt(2));
		}
            return tripple;
        }
        catch(Exception OtpErlangExit)
            {
                OtpErlangExit.printStackTrace();
                System.exit(-1);
            }
        //Should never get here but java forces me to return something
        Tripple[] result = {new Tripple(-1, -1, -1)};
        return result;
    }

    public Tuple[] OtherGhostsReceive()
    {
	Tuple tupple[] = new Tuple[30];
        OtpErlangObject[] msg = new OtpErlangObject[3];
        msg[0] = new OtpErlangAtom("otherGhosts");
        msg[1] = this.mbox.self();
        msg[2] = this.ghostPid;
        OtpErlangTuple tuple = new OtpErlangTuple(msg);
        mbox.send(this.serverName, this.serverHost, tuple);

        try {
            OtpErlangMap reply = (OtpErlangMap)this.mbox.receive();
            if(reply == null) return null;
	    if (reply.arity() == 0)
		{
		    return null;
		}
            OtpErlangObject[] list = reply.values();
	    int listLength = list.length;
	    for(int i = 0; i < listLength; ++i)
		{
		    OtpErlangTuple pos = (OtpErlangTuple) list[i];
		    tupple[i] = new Tuple(pos.elementAt(0),pos.elementAt(1));
		}
            return tupple;
        }
        catch(Exception OtpErlangExit)
            {
                System.out.println(OtpErlangExit.toString());
                System.exit(-1);
            }
        //Should never get here but java forces me to return something
        Tuple[] result = {new Tuple(-1, -1)};
        return result;
    }

	public int[] getChangedTiles(){
	OtpErlangObject[] msg = new OtpErlangObject[2];
        msg[0] = new OtpErlangAtom("changedTiles");
        msg[1] = this.mbox.self();
	OtpErlangTuple tuple = new OtpErlangTuple(msg);
        mbox.send(this.serverName, this.serverHost, tuple);

	try {
            OtpErlangList reply = (OtpErlangList)this.mbox.receive();
	    if(reply == null || reply.arity() == 0){
		int[] result = {-1};
		return result;
	    }
	    int result[] = new int[reply.arity()*2];
	    for(int i = 0;i < result.length;i+=2){
		OtpErlangTuple tup = (OtpErlangTuple) reply.elementAt(i/2);
		OtpErlangLong x = (OtpErlangLong) tup.elementAt(0);
		OtpErlangLong y = (OtpErlangLong) tup.elementAt(1);
		result[i] = x.intValue();
		result[i+1] = y.intValue();
	    }
            return result;
        }
        catch(Exception OtpErlangExit)
            {
                OtpErlangExit.printStackTrace();
                System.exit(-1);
            }
		//Should never get here but java forces me to return something
        int[] result = {-1};
        return result;
    }

    //TODO: Kanske att den här kan tas bort
    public void flush1() {
        try {
            OtpErlangObject reply;
	    do {
		reply = this.mbox.receive(0);
	    }while(reply != null);
        }
        catch(Exception OtpErlangExit)
            {
                OtpErlangExit.printStackTrace();
                System.exit(-1);
            }
    }
}
