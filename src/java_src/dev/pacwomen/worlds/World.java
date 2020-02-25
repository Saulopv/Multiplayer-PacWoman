package dev.pacwomen.worlds;

import java.awt.Graphics;

import dev.pacwomen.game.Game;
import dev.pacwomen.tiles.Tile;
import dev.pacwomen.utils.*;
import dev.pacwomen.communication.Mailbox;
import java.util.Arrays;

import com.ericsson.otp.erlang.*;


/**
 * The World class build the world (aka the background) of the game.
 * The width and height of the world is in tiles (a tile is currently 64px)
 */
public class World {

    private Game game;
    private int width, height; //in tiles
    private int[][] tiles;
    private Storage storage = Storage.getInstance();

    /**
     * World constructor. It loads in a text file and loads the world from it.
     * See world.txt for example. 
     * @param tuple, tuple contains map and the dimension of the map.
     * @param mailbox, a mailbox 
     */
    public World(OtpErlangTuple tuple, Mailbox mailbox){
	loadWorld(tuple, mailbox);
    }

    /**
     * Fetches changed tiles through the mailbox and updates their information.
     * @param mailbox, a mailbox communicating with the server.
     */
    public void tick(Mailbox mailbox) {
	int [] tile = mailbox.getChangedTiles();
	if(tile[0] == -1){
	    return;
	}
	for(int i = 0; i < tile.length;i+=2){
	    int x = tile[i];
	    int y = tile[i+1];
	    this.tiles[x][y] = 100;
	}
    }

    /**
     * This renders the world in tiles. One tile has a specific x,y position
     * and a texture. 
     * @param g, the graphics package
     */
    public void render(Graphics g){
	for(int y = 0; y < height;y++){
	    for(int x = 0; x < width;x++){
		
		getTile(x,y).render(g, x * Tile.TILEWIDTH, y * Tile.TILEHEIGHT);
	    }
	}
    }
    /**
     * Indexing tiles array at tiles [x][y] will get a specific tile.
     * @param x, the x position of the tile
     * @param y, the y position of the tile
     * @return Tile, one tile
     */
    public Tile getTile(int x, int y){
	Tile t = Tile.tiles[tiles[x][y]];
	if(t == null)
	    return Tile.backgroundTile;
	return t;
    }

    /**
     * Interprets the txt file to a world. The first four tokens are
     * interpreted as the width and height of the world,
     *  spawnX and spawnY for the player 
     * in that order. The rest of the tokens are interpreted as tiles. 
     * @param path, a path to the txt file for the world
     */
    private void loadWorld(OtpErlangTuple tuple, Mailbox mailbox){

	OtpErlangMap map = (OtpErlangMap) tuple.elementAt(0);
	OtpErlangTuple dimension = (OtpErlangTuple) tuple.elementAt(1);
	OtpErlangLong widthlong = (OtpErlangLong) dimension.elementAt(0);
	OtpErlangLong heightlong = (OtpErlangLong) dimension.elementAt(1);
	int w = 0;
	int h = 0;
	try {
	    w = (int) widthlong.intValue();
	    h = (int) heightlong.intValue();
	}
	catch (OtpErlangRangeException e)
	    {
		System.out.println("it forced me to. But error if you arrive here");
	    }
	this.width = w;
	this.height = h;
	tiles = new int[w][h];
	for(int y = 0; y < h;y++){
	    for(int x = 0; x < w;x++){
		OtpErlangObject[] tup = new OtpErlangObject[2];
	        tup[0] = new OtpErlangLong(x);
		tup[1] = new OtpErlangLong(y);
		OtpErlangTuple tuple2 = new OtpErlangTuple(tup);
		OtpErlangTuple tileInfo = (OtpErlangTuple) map.get((OtpErlangObject) tuple2);
		OtpErlangObject blockType = tileInfo.elementAt(0);
		OtpErlangObject onBlock = tileInfo.elementAt(1);
		if (blockType.equals(new OtpErlangAtom("wall")))
		    {
			tiles[x][y] = 1;
		    }
		else if (blockType.equals(new OtpErlangAtom("tile")))
		    {
			if (onBlock.equals(new OtpErlangAtom("fruit")))
			    {
				tiles[x][y] = 9;
			    }
			else if(onBlock.equals(new OtpErlangAtom("point")))
			    {
				tiles[x][y] = 8;
			    }
			else if(onBlock.equals(new OtpErlangAtom("powerUp")))
			    {
				tiles[x][y] = 100;
			    }
		    }
		else
		    {
			tiles[x][y] = 100;
		    }
	    }
	    
	}
    }

}

/*

 public static Tile pathTile = new Path(0);
    public static Tile wallEdge = new WallEdge(1); // works
    public static Tile wallCenterUp = new WallCenterUp(2);  //works
    public static Tile wallEdgeUp = new WallEdgeUp(3);     //works
    public static Tile wallEdgeDown = new WallEdgeDown(4); //works
    public static Tile wallCenterSide = new WallCenterSide(5);
    public static Tile wallEdgeLeft = new WallEdgeLeft(6); //works
    public static Tile wallEdgeRight = new WallEdgeRight(7);

    //consumables
    public static Tile pacDot = new PacDot(8);
    public static Tile cherry = new Cherry(9);
    public static Tile strawberry = new Strawberry(10);
    public static Tile pear = new Pear(11);
    public static Tile apple = new Apple(12);

    public static Tile backgroundTile = new Background(100);
 */
