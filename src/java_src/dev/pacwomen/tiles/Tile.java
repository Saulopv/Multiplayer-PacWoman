package dev.pacwomen.tiles;


import dev.pacwomen.gfx.Assets;
import dev.pacwomen.tiles.consumables.*;
import dev.pacwomen.tiles.environment.*;


import java.awt.*;

import java.awt.image.BufferedImage;

/**
 * A tile is a square in the game with a fixed width and height. Currently
 * a tile is 30*30 pixels.
 */
public class Tile {

    //STATIC STUFF HERE

    public static Tile[] tiles = new Tile[256];

    //environment
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



    //CLASS


    public static final int TILEWIDTH = 30, TILEHEIGHT = 30;
    protected BufferedImage texture;
    protected final int id;

    /**
     * Every tile has a texture and a id. This makes it easier to load
     * a specific type of tile.
     * WallEdge has id = 0 and Background has id = 1.
     * @param texture, a image of the tile
     * @param id, a number to represent the tile.
     */
    public Tile(BufferedImage texture, int id){
	this.texture = texture;
	this.id = id;
	tiles[id] = this;
    }

    public void tick(){

    }

    /**
     * Renders a tile with a texture, x and y position, and a width and height
     * @param g, the graphics package
     * @param x, the x position
     * @param y, the y position
     */
    public void render(Graphics g, int x, int y){
	g.drawImage(Assets.BACKGROUND, x, y, TILEWIDTH, TILEHEIGHT, null);
        g.drawImage(texture, x, y, TILEWIDTH, TILEHEIGHT, null);
    }

    /**
     * This method is supposed to be used later
     *to make sure the player can't pass through solids.
     * If tile is solid returns true otherwise false
     * @return a boolean
     */
    public boolean isSolid(){
	return false;
    }

    /**
     * returns id of a tile.
     * @return int, the id of a tile
     */
    public int getId(){
	return id;
    }

}
