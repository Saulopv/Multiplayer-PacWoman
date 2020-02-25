package dev.pacwomen.tiles.environment;


import dev.pacwomen.gfx.Assets;
import dev.pacwomen.tiles.Tile;

import java.awt.*;
import java.awt.image.BufferedImage;

public class WallEdge extends Tile {
    public WallEdge(int id){
	super(Assets.WALL_EDGE, id);
    }

    @Override
    public boolean isSolid(){
	return true;
    }
}

