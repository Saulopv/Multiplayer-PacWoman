package dev.pacwomen.gfx;

import dev.pacwomen.tiles.consumables.Consumable;

import static dev.pacwomen.tiles.Tile.TILEHEIGHT;
import static dev.pacwomen.tiles.Tile.TILEWIDTH;
import static dev.pacwomen.tiles.consumables.Consumable.ITEMHEIGHT;
import static dev.pacwomen.tiles.consumables.Consumable.ITEMWIDTH;
import static dev.pacwomen.characters.PacWoman.DEFAULT_HEIGHT;
import static dev.pacwomen.characters.PacWoman.DEFAULT_WIDTH;


public class Util {

    public static int[] centerConsumable(int x, int y){
        return center(x,y,ITEMWIDTH,ITEMHEIGHT);
    }

    public static int[] centerCharacter(int x, int y){
        return center(x,y,DEFAULT_WIDTH,DEFAULT_HEIGHT);
    }

    private static int[] center(int x, int y, int width, int height){
        int xCenter = width/2;
        int yCenter = height/2;
        int tileXCenter = TILEWIDTH/2 + x;
        int tileYCenter = TILEHEIGHT/2 + y;

        while(xCenter + x < tileXCenter){
            x++;
        }
        while(yCenter + y < tileYCenter){
            y++;
        }

        return new int[]{x,y};
    }
}
