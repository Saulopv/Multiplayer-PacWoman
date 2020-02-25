package dev.pacwomen.tiles.consumables;

import dev.pacwomen.gfx.Assets;
import dev.pacwomen.gfx.Util;
import dev.pacwomen.tiles.Tile;

import java.awt.*;
import java.awt.image.BufferedImage;

public class Consumable extends Tile {
    public static final int ITEMWIDTH = 25, ITEMHEIGHT = 25;
    private BufferedImage item;
    private boolean consumed;

    public Consumable(BufferedImage texture, int id) {
        super(Assets.BACKGROUND, id);
        item = texture;
        consumed = false;
    }

    @Override
    public void render(Graphics g, int x, int y) {
        super.render(g, x, y);
        int[] newCoords = Util.centerConsumable(x, y);
        if(!consumed) g.drawImage(item, newCoords[0], newCoords[1], ITEMWIDTH, ITEMHEIGHT, null);
    }


}
