package dev.pacwomen.gfx;

import java.awt.image.BufferedImage;

public class SpriteSheet {

    private BufferedImage sheet;

    public SpriteSheet(BufferedImage sheet){
        this.sheet = sheet;
    }

    /**
     * Crops an image according to input arguments.
     * @param x starting position on the x-axis.
     * @param y starting position on the y-axis.
     * @param w width to crop.
     * @param h height to crop.
     * @return a cropped image according to x,y,w and h.
     * @see BufferedImage
     */
    public BufferedImage crop(int x, int y, int w, int h){
        return sheet.getSubimage(x,y,w,h);
    }
}