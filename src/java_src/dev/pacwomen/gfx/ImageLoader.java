package dev.pacwomen.gfx;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class ImageLoader {

    /**
     * Loads an image.
     * @param path path to load.
     * @return Image found at the path.
     */
    public static BufferedImage loadImage(String path){
        try {
            return ImageIO.read(ImageLoader.class.getResource(path));


        } catch (IOException e){
            e.printStackTrace();
            System.exit(1);
        }
        return null;
    }


}
