package dev.pacwomen.gfx;

import java.awt.image.BufferedImage;

public class Assets {


    public static BufferedImage PACDOT,CHERRY,STRAWBERRY,PEAR,APPLE;

    public static BufferedImage BACKGROUND;
    public static BufferedImage WALL_EDGE, WALL_EDGE_UP, WALL_EDGE_DOWN, WALL_EDGE_LEFT, WALL_EDGE_RIGHT;
    public static BufferedImage WALL_CENTER_UP, WALL_CENTER_SIDE;


    public static BufferedImage FIVE_THOUSAND, THREE_THOUSAND, TWO_THOUSAND,ONE_THOUSAND;
    public static BufferedImage SEVEN_HUNDRED,FIVE_HUNDRED,THREE_HUNDRED,ONE_HUNDRED;
    public static BufferedImage GAME_OVER, READY;


    public static BufferedImage trump;
    public static BufferedImage[] TRUMP_IDLE_DOWN  = new BufferedImage[10];
    public static BufferedImage[] TRUMP_IDLE_LEFT  = new BufferedImage[10];
    public static BufferedImage[] TRUMP_IDLE_RIGHT = new BufferedImage[10];
    public static BufferedImage[] TRUMP_IDLE_UP    = new BufferedImage[10];
    public static BufferedImage[] TRUMP_WALK_DOWN  = new BufferedImage[10];
    public static BufferedImage[] TRUMP_WALK_LEFT  = new BufferedImage[10];
    public static BufferedImage[] TRUMP_WALK_RIGHT = new BufferedImage[10];
    public static BufferedImage[] TRUMP_WALK_UP = new BufferedImage[10];

    public static BufferedImage[][] TRUMP_IDLE = {TRUMP_IDLE_DOWN,TRUMP_IDLE_RIGHT,TRUMP_IDLE_UP,TRUMP_IDLE_LEFT};
    public static BufferedImage[][] TRUMP_WALK = {TRUMP_WALK_DOWN,TRUMP_WALK_RIGHT,TRUMP_WALK_UP,TRUMP_WALK_LEFT};


    public static BufferedImage[] RED_GHOST = new BufferedImage[8];
    public static BufferedImage[] PINK_GHOST = new BufferedImage[8];
    public static BufferedImage[] CYAN_GHOST = new BufferedImage[8];
    public static BufferedImage[] YELLOW_GHOST = new BufferedImage[8];
    public static BufferedImage[] FRAGILE_GHOST = new BufferedImage[1];

    public static BufferedImage[][] GHOSTS = {RED_GHOST, PINK_GHOST,CYAN_GHOST,YELLOW_GHOST};



    public static BufferedImage AMERICAN_FLAG;
    public static final int width = 256, height = 256;

    /**
     * Initializes all assets by loading and cropping images.
     */
    public static void init(){

        SpriteSheet idleTrumps = new SpriteSheet(ImageLoader.loadImage("/textures/trump_iddle.png"));
        SpriteSheet sheet2 = new SpriteSheet(ImageLoader.loadImage("/textures/RGB_Color_Sample.jpg"));
        SpriteSheet walkingTrump = new SpriteSheet(ImageLoader.loadImage("/textures/trump_walk.png"));
        SpriteSheet elements = new SpriteSheet(ImageLoader.loadImage("/textures/pacman.png"));
        SpriteSheet walls = new SpriteSheet(ImageLoader.loadImage("/textures/pacmantiles.png"));

        //Main character
        trump = idleTrumps.crop(0,0,width,height);
        cropTrumps(idleTrumps,TRUMP_IDLE);
        cropTrumps(walkingTrump,TRUMP_WALK);

        //Ghosts
        cropGhosts(elements);

        //Tiles
	//walL = sheet2.crop(0,172,173,173);
	BACKGROUND = sheet2.crop(175,175,170,170);
	cropWall(walls);

	//Consumables
	cropPoints(elements);
	cropConsumables(elements);

	//Visuals
        cropVisuals(elements);


    }

    /**
     * Crops any sheets with the same or larger dimensions but is specified for /textures/trump_iddle.png and /textures/trump_run.png
     * Initializes all trump images
     * @param sheet Sheet to crop.
     * @param trumps Two dimensional array where the cropped images are put.
     */
    private static void cropTrumps(SpriteSheet sheet, BufferedImage[][] trumps){
        for(int i = 0; i < 4; i++) {
            for (int j = 0; j < 10; j++) {
                trumps[i][j] = sheet.crop(256 * j, 256*i, width, height);
            }
        }
    }
    /**
     * Crops any sheets with the same or larger dimensions but is specified for /textures/pacman.png
     * Initializes all ghost images
     * @param sheet Sheet to crop.
     */
    private static void cropGhosts(SpriteSheet sheet){
        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 8; j++) {
                GHOSTS[i][j] = sheet.crop(3 + j * 20, 83 + (i * 20), 14, 14);
            }
        }


    }
    /**
     * Crops any sheets with the same or larger dimensions but is specified for /textures/pacman.png
     * Initializes all consumables
     * @param sheet Sheet to crop.
     */
    private static void cropConsumables(SpriteSheet sheet)   {
        CHERRY = sheet.crop(172,164,12,12);
        STRAWBERRY = sheet.crop(173,184,11,12);
        PEAR = sheet.crop(173, 204,11,12);
        APPLE = sheet.crop(172,224,12,12);
        PACDOT = sheet.crop(2,182,8,8);
    }
    /**
     * Crops any sheets with the same or larger dimensions but is specified for /textures/pacman.png
     * Initializes all consumable images
     * @param sheet Sheet to crop.
     */
    private static void cropPoints(SpriteSheet sheet){
        ONE_HUNDRED = sheet.crop(172,6,13,7);
        THREE_HUNDRED = sheet.crop(170, 26,15,7);
        FIVE_HUNDRED = sheet.crop(170, 46,15,7);
        SEVEN_HUNDRED = sheet.crop(170, 66,15,7);
        ONE_THOUSAND = sheet.crop(170, 86,18,7);
        TWO_THOUSAND = sheet.crop(168, 106,20,7);
        THREE_THOUSAND = sheet.crop(168, 126,20,7);
        FIVE_THOUSAND = sheet.crop(168, 145,20,9);
    }
    /**
     * Crops any sheets with the same or larger dimensions but is specified for /textures/pacman.png
     * Initializes all wall images
     * @param sheet Sheet to crop.
     */
    private static void cropWall(SpriteSheet sheet){
        //Big left block crop
        WALL_EDGE = sheet.crop(4,4,88,88);
        //Vertical block crop
        WALL_EDGE_UP = sheet.crop(196,4,24,24);
        WALL_CENTER_UP = sheet.crop(196,29,24,24);
        WALL_EDGE_DOWN = sheet.crop(196,68,24,24);
        //Horizontal block crop
        WALL_EDGE_LEFT = sheet.crop(228,4,24,24);
        WALL_CENTER_SIDE = sheet.crop(252,4,24,24);
        WALL_EDGE_RIGHT = sheet.crop(292,4,24,24);
    }
    /**
     * Crops any sheets with the same or larger dimensions but is specified for /textures/pacman.png
     * Initializes all visual images (Game Over, Ready etc)
     * @param sheet Sheet to crop.
     */
    public static void cropVisuals(SpriteSheet sheet){
        READY = sheet.crop(202,2,47,7);
        GAME_OVER = sheet.crop(13,192,79,7);
    }

    /**
     * Decides which color the bot is going to have.
     * @param value either 1 or 2
     * @return the color
     */
    public static BufferedImage[] chooseGhost(int value){
        if(value == 1){
            return GHOSTS[0];
        } else{
            return GHOSTS[3];
        }
    }
}
