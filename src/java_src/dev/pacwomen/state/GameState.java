package dev.pacwomen.state;

import dev.pacwomen.characters.Ghost;
import dev.pacwomen.characters.PacWoman;
import dev.pacwomen.characters.OtherPlayers;
import dev.pacwomen.characters.OtherGhosts;
import dev.pacwomen.game.Handler;
import dev.pacwomen.worlds.World;
import dev.pacwomen.communication.Mailbox;
import dev.pacwomen.gfx.Assets;
import static dev.pacwomen.tiles.Tile.TILEHEIGHT;
import static dev.pacwomen.tiles.Tile.TILEWIDTH;

import com.ericsson.otp.erlang.*;

import java.awt.*;

/**
 * A state represent a specific window in the game. This is a gamestate, where
 * the game will be played.
 */
public class GameState extends State{
    protected static final float DEFAULT_START_X = 100, DEFAULT_START_Y = 100;

    private PacWoman pacWoman;
    private static Ghost[] ghosts = new Ghost[1];
    private World world;
    private OtherPlayers players;
    private OtherGhosts otherghosts;
    
    public GameState(Handler handler, Mailbox mailbox) {
        super(handler);

	int[] coordinates = mailbox.initiatePlayer();
	OtpErlangTuple tuple = mailbox.initiateMap();
	initializePlayers(handler, coordinates[0], coordinates[1]);
        this.world = new World(tuple, mailbox);
        initializeGhosts(coordinates[2], coordinates[3], coordinates[4]);
	this.otherghosts = new OtherGhosts(coordinates[2], coordinates[3], coordinates[2] * TILEWIDTH, coordinates[3] * TILEHEIGHT, Assets.chooseGhost(coordinates[4]));
	this.status = 1;
	
    }

    public boolean startOrEndScreen() {
	return false;
    }

    @Override
    public int getPoints(){
	return this.pacWoman.getPoints();
    }

/**
 * Updates the world, player and ghost according to implementation in their classes
 */
@Override
public void tick(Mailbox mailbox) {
    tickGhosts(mailbox);
    pacWoman.tick(mailbox);
    if(pacWoman.won()){
	this.status = 5;
	return;
    } else if (pacWoman.dead()){
	this.status = -1;
	return;
    }
    world.tick(mailbox);
    players.tick(mailbox);
    otherghosts.tick(mailbox);
}

private void tickGhosts(Mailbox mailbox){
    for(Ghost ghost : ghosts){
        ghost.tick(mailbox);
    }
}

private void initializePlayers(Handler handler, int x, int y) {
    int xL = x * TILEWIDTH - TILEWIDTH / 2;
    int yL = x * TILEHEIGHT - TILEHEIGHT / 2 - TILEHEIGHT / 2;
    this.pacWoman = new PacWoman(handler, xL, yL, TILEWIDTH * 2, TILEHEIGHT * 2);

    this.players = new OtherPlayers(-1,-1, TILEWIDTH * 2, TILEHEIGHT * 2);
}

/**
 * Chooses which ghost to print and where.
 * @param x,y the positions
 * @param z the value 1 or 2 depending on what algorithm
 */
private static void initializeGhosts(int x, int y, int z){
  if(z == 1){
    ghosts[0] = new Ghost(x, y, x * TILEWIDTH, y * TILEHEIGHT, Assets.RED_GHOST);
  } else{
    ghosts[0] = new Ghost(x, y, x * TILEWIDTH, y * TILEHEIGHT, Assets.YELLOW_GHOST);
  }
}

/**
 * Draws the world, player and ghost
 * @param g, the graphics package
 */
@Override
public void render(Graphics g) {
    world.render(g);
    pacWoman.render(g);
    renderGhosts(g);
    players.render(g);
    otherghosts.render(g);
}

private void renderGhosts(Graphics g){
    for(Ghost ghost : ghosts){
        ghost.render(g);
    }
  }
}
