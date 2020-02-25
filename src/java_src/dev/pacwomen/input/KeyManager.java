package dev.pacwomen.input;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import dev.pacwomen.game.Game;
import dev.pacwomen.communication.Mailbox;

/**
 * The keymanager manages the input from the players from the keyboard
 */

public class KeyManager implements KeyListener {
    private boolean[] keys;

    public static boolean UP,DOWN,LEFT,RIGHT;

    /**
     * The keymanages has a array of booleans. This is used in other methods to check
     * if a keypress has returned true.
     */
    public KeyManager() {
        keys = new boolean[256];
    }


    public int tick(){
	if(keys[KeyEvent.VK_R]){
	    return 1;
	} else if(keys[KeyEvent.VK_Q]){
	    return -1;
	} else {
	    return 0;
	}
    }
    /**
     * Updates which buttons we are pressing. Currently we only use w,s,a,d.
     * @param mailbox, the mailbox connected to the player
     */
    public void tick(Mailbox mailbox){
        UP = keys[KeyEvent.VK_W];
        DOWN = keys[KeyEvent.VK_S];
        LEFT = keys[KeyEvent.VK_A];
        RIGHT = keys[KeyEvent.VK_D];
        if (UP == true)
            {
                mailbox.sendKeyManager(1);
                return;
            }
        else if(DOWN == true)
            {
                mailbox.sendKeyManager(2);
                return;
            }
        else if(LEFT == true)
            {
                mailbox.sendKeyManager(3);
                return;
            }
        else if(RIGHT == true)
            {
                mailbox.sendKeyManager(4);
                return;
            }
        else//no direction
            {
                mailbox.sendKeyManager(0);
                return;
            }
    }
    @Override
    public void keyTyped(KeyEvent e) {

    }

    /**
     * If a key is pressed, set the specific key in the array to true.
     * @param e, a keyevent
     */
    @Override
    public void keyPressed(KeyEvent e) {
        keys[e.getKeyCode()] = true;
    }


    /**
     * If a key is released, set the specific key in the array to false.
     * @param e, a keyevent
     */
    @Override
    public void keyReleased(KeyEvent e) {
        keys[e.getKeyCode()] = false;
    }
}
