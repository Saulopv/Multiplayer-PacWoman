package dev.pacwomen.game;

import dev.pacwomen.input.KeyManager;

public class Handler {
    private Game game;
    // map


    public Handler(Game game) {
        this.game = game;
    }


    public KeyManager getKeyManager(){
        return game.getKeyManager();
    }
}
