package dev.pacwomen.state;

import dev.pacwomen.game.Game;
import dev.pacwomen.game.Handler;
import dev.pacwomen.communication.Mailbox;

import java.awt.*;

public abstract class State {
    protected Handler handler;
    public int status; //0 for startscreen, 1 for playing, -1/5 for loss/win

    public State(Handler handler) {
        this.handler = handler;

    }
    public abstract void tick(Mailbox mailbox);
    public abstract void render(Graphics g);
    public int getPoints(){
	return -1;
    }
}

