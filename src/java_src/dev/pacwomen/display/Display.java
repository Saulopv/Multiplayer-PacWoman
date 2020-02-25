package dev.pacwomen.display;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyListener;

public class Display {
    private JFrame frame;
    private Canvas canvas;
    private String title;
    private int width, height;

    public Display(String title, int width, int height){
        this.title = title;
        this.width = width;
        this.height = height;
        createDisplay();
    }

    /**
     * Initializes and sets the frame.
     * @see JFrame
     */
    private void initFrame(){
        frame = new JFrame(title);
        frame.setSize(width,height);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setLocationRelativeTo(null);
        frame.setResizable(false);
        frame.setVisible(true);
    }
    /**
     * Initializes and sets the canvas.
     * @see Canvas
     */
    private void initCanvas(){
        canvas = new Canvas();
        canvas.setPreferredSize(new Dimension(width, height));
        canvas.setMaximumSize(new Dimension(width, height));
        canvas.setMinimumSize(new Dimension(width, height));
        frame.add(canvas);
        frame.pack();
    }

    /**
     * Creates the display through initializing the frame and the canvas.
     */
    private void createDisplay(){
        initFrame();
        initCanvas();
    }

    /**
     * Adds a keylistener to the frame.
     * @param l The keylistener to add.
     * @see KeyListener
     */
    public void addKeyListener(KeyListener l){
        this.frame.addKeyListener(l);
    }

    /**
     * @return The displays canvas.
     */
    public Canvas getCanvas(){
        return canvas;
    }

    /**
     *
     * @return The displays frame.
     */
    public JFrame getFrame() {
        return frame;
    }

}
