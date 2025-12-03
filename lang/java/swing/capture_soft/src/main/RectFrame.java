package main;

import java.awt.Color;

import javax.swing.JFrame;

public class RectFrame extends JFrame {

    public RectFrame() {
        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        setBounds(200, 200, 400, 400);
        setUndecorated(true);
        setBackground(new Color(0, 0, 0, 0));
    }
}
