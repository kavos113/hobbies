package view;

import javax.swing.JPanel;
import java.awt.Color;

public class ViewerPanel extends JPanel {
    public ViewerPanel() {
        super();
        this.setOpaque(true);
        this.setBackground(Color.GREEN);
        this.setBorder(javax.swing.BorderFactory.createLineBorder(Color.BLACK));
    }
}
