package view;

import javax.swing.JLabel;
import javax.swing.JPanel;
import java.awt.Font;

public class HeaderPanel extends JPanel {
    public HeaderPanel(String text, int fontSize) {
        super();
        initLayout(text, fontSize);
    }

    private void initLayout(String text, int fontSize){
        JLabel header = new JLabel(text);
        header.setFont(new Font("Migu 1P", Font.BOLD, fontSize));
        this.add(header);
    }
}
