package view.components;

import javax.swing.JButton;
import java.awt.Font;
import java.awt.Insets;
import java.awt.event.ActionListener;

public class ProcessButton extends JButton {
    public ProcessButton(String text, String actionCommand, ActionListener listener) {
        super();
        this.setFont(new Font("Migu 1P", Font.PLAIN, 20));
        this.setMargin(new Insets(5, 5, 5, 5));
        this.setText(text);
        this.setActionCommand(actionCommand);
        this.addActionListener(listener);
    }
}
