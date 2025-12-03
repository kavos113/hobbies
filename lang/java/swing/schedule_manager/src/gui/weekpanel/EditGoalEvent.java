package gui.weekpanel;

import gui.weekpanel.dialogs.GoalEditDialog;

import javax.swing.JLabel;
import javax.swing.JOptionPane;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

public class EditGoalEvent implements ActionListener {

    private final ArrayList<JLabel> labels = new ArrayList<>();

    @Override
    public void actionPerformed(ActionEvent e) {
        String[][] texts = new String[labels.size()][2];

        for (int i = 0; i < labels.size(); i++) {
            texts[i] = labels.get(i).getText().split(":", 2);
        }
        new GoalEditDialog(this, texts);
    }

    public void addLabel(JLabel label){
        labels.add(label);
    }

    public void updateTexts(String[] texts){
        if(texts.length != labels.size()){
            JOptionPane.showMessageDialog(null, "yabaidesuyo", "error", JOptionPane.ERROR_MESSAGE);
            return;
        }

        for (int i = 0; i < labels.size(); i++) {
            labels.get(i).setText(texts[i]);
        }
    }
}
