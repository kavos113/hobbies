package gui.weekpanel;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import java.awt.GridLayout;

public class ScheduleIndex extends JPanel {

    public ScheduleIndex() {
        initLayout();
    }

    private void initLayout(){
        GridLayout layout = new GridLayout(17, 1);
        setLayout(layout);

        JLabel[] timeStamps = new JLabel[17];
        for (int i = 0; i < timeStamps.length; i++) {
            timeStamps[i] = new JLabel(i + 6 + ":00");
            timeStamps[i].setBorder(new EmptyBorder(0, 0, 0, 8));
            timeStamps[i].setHorizontalAlignment(JLabel.RIGHT);
            add(timeStamps[i]);
        }
    }
}
