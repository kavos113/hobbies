package gui.weekpanel;

import javax.swing.JLabel;
import javax.swing.JPanel;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.GridLayout;

public class TimeTablePanel extends JPanel {

    private JLabel[] detailTimes;

    public TimeTablePanel(){
        initLayout();
    }

    private void initLayout(){
        setLayout(new GridLayout(4, 1));

        detailTimes = new JLabel[4];
        for (int i = 0; i < detailTimes.length; i++) {
            detailTimes[i] = new JLabel();
            detailTimes[i].setOpaque(true);
            detailTimes[i].setFont(new Font("Meiryo UI", Font.PLAIN, 10));
            add(detailTimes[i]);
        }
    }

    public void settingColor(int indexInTimeTablePanel, Color backColor, Color foreColor){
        detailTimes[indexInTimeTablePanel].setBackground(backColor);
        detailTimes[indexInTimeTablePanel].setForeground(foreColor);
    }

    public void setScheduleText(int index, String content){
        detailTimes[index].setText(content);
    }

    //中心線を引く
    @Override
    public void paint(Graphics g) {
        super.paint(g);

        g.drawLine(0, getHeight() / 2, getWidth(), getHeight() / 2);
    }
}
