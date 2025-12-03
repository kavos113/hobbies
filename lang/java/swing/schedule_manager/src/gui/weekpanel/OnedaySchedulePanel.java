package gui.weekpanel;

import main.ScheduleType;

import javax.swing.JPanel;
import java.awt.Color;
import java.awt.GridLayout;

public class OnedaySchedulePanel extends JPanel {

    private TimeTablePanel[] timeTables;

    public OnedaySchedulePanel() {
        initLayout();
    }

    private void initLayout(){
        final int HOUR_PER_DAY = 17;

        GridLayout layout = new GridLayout(HOUR_PER_DAY, 1);
        setLayout(layout);

        timeTables = new TimeTablePanel[HOUR_PER_DAY];
        for (int i = 0; i < timeTables.length; i++) {
            timeTables[i] = new TimeTablePanel();
            add(timeTables[i]);
        }
    }

    public void configureColor(int start, int end, String content, ScheduleType type){

        for (int i = start; i < end; i++) {
            timeTables[i / 4].settingColor(i % 4, backGround(type), foreGround(type));
        }
        timeTables[start / 4].setScheduleText(start % 4, content);
    }

    public void deleteColor(int start,int end){
        for (int i = start; i < end; i++) {
            timeTables[i / 4].settingColor(i % 4, null, null);
        }
        timeTables[start / 4].setScheduleText(start % 4, "");
    }

    private Color backGround(ScheduleType type){
        switch (type) {
            case 受講 -> {
                return new Color(0, 225, 0);
            }
            case その他東進or学習 -> {
                return new Color(0, 130, 0);
            }
            case 学校 -> {
                return new Color(0, 110, 200);
            }
            case 生活 -> {
                return new Color(222, 184, 135);
            }
            case 趣味その他 -> {
                return new Color(255, 0, 0);
            }
            case 睡眠 -> {
                return new Color(180, 180, 180);
            }
        }
        return null;
    }

    private Color foreGround(ScheduleType type){
        switch (type) {
            case 受講, 睡眠, 生活 -> {
                return new Color(0, 0, 0);
            }
            case その他東進or学習, 学校, 趣味その他 -> {
                return new Color(255, 255, 255);
            }
        }
        return null;
    }
}
