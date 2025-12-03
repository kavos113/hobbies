package main;

import gui.weekpanel.OnedayPanel;
import utils.LoadProperties;

import javax.swing.JOptionPane;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

public class SaveFileLoader {

    private final OnedayPanel[] onedayPanels = new OnedayPanel[7];
    private final DailySchedule[] schedules = new DailySchedule[7];
    private int firstday;
    private int year;

    public SaveFileLoader(){

    }

    public LoadProperties load(File saveFile){
        String[] goals = new String[6];
        try {
            BufferedReader reader = new BufferedReader(new FileReader(saveFile));

            String str = reader.readLine();
            String[] firstLine = str.split(",", -1);
            year = Integer.parseInt(firstLine[0]);
            firstday = Integer.parseInt(firstLine[1]);

            goals = Arrays.copyOfRange(firstLine, 2, firstLine.length);


            initDailySchedule();

            str = reader.readLine();

            while (str != null){
                addSchedule(str.split(",", -1));

                str = reader.readLine();
            }

            createOnedayPanel();
            settingColor();

            reader.close();
        } catch (IOException e) {
            JOptionPane.showMessageDialog(null, "Loading Error",
                    "Error", JOptionPane.ERROR_MESSAGE);
            e.printStackTrace();
        }

        return new LoadProperties(onedayPanels, firstday, goals);
    }

    private void initDailySchedule(){
        for (int i = 0; i < schedules.length; i++) {
            schedules[i] = new DailySchedule(year, firstday + i, i);
        }
    }

    private void addSchedule(String[] scheduleLine){
        int day = Integer.parseInt(scheduleLine[1]);

        int startTime = Integer.parseInt(scheduleLine[2]);
        int endTime = Integer.parseInt(scheduleLine[3]);
        String contnt = scheduleLine[4];
        ScheduleType type = ScheduleType.convertToType(scheduleLine[5]);

        schedules[day - firstday].addSchedule(startTime, endTime, contnt, type);
    }

    private void createOnedayPanel(){
        for (int i = 0; i < onedayPanels.length; i++) {
            onedayPanels[i] = new OnedayPanel(schedules[i]);
        }
    }

    private void settingColor(){
        for (int i = 0; i < schedules.length; i++) {
            ArrayList<Schedule> scheduleList = schedules[i].getScheduleList();
            for (Schedule schedule : scheduleList) {
                onedayPanels[i].configureColor(schedule);
            }
        }
    }
}
