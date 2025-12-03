package main;

import gui.weekpanel.OnedayPanel;

import javax.swing.JOptionPane;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;

public class SaveFileMaker {

    public static void save(OnedayPanel[] currentWeek, String[] goals){
        String dateForPath = currentWeek[0].getSchedule().getYear() + "_"
                + currentWeek[0].getSchedule().getMonth() + "_" + currentWeek[0].getSchedule().getDay();

        String pathname = "saves\\From" + dateForPath + ".csv";

        if (isExistFile(pathname)){
            int status = JOptionPane.showConfirmDialog(null, "Do you want to OVERWRITE?",
                    "ファイルの上書き", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);

            if (status != JOptionPane.YES_OPTION){
                System.out.println(status);
                return;
            }
        }

        File saveFile = new File(pathname);

        try(FileWriter writer = new FileWriter(saveFile)) {

            writer.write(currentWeek[0].getSchedule().getYear() + "," + currentWeek[0].getSchedule().getDayFromNewYear());
            for (String goal : goals) {
                writer.write("," + goal);
            }
            writer.write("\n");


            for (OnedayPanel onedayPanel : currentWeek) {
                DailySchedule dayData = onedayPanel.getSchedule();
                ArrayList<Schedule> list = dayData.getScheduleList();

                for (Schedule schedule : list) {
                    writer.write(dayData.getYear() + ",");
                    writer.write(dayData.getDayFromNewYear() + ",");
                    writer.write(schedule.startTime() + ",");
                    writer.write(schedule.endTime() + ",");
                    writer.write(schedule.content() + ",");
                    writer.write(schedule.type().toString());
                    writer.write("\n");
                }
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static boolean isExistFile(String pathname){
        Path path = Paths.get(pathname);
        path = path.toAbsolutePath();

        return Files.exists(path);
    }
}
