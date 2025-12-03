package main;

import javax.imageio.ImageIO;
import java.awt.AWTException;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class ScreenPrinter {

    public static void printScreen(String folderPath, Rectangle region){
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss");
        String time = formatter.format(LocalDateTime.now());

        File output = new File(folderPath + "\\SMcapture" + time + ".png");
        try {
            Robot robot = new Robot();

            BufferedImage screenShot = robot.createScreenCapture(region);
            ImageIO.write(screenShot, "png", output);
        } catch (AWTException | IOException e) {
            e.printStackTrace();
        }
    }

}
