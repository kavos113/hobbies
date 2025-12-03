package test;

import javax.imageio.ImageIO;
import java.awt.AWTException;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;

public class SampleRun {

    public static void main(String[] args) {

        new SampleRun();
    }

    public SampleRun(){

        String path = "output\\cap.png";
        File file = new File(path);

        Sample sample = null;

        try {
            sample = new Sample();
        } catch (AWTException e) {
            e.printStackTrace();
        }

        BufferedImage image = sample.createScreenCapture(new Rectangle(Toolkit.getDefaultToolkit().getScreenSize()));

        try {
            ImageIO.write(image, "png", file);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

}
