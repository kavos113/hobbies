package main;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfByte;
import org.opencv.core.Size;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.videoio.VideoWriter;

import javax.imageio.ImageIO;
import java.awt.AWTException;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.Toolkit;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.TimeUnit;

public class Capture {
    static {System.loadLibrary(Core.NATIVE_LIBRARY_NAME);}

    Rectangle screenRectangle = new Rectangle(Toolkit.getDefaultToolkit().getScreenSize());
    String output;
    Timer timer;
    VideoWriter encoder;
    Robot robot;

    ArrayList<BufferedImage> tempCapture = new ArrayList<>();

    public Capture(){
        robot = null;

        try {
            robot = new Robot();
        } catch (AWTException e) {
            e.printStackTrace();
        }
    }

    public void createCapture() {
        long l = System.currentTimeMillis();
        BufferedImage capture;

        capture = robot.createScreenCapture(screenRectangle);
        tempCapture.add(capture);
        System.out.println(System.currentTimeMillis() - l);
    }

    public void startCapture(){
        int fourcc = VideoWriter.fourcc('m', 'p', '4', 'v');
        encoder = new VideoWriter(output, fourcc, 25, new Size(screenRectangle.width, screenRectangle.height));

        timer = new Timer();
        timer.schedule(new CaptureTimer(), 0, 40);

        Thread thread = new EncodeThread();
        try {
            TimeUnit.MILLISECONDS.sleep(100);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        //thread.start();
    }

    public void stopCapture(){
        timer.cancel();
        encode();
    }

    public void setFoldeerPath(String path) {
        LocalDateTime dateTime = LocalDateTime.now();

        DateTimeFormatter dFormatter = DateTimeFormatter.ofPattern("yyyy_MM_dd__HH_mm_ss");
        String date = dFormatter.format(dateTime);

        output = path + date + ".mp4";
    }

    public static Mat bufferedImageToMat(BufferedImage image){
        ByteArrayOutputStream bout = new ByteArrayOutputStream();

        try {
            ImageIO.write(image, "png", bout);
            bout.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

        MatOfByte mat = new MatOfByte(bout.toByteArray());

        return Imgcodecs.imdecode(mat, Imgcodecs.IMREAD_COLOR);
    }

    public void setCaptureRange(Rectangle rect){
        screenRectangle = rect;
    }

    class CaptureTimer extends TimerTask{

        @Override
        public void run() {
            createCapture();
        }
    }

    class EncodeThread extends Thread{

        @Override
        public void run() {
            while (true){
                if (tempCapture.size() != 0){
                    BufferedImage ecdImage = tempCapture.get(0);

                    encoder.write(bufferedImageToMat(ecdImage));
                    tempCapture.remove(0);
                }else {
                    break;
                }
            }

            encoder.release();
            System.out.println("owattayo");
        }
    }

    int enc = 0;

    public void encode (){
        for (int i = 0; i < tempCapture.size(); i++){
            encoder.write(bufferedImageToMat(tempCapture.get(i)));
            enc++;

            if (enc % (tempCapture.size() / 10) == 0){
                System.out.println(i * 100 / tempCapture.size() + "%");
            }
        }
        encoder.release();
        System.out.println("owattayo");
    }
}