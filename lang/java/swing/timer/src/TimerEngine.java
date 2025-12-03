import javax.swing.JOptionPane;
import java.util.Timer;
import java.util.TimerTask;

public class TimerEngine {
    int hour, min, sec;
    Timer timer;
    Main parentFrame;

    public TimerEngine(int hour, int min, int sec, Main parentFrame) {
        this.hour = hour;
        this.min = min;
        this.sec = sec;
        this.parentFrame = parentFrame;
    }

    public void start(){
        timer = new Timer();
        timer.scheduleAtFixedRate(new perSecond(), 1000, 1000);
    }

    public void stop(){
        timer.cancel();
        timer = null;
    }

    public void cancel(){
        hour = 0;
        min = 0;
        sec = 0;
    }

    public void setTime(int hour, int min, int sec) {
        this.hour = hour;
        this.min = min;
        this.sec = sec;
    }

    class perSecond extends TimerTask{
        @Override
        public void run() {
            if(hour == 0 && min == 0 && sec == 0){
                TimerEngine.this.stop();
                TimerEngine.this.cancel();
                JOptionPane.showMessageDialog(parentFrame, "タイマー終了", "THE END", JOptionPane.INFORMATION_MESSAGE);
                return;
            }

            checkZero();

            sec--;

            parentFrame.setTimerTimeText(hour, min, sec);
        }

        private void checkSixty(){
            if(sec == 60){
                sec = 0;
                min++;
            }

            if(min == 60){
                min = 0;
                hour++;
            }
        }

        private void checkZero(){
            if(sec == 0){
                sec = 60;

                if(min == 0){
                    min = 60;
                    hour--;
                }

                min--;
            }


        }
    }
}
