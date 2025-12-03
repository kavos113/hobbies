import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class Main extends JFrame {

    JLabel timerTime;
    JSpinner hourSpinner, minSpinner, secSpinner;
    TimerEngine timerEngine;

    public static void main(String[] args) {
        new Main();
    }

    public Main(){
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        setTitle("Timer");
        setBounds(100, 100, 800, 400);

        createUIComponents();

        timerEngine = new TimerEngine(0, 0, 0, this);

        setVisible(true);
    }

    private void createUIComponents(){
        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());

        timerTime = new JLabel("00:00:00");
        timerTime.setHorizontalAlignment(JLabel.CENTER);
        timerTime.setFont(new Font("courier new", Font.BOLD, 150));


        JPanel buttonsPanel = new JPanel();
        buttonsPanel.setLayout(new FlowLayout());

        JButton startButton = new JButton("START");
        startButton.setPreferredSize(new Dimension(120, 40));
        startButton.addActionListener(new buttonEvent());
        startButton.setActionCommand("start");

        JButton stopButton = new JButton("STOP");
        stopButton.setPreferredSize(new Dimension(120, 40));
        stopButton.addActionListener(new buttonEvent());
        stopButton.setActionCommand("stop");

        JButton cancelButton = new JButton("CANCEL");
        cancelButton.setPreferredSize(new Dimension(120, 40));
        cancelButton.addActionListener(new buttonEvent());
        cancelButton.setActionCommand("cancel");

        JButton settingButton = new JButton("SET TIME");
        settingButton.setPreferredSize(new Dimension(120, 40));
        settingButton.addActionListener(new buttonEvent());
        settingButton.setActionCommand("set");

        SpinnerNumberModel hourModel = new SpinnerNumberModel(0, 0, null, 1);
        hourSpinner = new JSpinner(hourModel);
        hourSpinner.setPreferredSize(new Dimension(70, 40));
        hourSpinner.setFont(new Font("メイリオ", Font.PLAIN, 20));

        SpinnerNumberModel minModel = new SpinnerNumberModel(0, 0, 59, 1);
        minSpinner = new JSpinner(minModel);
        minSpinner.setPreferredSize(new Dimension(50, 40));
        minSpinner.setFont(new Font("メイリオ", Font.PLAIN, 20));

        SpinnerNumberModel secModel = new SpinnerNumberModel(0, 0, 59, 1);
        secSpinner = new JSpinner(secModel);
        secSpinner.setPreferredSize(new Dimension(50, 40));
        secSpinner.setFont(new Font("メイリオ", Font.PLAIN, 20));

        JLabel hourSpinnerLabel = new JLabel("時間");
        //hourSpinnerLabel.setPreferredSize(new Dimension(40, 40));
        hourSpinnerLabel.setFont(new Font("メイリオ", Font.PLAIN, 20));

        JLabel minSpinnerLabel = new JLabel("分");
        //minSpinnerLabel.setPreferredSize(new Dimension(40, 40));
        minSpinnerLabel.setFont(new Font("メイリオ", Font.PLAIN, 20));

        JLabel secSpinnerLabel = new JLabel("秒");
        //secSpinnerLabel.setPreferredSize(new Dimension(40, 40));
        secSpinnerLabel.setFont(new Font("メイリオ", Font.PLAIN, 20));

        buttonsPanel.add(startButton);
        buttonsPanel.add(stopButton);
        buttonsPanel.add(cancelButton);
        buttonsPanel.add(settingButton);
        buttonsPanel.add(hourSpinner);
        buttonsPanel.add(hourSpinnerLabel);
        buttonsPanel.add(minSpinner);
        buttonsPanel.add(minSpinnerLabel);
        buttonsPanel.add(secSpinner);
        buttonsPanel.add(secSpinnerLabel);


        mainPanel.add(timerTime, BorderLayout.CENTER);
        mainPanel.add(buttonsPanel, BorderLayout.SOUTH);

        add(mainPanel);
    }

    public void setTimerTimeText(int hour, int min, int sec){
        timerTime.setText(String.format("%02d:%02d:%02d\n", hour, min, sec));
    }

    class buttonEvent implements ActionListener{

        @Override
        public void actionPerformed(ActionEvent e) {
            String cmd = e.getActionCommand();
            switch (cmd) {
                case "set":
                    Main.this.setTimerTimeText((Integer) hourSpinner.getValue(), (Integer) minSpinner.getValue(), (Integer) secSpinner.getValue());
                    timerEngine.setTime((Integer) hourSpinner.getValue(), (Integer) minSpinner.getValue(), (Integer) secSpinner.getValue());
                    break;
                case "start":
                    timerEngine.start();
                    break;
                case "stop":
                    timerEngine.stop();
                    break;
                case "cancel":
                    if (timerEngine.timer != null) {
                        timerEngine.stop();
                    }
                    timerEngine.cancel();
                    Main.this.setTimerTimeText(0, 0, 0);
                    break;
            }
        }
    }
}
