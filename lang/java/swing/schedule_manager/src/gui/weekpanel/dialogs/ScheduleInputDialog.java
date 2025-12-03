package gui.weekpanel.dialogs;

import gui.weekpanel.OnedayPanel;
import main.Schedule;
import main.ScheduleType;
import utils.TimeConvert;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class ScheduleInputDialog extends JDialog {

    private boolean isFixMode = false;
    private final int startTime;
    private int indexOfSchedule;
    private final OnedayPanel[] onedayPanels;
    private Schedule beforeFixSchedule;
    private JTextField startTimeHour, startTimeMin;
    private JTextField endTimeHour, endTimeMin;
    private JTextField contentText;
    private JComboBox<ScheduleType> typeSelector;

    /**
     * 新規作成用
     * パネルが配列なのは繰り返し予定の処理用
     */
    public ScheduleInputDialog(OnedayPanel[] onedayPanels, int startTime) {
        this.startTime = startTime;
        this.onedayPanels = onedayPanels;

        setTitle("Schedule Input");
        setBounds(1150, 150, 500, 300);
        setResizable(false);
        initLayout();

        setVisible(true);
    }

    /**
     * 修正用
     */
    public ScheduleInputDialog(OnedayPanel[] onedayPanels, Schedule schedule, int index){
        this(onedayPanels, schedule.startTime());

        setDefaults(schedule.endTime(), schedule.content(), schedule.type());
        isFixMode = true;
        beforeFixSchedule = schedule;
        indexOfSchedule = index;
    }

    private void setDefaults(int endTime, String content, ScheduleType type){
        String[] endTimes = TimeConvert.TimeToString(endTime).split(":");

        endTimeHour.setText(endTimes[0]);
        endTimeMin.setText(endTimes[1]);
        contentText.setText(content);
        typeSelector.setSelectedItem(type);
    }

    private void initLayout(){
        GridLayout layout = new GridLayout(1, 2);
        setLayout(layout);

        createTimePanel();
        createNaiyoPanel();

        setStartTime();
    }

    private void createTimePanel(){
        JPanel timePanel = new JPanel();
        timePanel.setLayout(null);

        JLabel startTimeLabel = new JLabel("Start:");
        startTimeLabel.setBounds(30, 50, 100, 50);
        startTimeLabel.setFont(new Font("Ricty Diminished", Font.PLAIN, 25));
        timePanel.add(startTimeLabel);

        startTimeHour = new JTextField();
        startTimeHour.setBounds(120, 50, 40, 50);
        startTimeHour.setFont(new Font("Ricty Diminished", Font.PLAIN, 25));
        timePanel.add(startTimeHour);

        JLabel startTimeColon = new JLabel(":");
        startTimeColon.setBounds(160, 50, 15, 50);
        startTimeColon.setFont(new Font("Ricty Diminished", Font.PLAIN, 25));
        timePanel.add(startTimeColon);

        startTimeMin = new JTextField();
        startTimeMin.setBounds(175, 50, 40, 50);
        startTimeMin.setFont(new Font("Ricty Diminished", Font.PLAIN, 25));
        timePanel.add(startTimeMin);

        JLabel endTime = new JLabel("End:");
        endTime.setBounds(30, 100, 100, 50);
        endTime.setFont(new Font("Ricty Diminished", Font.PLAIN, 25));
        timePanel.add(endTime);

        endTimeHour = new JTextField();
        endTimeHour.setBounds(120, 100, 40, 50);
        endTimeHour.setFont(new Font("Ricty Diminished", Font.PLAIN, 25));
        timePanel.add(endTimeHour);

        JLabel endTimeColon = new JLabel(":");
        endTimeColon.setBounds(160, 100, 15, 50);
        endTimeColon.setFont(new Font("Ricty Diminished", Font.PLAIN, 25));
        timePanel.add(endTimeColon);

        endTimeMin = new JTextField();
        endTimeMin.setBounds(175, 100, 40, 50);
        endTimeMin.setFont(new Font("Ricty Diminished", Font.PLAIN, 25));
        timePanel.add(endTimeMin);

        add(timePanel);
    }

    private void createNaiyoPanel(){
        String[] contentSelectTexts =
                {"数学受講", "物理受講", "英語受講", "計算演習", "テスト直し",
                "数学α", "EXCEL化学", "セミナー物理", "学校英語",
                "読書", "プログラミング", "ごはん・その他", "学校（登校）",
                "受講の予習", "ねる"};

        JPanel panel = new JPanel();
        panel.setLayout(null);

        JLabel contentLabel = new JLabel("内容");
        contentLabel.setBounds(30, 30, 80, 50);
        contentLabel.setFont(new Font("Ricty Diminished", Font.PLAIN, 20));
        panel.add(contentLabel);

        JComboBox<String> contentSelecter = new JComboBox<>(contentSelectTexts);
        contentSelecter.setBounds(110, 30, 120, 30);
        contentSelecter.setFont(new Font("Ricty Diminished", Font.PLAIN, 15));
        panel.add(contentSelecter);

        contentText = new JTextField();
        contentText.setBounds(30, 80, 180, 30);
        contentText.setFont(new Font("Ricty Diminished", Font.PLAIN, 15));
        panel.add(contentText);

        contentSelecter.addActionListener(new getSelect(contentSelecter, contentText));

        JLabel typeLabel = new JLabel("種類:");
        typeLabel.setBounds(30, 150, 80, 30);
        typeLabel.setFont(new Font("Ricty Diminished", Font.PLAIN, 20));
        panel.add(typeLabel);

        typeSelector = new JComboBox<>(ScheduleType.values());
        typeSelector.setBounds(110, 150, 100, 30);
        typeSelector.setFont(new Font("Ricty Diminished", Font.PLAIN, 15));
        panel.add(typeSelector);

        createNextButton(panel);

        add(panel);
    }

    private void createNextButton(JPanel parent){
        JButton cancel = new JButton("キャンセル");
        cancel.setBounds(10, 215, 110, 30);
        cancel.setFont(new Font("Ricty Diminished", Font.BOLD, 15));
        cancel.addActionListener(new buttonPressed());
        cancel.setActionCommand("cancel");
        parent.add(cancel);

        JButton end = new JButton("終");
        end.setBounds(130, 215, 50, 30);
        end.setFont(new Font("Ricty Diminished", Font.BOLD, 15));
        end.addActionListener(new buttonPressed());
        end.setActionCommand("end");
        parent.add(end);

        JButton next = new JButton("次");
        next.setBounds(190, 215, 50, 30);
        next.setFont(new Font("Ricty Diminished", Font.BOLD, 15));
        next.addActionListener(new buttonPressed());
        next.setActionCommand("next");
        parent.add(next);
    }

    private void setStartTime(){
        int[] times = TimeConvert.TimeToInt(startTime);

        startTimeHour.setText(String.valueOf(times[0]));
        startTimeMin.setText(String.valueOf(times[1]));
    }

    record getSelect(JComboBox<String> selector, JTextField textField) implements ActionListener {

        @Override
            public void actionPerformed(ActionEvent e) {
                String selectedText = (String) selector.getSelectedItem();
                textField.setText(selectedText);
            }
        }

    class buttonPressed implements ActionListener{

        @Override
        public void actionPerformed(ActionEvent e) {
            String cmd = e.getActionCommand();

            if(cmd.equals("cancel")){
                setVisible(false);
                dispose();
                return;
            }

            if(checkInput()) return;

            int startTime = TimeConvert.StringToTime(Integer.parseInt(startTimeHour.getText()), Integer.parseInt(startTimeMin.getText()));
            int endTime = TimeConvert.StringToTime(Integer.parseInt(endTimeHour.getText()), Integer.parseInt(endTimeMin.getText()));

            if (cmd.equals("next")) {

                if (isFixMode) {
                    fixMode(startTime, endTime);
                    return;
                }

                new ScheduleInputDialog(ScheduleInputDialog.this.onedayPanels, endTime);

                for (OnedayPanel onedayPanel : onedayPanels) {
                    onedayPanel.configureSchedule(startTime, endTime,
                            contentText.getText(), (ScheduleType) typeSelector.getSelectedItem());
                }

                setVisible(false);
                dispose();
            } else if (cmd.equals("end")) {
                if (isFixMode) {
                    fixMode(startTime, endTime);
                    return;
                }

                for (OnedayPanel onedayPanel : onedayPanels) {
                    onedayPanel.configureSchedule(startTime, endTime,
                            contentText.getText(), (ScheduleType) typeSelector.getSelectedItem());
                }
                setVisible(false);
                dispose();
            }
        }

        private void fixMode(int startTime, int endTime){
            Schedule afterFixed = new Schedule(startTime, endTime, contentText.getText(), (ScheduleType) typeSelector.getSelectedItem());
            for (OnedayPanel onedayPanel : onedayPanels) {
                onedayPanel.fixSchedule(beforeFixSchedule, afterFixed, indexOfSchedule);
            }
            setVisible(false);
            dispose();
        }

        //true:だめ false:OK
        private boolean checkInput(){
            if(endTimeHour.getText().equals("") || endTimeMin.getText().equals("") || startTimeHour.getText().equals("") || startTimeMin.getText().equals("")){
                JOptionPane.showMessageDialog(ScheduleInputDialog.this
                        , "時刻を入力してください", "入力エラー", JOptionPane.ERROR_MESSAGE);

                return true;
            }else if (Integer.parseInt(endTimeMin.getText()) % 15 != 0 || Integer.parseInt(startTimeMin.getText()) % 15 != 0){
                JOptionPane.showMessageDialog(ScheduleInputDialog.this,
                        "分は15分刻みで入力してください", "入力エラー", JOptionPane.ERROR_MESSAGE);

                return true;
            }

            if(contentText.getText().equals("")){
                JOptionPane.showMessageDialog(ScheduleInputDialog.this,
                        "内容を入力してください", "入力エラー", JOptionPane.ERROR_MESSAGE);
                return true;
            }

            return false;
        }
    }
}
