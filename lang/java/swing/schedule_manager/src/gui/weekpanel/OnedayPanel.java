package gui.weekpanel;

import gui.weekpanel.dialogs.ScheduleFixDialog;
import gui.weekpanel.dialogs.ScheduleInputDialog;
import main.DailySchedule;
import main.Schedule;
import main.ScheduleType;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.LineBorder;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class OnedayPanel extends JPanel {

    private final DailySchedule schedule;
    private OnedaySchedulePanel schedulePanel;

    public OnedayPanel(DailySchedule schedule) {
        this.schedule = schedule;

        initLayout();
    }

    private void initLayout(){
        setLayout(new BorderLayout());

        add(initHeadLayout(schedule.getMonth(), schedule.getDay()), BorderLayout.NORTH);
        add(initFootLayout(), BorderLayout.SOUTH);

        schedulePanel = new OnedaySchedulePanel();
        schedulePanel.setBorder(new LineBorder(Color.BLACK));
        add(schedulePanel, BorderLayout.CENTER);
    }

    private JPanel initHeadLayout(int month, int day){
        JPanel eachDatePanel = new JPanel();
        eachDatePanel.setLayout(new FlowLayout());
        eachDatePanel.setBorder(new LineBorder(new Color(0, 0, 0)));

        JLabel dateLabel = new JLabel(month + "/" + day);
        dateLabel.setFont(new Font("Ricty Diminished", Font.BOLD, 35));
        dateLabel.setOpaque(true);
        eachDatePanel.add(dateLabel);

        switch (schedule.getDayOfWeek()) {
            case 0 -> dateLabel.setForeground(Color.red);
            case 6 -> dateLabel.setForeground(Color.blue);
        }

        return eachDatePanel;
    }

    private JPanel initFootLayout(){
        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new FlowLayout());
        buttonPanel.setPreferredSize(new Dimension(100, 50));

        JButton createSchedule = new JButton("作成");
        createSchedule.setFont(new Font("Ricty Diminished", Font.BOLD, 15));
        createSchedule.setMargin(new Insets(5, 5, 5, 5));
        createSchedule.addActionListener(new createDialog());
        createSchedule.setActionCommand("new");
        buttonPanel.add(createSchedule);

        JButton fixSchedule = new JButton("修正");
        fixSchedule.setFont(new Font("Ricty Diminished", Font.BOLD, 15));
        fixSchedule.setMargin(new Insets(5, 5, 5, 5));
        fixSchedule.addActionListener(new createDialog());
        fixSchedule.setActionCommand("fix");
        buttonPanel.add(fixSchedule);

        return buttonPanel;
    }

    public void configureSchedule(int startTime, int endTime, String content, ScheduleType type){
        schedule.addSchedule(startTime, endTime, content, type);
        schedulePanel.configureColor(startTime, endTime, content, type);
    }

    public void fixSchedule(Schedule beforeFix, Schedule afterFix, int index){
        schedulePanel.deleteColor(beforeFix.startTime(), beforeFix.endTime());
        schedulePanel.configureColor(afterFix.startTime(), afterFix.endTime(), afterFix.content(), afterFix.type());
        schedule.fixSchedule(index, afterFix);
    }

    public void configureColor(Schedule schedule){
        schedulePanel.configureColor(schedule.startTime(), schedule.endTime(), schedule.content(), schedule.type());
    }

    public DailySchedule getSchedule() {
        return schedule;
    }

    class createDialog implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            String cmd = e.getActionCommand();
            switch (cmd) {
                case "new" -> new ScheduleInputDialog(new OnedayPanel[]{OnedayPanel.this}, 0);
                case "fix" -> new ScheduleFixDialog(OnedayPanel.this, schedule);
            }
        }
    }
}
