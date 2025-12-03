package gui.weekpanel.dialogs;

import gui.weekpanel.OnedayPanel;
import main.DailySchedule;
import main.Schedule;
import utils.TimeConvert;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;


public class ScheduleFixDialog extends JDialog {

    private final OnedayPanel parentPanel;
    private final DailySchedule schedule;
    private JTable schedulesTable;

    /**
     * 単予定専用
     */
    public ScheduleFixDialog(OnedayPanel parentPanel, DailySchedule schedule) {
        this.parentPanel = parentPanel;
        this.schedule = schedule;

        setTitle("Schedule Input");
        setBounds(1150, 150, 750, 500);
        setResizable(false);
        initLayout();

        setVisible(true);
    }

    private void initLayout(){
        setLayout(new BorderLayout());

        add(initFoot(), BorderLayout.SOUTH);
        add(initTable(), BorderLayout.CENTER);

        //Head
        JLabel dateLabel = new JLabel();
        dateLabel.setText(schedule.getMonth() + "月" + schedule.getDay() + "日");
        dateLabel.setFont(new Font("Ricty Diminished", Font.BOLD, 35));
        dateLabel.setHorizontalAlignment(JLabel.LEFT);
        add(dateLabel, BorderLayout.NORTH);
    }

    private JPanel initTable(){
        String[] tableColumn = {"開始時刻", "終了時刻", "内容", "種類"};

        ArrayList<Schedule> schedules = schedule.getScheduleList();
        String[][] scheduleItems = new String[schedules.size()][tableColumn.length];
        for (int i = 0; i < scheduleItems.length; i++) {
            Schedule eachSchedule = schedules.get(i);

            scheduleItems[i][0] = TimeConvert.TimeToString(eachSchedule.startTime());
            scheduleItems[i][1] = TimeConvert.TimeToString(eachSchedule.endTime());
            scheduleItems[i][2] = eachSchedule.content();
            scheduleItems[i][3] = eachSchedule.type().toString();
        }

        DefaultTableModel tableModel = new DefaultTableModel(scheduleItems, tableColumn){
            @Override
            public boolean isCellEditable(int row, int column) {
                return false;
            }
        };

        JPanel tablePanel = new JPanel(new FlowLayout());

        schedulesTable = new JTable(tableModel);
        schedulesTable.setFont(new Font("Meiryo UI", Font.PLAIN, 15));

        JScrollPane scrollPane = new JScrollPane(schedulesTable);
        tablePanel.add(scrollPane);

        return tablePanel;
    }

    private JPanel initFoot(){
        JPanel footPanel = new JPanel(null);
        footPanel.setPreferredSize(new Dimension(750, 50));

        JButton editButton = new JButton("編集");
        editButton.setBounds(500, 10, 80, 30);
        editButton.setFont(new Font("Ricty Diminished", Font.BOLD, 15));
        editButton.addActionListener(new pressButton());
        editButton.setActionCommand("edit");
        footPanel.add(editButton);

        JButton closeButton = new JButton("とじる");
        closeButton.setBounds(600, 10, 100, 30);
        closeButton.setFont(new Font("Ricty Diminished", Font.BOLD, 15));
        closeButton.addActionListener(new pressButton());
        closeButton.setActionCommand("close");
        footPanel.add(closeButton);

        return footPanel;
    }

    class pressButton implements ActionListener{

        @Override
        public void actionPerformed(ActionEvent e) {
            String cmd = e.getActionCommand();
            if (cmd.equals("edit")) {
                int index = schedulesTable.getSelectedRow();
                new ScheduleInputDialog(new OnedayPanel[]{parentPanel}, schedule.getScheduleList().get(index), index);
            } else if (cmd.equals("close")) {
                setVisible(false);
                dispose();
            }
        }
    }
}
