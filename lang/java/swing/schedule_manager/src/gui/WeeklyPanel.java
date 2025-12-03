package gui;

import gui.weekpanel.EditGoalEvent;
import gui.weekpanel.LeftHeadPanel;
import gui.weekpanel.OnedayPanel;
import main.DailySchedule;
import main.SaveFileMaker;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.border.LineBorder;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;

public class WeeklyPanel extends JPanel {

    private final int weekStartDate;
    private OnedayPanel[] dailyPanels;
    private JPanel mainPanel;
    private JLabel[] goalLabels;

    /**
     *
     * @param weekStartDate write with dayFromNewYork
     */
    public WeeklyPanel(int weekStartDate){
        this.weekStartDate = weekStartDate;

        setLayout(new BorderLayout());
        initLayout();
        initGoalPanel();
        setVisible(true);
    }

    private void initLayout(){
        mainPanel = new JPanel();
        mainPanel.setLayout(new GridLayout(1, 8));

        LeftHeadPanel leftHeadPanel = new LeftHeadPanel();
        mainPanel.add(leftHeadPanel);

        dailyPanels = new OnedayPanel[7];
        for (int i = 0; i < dailyPanels.length; i++) {
            dailyPanels[i] = new OnedayPanel(new DailySchedule(2023, weekStartDate + i, i));
            mainPanel.add(dailyPanels[i]);
        }

        add(mainPanel, BorderLayout.CENTER);
    }

    private void initGoalPanel(){
        JPanel footer = new JPanel();
        GridBagLayout layout = new GridBagLayout();
        footer.setLayout(layout);

        GridBagConstraints gbcFooter = new GridBagConstraints();
        gbcFooter.gridx = 0;
        gbcFooter.gridy = 0;
        gbcFooter.gridwidth = 1;
        gbcFooter.gridheight = 1;
        gbcFooter.weightx = 1.0d;
        gbcFooter.weighty = 1.0d;
        gbcFooter.anchor = GridBagConstraints.WEST;
        gbcFooter.fill = GridBagConstraints.BOTH;
        gbcFooter.insets = new Insets(0, 15, 0, 15);

        goalLabels = new JLabel[6];
        EditGoalEvent goalEvent = new EditGoalEvent();

        goalLabels[0] = new JLabel("東進受講目標:");
        goalLabels[0].setFont(new Font("Meiryo", Font.PLAIN, 18));
        layout.setConstraints(goalLabels[0], gbcFooter);

        goalLabels[1] = new JLabel("東進演習目標:");
        goalLabels[1].setFont(new Font("Meiryo", Font.PLAIN, 18));
        gbcFooter.gridy = 1;
        layout.setConstraints(goalLabels[1], gbcFooter);

        goalLabels[2] = new JLabel("他学習目標　:");
        goalLabels[2].setFont(new Font("Meiryo", Font.PLAIN, 18));
        gbcFooter.gridy = 2;
        layout.setConstraints(goalLabels[2], gbcFooter);

        goalLabels[3] = new JLabel("趣味目標:");
        goalLabels[3].setFont(new Font("Meiryo", Font.PLAIN, 18));
        gbcFooter.gridx = 1;
        gbcFooter.gridy = 0;
        layout.setConstraints(goalLabels[3], gbcFooter);

        goalLabels[4] = new JLabel("読書目標:");
        goalLabels[4].setFont(new Font("Meiryo", Font.PLAIN, 18));
        gbcFooter.gridx = 1;
        gbcFooter.gridy = 1;
        layout.setConstraints(goalLabels[4], gbcFooter);

        goalLabels[5] = new JLabel("他目標　:");
        goalLabels[5].setFont(new Font("Meiryo", Font.PLAIN, 18));
        gbcFooter.gridx = 1;
        gbcFooter.gridy = 2;
        layout.setConstraints(goalLabels[5], gbcFooter);

        JButton editGoal = new JButton("edit\ngoal");
        editGoal.setFont(new Font("Ricty Diminished", Font.BOLD, 22));
        editGoal.addActionListener(goalEvent);
        gbcFooter.gridx = 2;
        gbcFooter.gridy = 0;
        gbcFooter.gridheight = 3;
        gbcFooter.anchor = GridBagConstraints.CENTER;
        gbcFooter.insets = new Insets(5, 5, 5,5 );
        gbcFooter.weightx = 0.3d;
        layout.setConstraints(editGoal, gbcFooter);

        for (JLabel goal : goalLabels) {
            footer.add(goal);
            goalEvent.addLabel(goal);
        }
        footer.add(editGoal);

        add(footer, BorderLayout.SOUTH);
    }

    public void save(){
        String[] goalString = new String[goalLabels.length];
        for (int i = 0; i < goalLabels.length; i++) {
            String[] text = goalLabels[i].getText().split(":", 2);
            goalString[i] = text[1];
        }

        SaveFileMaker.save(dailyPanels, goalString);
    }

    public void setDailyPanels(OnedayPanel[] dailyPanels) {
        for (OnedayPanel panel : this.dailyPanels) {
            mainPanel.remove(panel);
        }

        repaint();

        for (OnedayPanel dailyPanel : dailyPanels) {
            mainPanel.add(dailyPanel);

            //to repaint all components
            dailyPanel.setBorder(new LineBorder(Color.BLACK, 1));
            dailyPanel.setBorder(null);
        }

        this.dailyPanels = dailyPanels;
    }

    public void setGoalLabels(String[] goalTexts){
        if(goalTexts.length != goalLabels.length){
            JOptionPane.showMessageDialog(null, "Error in loading GoalLabels", "An Error Occured", JOptionPane.ERROR_MESSAGE);
            System.out.println("goalTexts.length = " + goalTexts.length);
            System.out.println("goalLabels.length = " + goalLabels.length);
            return;
        }
        for (int i = 0; i < goalLabels.length; i++) {
            goalLabels[i].setText(goalLabels[i].getText() + goalTexts[i]);
        }
    }

    public OnedayPanel[] getDailyPanels() {
        return dailyPanels;
    }
}
