package gui.weekpanel.dialogs;

import gui.weekpanel.EditGoalEvent;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;
import java.awt.BorderLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class GoalEditDialog extends JDialog {

    private final EditGoalEvent event;
    private final String[][] texts;
    private JTable table;

    public GoalEditDialog(EditGoalEvent event, String[][] texts) {
        this.event = event;
        this.texts = texts;

        setTitle("Goal Edit");
        setBounds(1150, 150, 500, 350);
        setResizable(false);
        initLayout();

        setVisible(true);
    }

    private void initLayout(){
        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());
        mainPanel.setBorder(BorderFactory.createEmptyBorder(30, 30, 30, 30));

        String[] tableColumns = {"種類", "目標"};

        DefaultTableModel tableModel = new DefaultTableModel(texts, tableColumns);

        table = new JTable(tableModel);
        table.setFont(new Font("Meiryo", Font.PLAIN, 20));
        table.setRowHeight(30);
        JScrollPane scrollPane = new JScrollPane(table);

        mainPanel.add(scrollPane, BorderLayout.CENTER);


        JButton finishButton = new JButton("Finish Editing");
        finishButton.setFont(new Font("Ricty Diminished", Font.BOLD, 20));
        finishButton.addActionListener(new finishEdit());
        mainPanel.add(finishButton, BorderLayout.SOUTH);

        add(mainPanel);
    }

    class finishEdit implements ActionListener{

        @Override
        public void actionPerformed(ActionEvent e) {
            String[] backData = new String[texts.length];
            for (int i = 0; i < backData.length; i++) {
                backData[i] = table.getValueAt(i,0) + ":" + table.getValueAt(i, 1);
            }

            event.updateTexts(backData);
            setVisible(false);
            dispose();
        }
    }
}
