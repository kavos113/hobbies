package test;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;
import java.awt.Dimension;
import java.awt.GridLayout;

public class SwingTest extends JFrame {

    public static void main(String[] args){
        SwingTest test = new SwingTest();

        test.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        test.setVisible(true);
    }

    SwingTest(){
        setTitle("SwingTest");
        setBounds( 10, 10, 300, 200);

        String[] columnNames = {"COUNTRY", "WIN", "LOST", "EVEN"};
        String[][] tabledata = {
                {"日本", "3勝", "0敗", "1分"},
                {"クロアチア", "3勝", "1敗", "0分"},
                {"ブラジル", "1勝", "2敗", "1分"},
                {"オーストラリア", "2勝", "2敗", "0分"}};
        DefaultTableModel tableModel
                = new DefaultTableModel(tabledata, columnNames);

        JTable table = new JTable(tableModel);

        JScrollPane sp = new JScrollPane(table);
        sp.setPreferredSize(new Dimension(250, 70));

        JPanel p = new JPanel();
        GridLayout layout = new GridLayout(2, 2);
        p.setLayout(layout);
        p.add(sp);

        JLabel label1 = new JLabel("hho");
        p.add(label1);

        JButton button = new JButton("remove");
        button.addActionListener(e -> {
            System.out.println("hou");
            p.remove(label1);
            p.repaint();
        });
        p.add(button);

        JButton button1 = new JButton("add");
        button1.addActionListener(e -> {
            System.out.println("houhe");
            p.add(label1);
            p.repaint();
        });
        p.add(button1);

        add(p);


    }

}
