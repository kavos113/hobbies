package gui.weekpanel;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.LineBorder;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridLayout;

public class LeftHeadPanel extends JPanel {

    public LeftHeadPanel(){
        initLayout();
    }

    private void initLayout(){
        setLayout(new BorderLayout());

        initHeadLayout();
        initFootLayout();

        ScheduleIndex indexPanel = new ScheduleIndex();
        indexPanel.setBorder(new LineBorder(new Color(0, 0, 0)));
        add(indexPanel, BorderLayout.CENTER);
    }

    private void initHeadLayout(){
        //panelにしたのはonedaypanelのヘッダと高さを一致させるため
        JPanel edgeLabelPanel = new JPanel();
        edgeLabelPanel.setLayout(new FlowLayout());
        edgeLabelPanel.setBorder(new LineBorder(new Color(0, 0, 0)));
        add(edgeLabelPanel, BorderLayout.NORTH);

        JLabel edgeLabel = new JLabel("予定");
        edgeLabel.setFont(new Font("Ricty Diminished", Font.BOLD, 35));
        edgeLabel.setHorizontalAlignment(JLabel.CENTER);
        edgeLabelPanel.add(edgeLabel);
    }

    private void initFootLayout(){
        JPanel footPanel = new JPanel();

        GridLayout layout = new GridLayout(2, 2);
        layout.setHgap(2);
        layout.setVgap(2);

        footPanel.setLayout(layout);
        footPanel.setPreferredSize(new Dimension(100, 50));
        add(footPanel, BorderLayout.SOUTH);
    }


}
