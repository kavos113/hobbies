package view;

import javax.swing.JFrame;
import java.awt.BorderLayout;

public class MainFrame extends JFrame {
    public MainFrame() {
        super();
        initFrame();
        initLayout();
        this.setVisible(true);
    }

    private void initFrame(){
        this.setSize(1200, 800);
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        this.setTitle("PDF Tools");
    }

    private void initLayout(){
        this.setLayout(new BorderLayout());

        MainPanel mainPanel = new MainPanel();
        this.add(mainPanel, BorderLayout.CENTER);

        HeaderPanel headerPanel = new HeaderPanel("PDF Tools", 32);
        this.add(headerPanel, BorderLayout.NORTH);
    }
}
