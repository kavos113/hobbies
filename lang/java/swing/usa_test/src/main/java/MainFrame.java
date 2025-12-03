import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.Insets;

public class MainFrame extends JFrame {

    JPanel mainPanel;
    CardLayout layout;

    public MainFrame(){
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        setBounds(100,100, 1200, 800);
        setTitle("USAQuiz");

        setLayout(new BorderLayout());
        initLayout();

        setVisible(true);
    }

    private void initLayout(){
        add(createHeader(), BorderLayout.NORTH);

        mainPanel = new JPanel();
        layout = new CardLayout();
        mainPanel.setLayout(layout);

        mainPanel.add(createHomePanel(), "home");
        mainPanel.add(new QuizPanel(), "quiz");
        layout.show(mainPanel, "home");

        add(mainPanel, BorderLayout.CENTER);
    }

    private JPanel createHeader(){
        JPanel header = new JPanel();
        FlowLayout layout = new FlowLayout();
        layout.setHgap(150);
        header.setLayout(layout);

        JLabel titleLabel = new JLabel("アメリカ州名クイズ");
        titleLabel.setFont(new Font("Ricty Diminished", Font.BOLD, 40));

        JButton homeButton = new JButton("ホームに戻る");
        homeButton.setFont(new Font("Ricty Diminished", Font.PLAIN, 20));
        homeButton.addActionListener(e -> this.layout.show(mainPanel, "home"));

        header.add(titleLabel);
        header.add(homeButton);

        return header;
    }

    private JPanel createHomePanel() {
        JPanel main = new JPanel(new GridLayout(2, 1));

        JLabel mainLabel = new JLabel("アメリカ州名クイズ");
        mainLabel.setFont(new Font("Ricty Diminished", Font.BOLD, 100));
        mainLabel.setHorizontalAlignment(JLabel.CENTER);

        JPanel startPanel = new JPanel();

        JButton startButton = new JButton("開始");
        startButton.setFont(new Font("Ricty Diminished", Font.PLAIN, 40));
        startButton.setHorizontalAlignment(JButton.CENTER);
        startButton.setVerticalAlignment(JButton.CENTER);
        startButton.setMargin(new Insets(30, 60, 30, 60));
        startButton.addActionListener(e -> layout.show(mainPanel, "quiz"));

        startPanel.add(startButton);
        main.add(mainLabel);
        main.add(startPanel);

        return main;
    }
}
