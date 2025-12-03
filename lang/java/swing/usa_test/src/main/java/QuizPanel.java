import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Objects;

public class QuizPanel extends JPanel {

    private States states;
    JPanel answerPanel;
    JLabel answerIcon;
    JTextField answerTextField;
    JLabel imageLabel;
    JLabel answerLabel;

    public QuizPanel(){
        states = StatesList.getRandom();
        initLayout();
    }

    private void initLayout(){
        GridBagLayout layout = new GridBagLayout();
        setLayout(layout);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1.0d;
        gbc.weighty = 1.0d;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.BOTH;

        imageLabel = new JLabel(new ImageIcon(fixImage()));
        imageLabel.setHorizontalAlignment(JLabel.CENTER);
        layout.setConstraints(imageLabel, gbc);



        JPanel replyPanel = createReplyPanel();
        gbc.gridy = 1;
        layout.setConstraints(replyPanel, gbc);

        answerPanel = createAnswerPanel();

        gbc.gridy = 2;
        layout.setConstraints(answerPanel, gbc);


        add(imageLabel);
        add(replyPanel);
        add(answerPanel);

        answerPanel.setVisible(false);
    }

    private JPanel createReplyPanel(){
        JLabel ans = new JLabel("解答");
        ans.setFont(new Font("Ricty Diminished", Font.PLAIN, 20));
        ans.setBounds(100, 0, 120, 40);
        ans.setHorizontalAlignment(JLabel.RIGHT);

        answerTextField = new JTextField();
        answerTextField.setFont(new Font("Ricty Diminished", Font.PLAIN, 20));
        answerTextField.setBounds(230, 0, 400, 40);

        JLabel st = new JLabel("州");
        st.setFont(new Font("Ricty Diminished", Font.PLAIN, 20));
        st.setBounds(640, 0, 120, 40);

        JButton ansButton = new JButton("解答");
        ansButton.setFont(new Font("Ricty Diminished", Font.PLAIN, 20));
        ansButton.setBounds(900, 0, 120, 40);
        ansButton.addActionListener(e -> {
            answerPanel.setVisible(true);
            checkAnswer();
        });

        JPanel replyPanel = new JPanel(null);
        replyPanel.add(ans);
        replyPanel.add(answerTextField);
        replyPanel.add(st);
        replyPanel.add(ansButton);

        return replyPanel;
    }

    private JPanel createAnswerPanel(){
        answerIcon = new JLabel();
        answerIcon.setBounds(50, 0, 40, 40);

        JLabel ans = new JLabel("正解");
        ans.setFont(new Font("Ricty Diminished", Font.PLAIN, 20));
        ans.setBounds(100, 0, 120, 40);
        ans.setHorizontalAlignment(JLabel.RIGHT);

        answerLabel = new JLabel(states.answer());
        answerLabel.setFont(new Font("Ricty Diminished", Font.PLAIN, 20));
        answerLabel.setBounds(230, 0, 400, 40);
        answerLabel.setHorizontalAlignment(JLabel.RIGHT);

        JLabel st = new JLabel("州");
        st.setFont(new Font("Ricty Diminished", Font.PLAIN, 20));
        st.setBounds(640, 0, 60, 40);

        JButton ansButton = new JButton("次へ");
        ansButton.setFont(new Font("Ricty Diminished", Font.PLAIN, 20));
        ansButton.setBounds(900, 0, 120, 40);
        ansButton.addActionListener(e -> refresh());

        JPanel answerPanel = new JPanel(null);
        answerPanel.add(ans);
        answerPanel.add(answerLabel);
        answerPanel.add(st);
        answerPanel.add(ansButton);
        answerPanel.add(answerIcon);

        return answerPanel;
    }

    private BufferedImage fixImage(){
        BufferedImage origin = null;
        try {
            origin = ImageIO.read(new File(states.path().toString()));
        } catch (IOException e) {
            e.printStackTrace();
        }
        BufferedImage image = new BufferedImage(750, 444, BufferedImage.TYPE_3BYTE_BGR);
        image.createGraphics().drawImage(Objects.requireNonNull(origin).getScaledInstance(
                750, 444, Image.SCALE_AREA_AVERAGING), 0, 0, 750, 444, null);

        return image;
    }

    private void checkAnswer(){
        if (answerTextField.getText().equals(states.answer())){
            answerIcon.setIcon(new ImageIcon("imgs\\icon\\maru.png"));
        }else{
            answerIcon.setIcon(new ImageIcon("imgs\\icon\\batsu.png"));
        }
    }

    private void refresh(){
        states = StatesList.getRandom();
        imageLabel.setIcon(new ImageIcon(fixImage()));
        answerLabel.setText(states.answer());
        answerTextField.setText("");

        answerPanel.setVisible(false);
    }

}
