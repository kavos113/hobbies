package main;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import java.awt.Font;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.Objects;


public class MainFrame extends JFrame implements ActionListener {

    JButton startButton, stopButton, folderChooseButton, rectButton, okButton, allrectButton;
    JFileChooser folderChooser;
    JTextField pathField;
    RectFrame rectFrame;
    Capture capture;

    public MainFrame() {
        capture = new Capture();

        setBounds(100, 100, 400, 400);
        setDefaultCloseOperation(EXIT_ON_CLOSE);

        JPanel panel = new JPanel();
        panel.setLayout(null);
        panel.setBounds(0, 0, 400, 400);
        add(panel);

        JLabel titleLabel = new JLabel("画面収録装置");
        titleLabel.setBounds(0, 30, 400, 50);
        titleLabel.setHorizontalAlignment(JLabel.CENTER);
        titleLabel.setFont(new Font("メイリオ", Font.BOLD, 50));

        pathField = new JTextField("");
        pathField.setBounds(30, 150, 250, 30);

        startButton = new JButton("START");
        startButton.setBounds(50, 300, 100, 50);
        startButton.addActionListener(this);
        startButton.setActionCommand("start");

        stopButton = new JButton("STOP");
        stopButton.setBounds(250, 300, 100, 50);
        stopButton.addActionListener(this);
        stopButton.setActionCommand("stop");
        stopButton.setEnabled(false);

        rectButton = new JButton("範囲選択");
        rectButton.setBounds(140, 200, 100, 40);
        rectButton.addActionListener(this);
        rectButton.setActionCommand("rect");

        allrectButton = new JButton("範囲:全画面");
        allrectButton.setBounds(10, 200, 120, 40);
        allrectButton.addActionListener(this);
        allrectButton.setActionCommand("allrect");

        okButton = new JButton("範囲選択完了");
        okButton.setBounds(260, 200, 120, 40);
        okButton.addActionListener(this);
        okButton.setActionCommand("ok");
        okButton.setEnabled(false);

        folderChooseButton = new JButton("参照");
        folderChooseButton.setBounds(290, 150, 70, 30);
        folderChooseButton.addActionListener(this);
        folderChooseButton.setActionCommand("folder");

        JLabel folderlLabel = new JLabel("保存先フォルダ");
        folderlLabel.setBounds(30, 120, 200, 30);

        panel.add(titleLabel);
        panel.add(folderlLabel);
        panel.add(pathField);
        panel.add(folderChooseButton);
        panel.add(stopButton);
        panel.add(startButton);
        panel.add(rectButton);
        panel.add(okButton);
        panel.add(allrectButton);

        folderChooser = new JFileChooser("C:\\Users");
        folderChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

        rectFrame = new RectFrame();

        setVisible(true);
    }

    public static void main(String[] args) {
        JFrame.setDefaultLookAndFeelDecorated(true);
        new MainFrame();
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();

        switch(command) {
            case "start":
                if (pathField.getText().equals("")){
                    JOptionPane.showMessageDialog(this, "保存先を入力してください");
                    break;
                }
                capture.setFoldeerPath(pathField.getText() + "\\");

                capture.startCapture();
                startButton.setEnabled(false);
                stopButton.setEnabled(true);
                pathField.setEditable(false);

                break;
            case "stop":
                startButton.setEnabled(true);
                stopButton.setEnabled(false);
                capture.stopCapture();
                break;
            case "folder":
                int mode = folderChooser.showOpenDialog(this);

                switch(mode) {
                    case JFileChooser.APPROVE_OPTION:
                        File file = folderChooser.getSelectedFile();
                        pathField.setText(file.getPath());
                }

                break;
            case "rect":
                rectFrame.setVisible(true);
                okButton.setEnabled(true);
                rectButton.setEnabled(false);
                break;
            case "ok":
                okButton.setEnabled(false);
                rectButton.setEnabled(true);

                capture.setCaptureRange(rectFrame.getBounds());

                break;
            case "allrect":
                capture.setCaptureRange(new Rectangle(Toolkit.getDefaultToolkit().getScreenSize()));
                rectFrame.setVisible(false);
                break;
        }
    }
}
