package gui;

import gui.weekpanel.OnedayPanel;
import gui.weekpanel.dialogs.RepeatScheduleInputDialog;
import main.SaveFileLoader;
import main.ScreenPrinter;
import utils.LoadProperties;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.stream.Stream;

public class MainFrame extends JFrame {

    private final HashMap<Integer, WeeklyPanel> weeklyPanelHashMap = new HashMap<>();
    private JLabel statusLabel;
    private JPanel mainPanel;
    private CardLayout layout;
    private int nowFirstDay = 49;

    public MainFrame(){
        initFrame();
        initLayout();

        setVisible(true);

        loadAll();
    }

    private void initFrame(){
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        setBounds(200, 50, 1400, 980);
        setTitle("ScheduleManager");
        //setResizable(false);
    }

    private void initLayout(){
        setLayout(new BorderLayout());

        initMainPanel();
        initHeaderPanel();
    }

    private void initMainPanel(){
        mainPanel = new JPanel();
        layout = new CardLayout();
        mainPanel.setLayout(layout);

        WeeklyPanel weeklyPanel = new WeeklyPanel(nowFirstDay);
        weeklyPanelHashMap.put(nowFirstDay, weeklyPanel);
        mainPanel.add(nowFirstDay + "weekly", weeklyPanel);

        add(mainPanel, BorderLayout.CENTER);
    }

    private void initHeaderPanel(){
        JPanel header = new JPanel();
        header.setLayout(new FlowLayout(FlowLayout.LEFT, 15, 5));

        JLabel title = new JLabel("Schedule Manager");
        title.setFont(new Font("Meiryo UI", Font.BOLD, 50));
        header.add(title);

        JButton saveButton = new JButton("Save");
        saveButton.setFont(new Font("Ricty Diminished", Font.BOLD, 20));
        saveButton.addActionListener(new saveAndLoad());
        saveButton.setActionCommand("save");
        header.add(saveButton);

        JButton loadButton = new JButton("Load");
        loadButton.setFont(new Font("Ricty Diminished", Font.BOLD, 20));
        loadButton.addActionListener(new saveAndLoad());
        loadButton.setActionCommand("load");
        header.add(loadButton);

        JButton prevButton = new JButton("Prev Week");
        prevButton.setFont(new Font("Ricty Diminished", Font.BOLD, 20));
        prevButton.addActionListener(new nextAndPrev());
        prevButton.setActionCommand("prev");
        header.add(prevButton);

        JButton nextButton = new JButton("Next Week");
        nextButton.setFont(new Font("Ricty Diminished", Font.BOLD, 20));
        nextButton.addActionListener(new nextAndPrev());
        nextButton.setActionCommand("next");
        header.add(nextButton);

        JButton bunchButton = new JButton("Repeat Schedule");
        bunchButton.setFont(new Font("Ricty Diminished", Font.BOLD, 20));
        bunchButton.addActionListener(new repeatSchedule());
        header.add(bunchButton);

        statusLabel = new JLabel();
        statusLabel.setFont(new Font("Ricty Diminished", Font.BOLD, 20));
        header.add(statusLabel);


        add(header, BorderLayout.NORTH);
    }

    private void showPanel(){
        if (weeklyPanelHashMap.getOrDefault(nowFirstDay, null) == null){
            WeeklyPanel newPanel = new WeeklyPanel(nowFirstDay);
            weeklyPanelHashMap.put(nowFirstDay, newPanel);

            mainPanel.add(nowFirstDay + "weekly", newPanel);
        }

        layout.show(mainPanel, nowFirstDay + "weekly");
    }

    private void loadAll(){
        try(Stream<Path> stream = Files.list(Paths.get(".\\saves"))){
            stream.forEach(p -> load(p.toFile()));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void load(File loadFile){
        SaveFileLoader loader = new SaveFileLoader();
        LoadProperties properties = loader.load(loadFile);

        WeeklyPanel panel = new WeeklyPanel(properties.startDays());
        panel.setDailyPanels(properties.panels());
        panel.setGoalLabels(properties.goals());
        weeklyPanelHashMap.put(properties.startDays(), panel);
        mainPanel.add(properties.startDays() + "weekly", panel);

        nowFirstDay = properties.startDays();
        showPanel();
    }

    class saveAndLoad implements ActionListener {


        private static final String captureFolderPath = "scman";

        @Override
        public void actionPerformed(ActionEvent e) {
            String cmd = e.getActionCommand();

            if (cmd.equals("save")){
                weeklyPanelHashMap.get(nowFirstDay).save();

                DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HH:mm:ss");
                String time = formatter.format(LocalDateTime.now());
                statusLabel.setText("saved at " + time);

                ScreenPrinter.printScreen(captureFolderPath, getBounds());
            }else if (cmd.equals("load")){
                JFileChooser fileChooser = new JFileChooser(Paths.get("").toAbsolutePath().toString());
                int selectStatus = fileChooser.showOpenDialog(MainFrame.this);

                if (selectStatus == JFileChooser.APPROVE_OPTION){
                    load(fileChooser.getSelectedFile());
                }
            }
        }
    }

    class nextAndPrev implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            String cmd = e.getActionCommand();

            int moveDays = 7;
            if (cmd.equals("prev")){
                moveDays =  -7;
            }

            nowFirstDay += moveDays;

            showPanel();
        }
    }

    class repeatSchedule implements ActionListener{

        @Override
        public void actionPerformed(ActionEvent e) {
            OnedayPanel[] currentWeek = weeklyPanelHashMap.get(nowFirstDay).getDailyPanels();
            new RepeatScheduleInputDialog(currentWeek, 0);
        }
    }

    /*@Override
    public void paint(Graphics g) {
        super.paint(g);

        System.out.println("getHeight() = " + getHeight());
    }*/
}
