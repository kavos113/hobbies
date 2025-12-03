package view;

import model.FileStock;
import util.GBCUtil;

import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class WorkspacePanel extends JPanel {

    private JList<String> processFilesList;
    private JList<String> allFilesList;
    private DefaultListModel<String> processFilesModel;
    private DefaultListModel<String> allFilesModel;

    private FileStock processFileStock;
    private FileStock allFileStock;


    public WorkspacePanel() {
        super();
        initLayout();
    }

    private void initLayout(){
        this.setLayout(new BorderLayout());
        this.add(createWorkspaceListPanel(), BorderLayout.CENTER);

        this.add(createWorkspaceHeader(), BorderLayout.NORTH);
    }

    private JPanel createWorkspaceListPanel(){
        JPanel workspaceListPanel = new JPanel();
        GridBagLayout layout = new GridBagLayout();
        workspaceListPanel.setLayout(layout);

        GridBagConstraints gbc = GBCUtil.createInitialGridBagConstraints();

        JLabel processFilesLabel = new JLabel("Process Files");
        processFilesLabel.setFont(new Font("Migu 1P", Font.PLAIN, 14));
        gbc.weighty = 0;
        layout.setConstraints(processFilesLabel, gbc);

        JLabel allFilesLabel = new JLabel("All Files");
        allFilesLabel.setFont(new Font("Migu 1P", Font.PLAIN, 14));
        gbc.gridy += 2;
        layout.setConstraints(allFilesLabel, gbc);

        gbc.gridy--;
        gbc.weighty = 1;
        processFilesModel = new DefaultListModel<>();
        processFilesList = new JList<>(processFilesModel);
        processFileStock = new FileStock();
        JScrollPane processFilesScrollPane = new JScrollPane();
        processFilesScrollPane.setViewportView(processFilesList);
        layout.setConstraints(processFilesScrollPane, gbc);

        gbc.gridy += 2;
        allFilesModel = new DefaultListModel<>();
        allFilesList = new JList<>(allFilesModel);
        allFileStock = new FileStock();
        JScrollPane allFilesScrollPane = new JScrollPane();
        allFilesScrollPane.setViewportView(allFilesList);
        layout.setConstraints(allFilesScrollPane, gbc);

        workspaceListPanel.add(processFilesLabel);
        workspaceListPanel.add(processFilesScrollPane);
        workspaceListPanel.add(allFilesLabel);
        workspaceListPanel.add(allFilesScrollPane);
        return workspaceListPanel;
    }

    private HeaderPanel createWorkspaceHeader(){
        HeaderPanel workspaceHeaderPanel = new HeaderPanel("Workspace", 16);

        JButton addFilesButton = createWorkspaceButton("＋", new Color(0, 128, 0), "addFiles");
        JButton removeFilesButton = createWorkspaceButton("－", Color.BLUE, "removeFiles");
        JButton clearFilesButton = createWorkspaceButton("×", Color.RED, "clearFiles");
        JButton toProcessButton = createWorkspaceButton("↑", Color.BLACK, "toProcess");
        JButton toAllButton = createWorkspaceButton("↓", Color.BLACK, "toAll");

        workspaceHeaderPanel.add(addFilesButton);
        workspaceHeaderPanel.add(removeFilesButton);
        workspaceHeaderPanel.add(clearFilesButton);
        workspaceHeaderPanel.add(toProcessButton);
        workspaceHeaderPanel.add(toAllButton);

        return workspaceHeaderPanel;
    }

    private JButton createWorkspaceButton(String text, Color color, String actionCommand){
        JButton button = new JButton(text);

        button.setFont(new Font("Migu 1M", Font.BOLD, 16));
        button.setMargin(new Insets(0, 0, 0, 0));
        button.setForeground(color);
        button.setActionCommand(actionCommand);
        button.addActionListener(new fileIOEvent());

        return button;
    }

    public ArrayList<File> getProcessFiles(){
        return processFileStock.getFiles();
    }

    public void addFile(File file){
        allFileStock.addFile(file);
        allFilesModel.addElement(file.getName());
        processFileStock.addFile(file);
        processFilesModel.addElement(file.getName());
    }

    public void addFiles(ArrayList<File> files){
        for (File file : files){
            addFile(file);
        }
    }

    public void deleteFileFromProcess(File file){
        processFilesModel.removeElement(file.getName());
        processFileStock.removeFile(file);
    }

    public void deleteFile(File file){
        allFilesModel.removeElement(file.getName());
        allFileStock.removeFile(file);
        deleteFileFromProcess(file);
    }

    public void deleteFilesFromProcess(ArrayList<File> files){
        for (File file : files){
            deleteFileFromProcess(file);
        }
    }

    public void deleteFiles(ArrayList<File> files){
        for (File file : files){
            deleteFile(file);
        }
    }

    private class fileIOEvent implements ActionListener {

        FilePicker filePicker = new FilePicker();

        @Override
        public void actionPerformed(ActionEvent e) {
            String command = e.getActionCommand();
            switch (command){
                case "addFiles":
                    addFiles();
                    break;
                case "removeFiles":
                    removeFiles();
                    break;
                case "clearFiles":
                    clearFiles();
                    break;
                case "toProcess":
                    toProcess();
                    break;
                case "toAll":
                    toAll();
                    break;
            }
        }

        private void toAll() {
            List<String> selectedFilesFromProcess = processFilesList.getSelectedValuesList();
            for (String selectedFile : selectedFilesFromProcess){
                processFileStock.removeFile(selectedFile);
                processFilesModel.removeElement(selectedFile);
            }
        }

        private void toProcess() {
            List<String> selectedFilesFromAll = allFilesList.getSelectedValuesList();
            for (String selectedFile : selectedFilesFromAll){
                processFileStock.addFile(allFileStock.getFile(selectedFile));
                processFilesModel.addElement(selectedFile);
            }
        }

        private void clearFiles() {
            allFilesModel.clear();
            processFilesModel.clear();
            allFileStock.clear();
            processFileStock.clear();
        }

        private void removeFiles() {
            List<String> selectedFilesFromAll = allFilesList.getSelectedValuesList();
            List<String> selectedFilesFromProcess = processFilesList.getSelectedValuesList();

            for (String selectedFile : selectedFilesFromAll){
                allFilesModel.removeElement(selectedFile);
                allFileStock.removeFile(selectedFile);
                if(processFilesModel.contains(selectedFile)){
                    processFilesModel.removeElement(selectedFile);
                    processFileStock.removeFile(selectedFile);
                }
            }

            for (String selectedFile : selectedFilesFromProcess){
                processFilesModel.removeElement(selectedFile);
                processFileStock.removeFile(selectedFile);
                allFilesModel.removeElement(selectedFile);
                allFileStock.removeFile(selectedFile);
            }
        }

        private void addFiles(){
            File newFile = filePicker.pickFile(WorkspacePanel.this);
            if (newFile == null){
                return;
            }
            allFileStock.addFile(newFile);
            allFilesModel.addElement(newFile.getName());
            processFileStock.addFile(newFile);
            processFilesModel.addElement(newFile.getName());
        }
    }
}
