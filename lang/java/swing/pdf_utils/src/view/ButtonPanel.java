package view;

import model.FileStock;
import model.PDFProcessor;
import view.components.ProcessButton;

import javax.swing.JPanel;
import java.awt.Color;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;

public class ButtonPanel extends JPanel {

    private final WorkspacePanel workspacePanel;

    public ButtonPanel(WorkspacePanel workspacePanel) {
        super();
        initLayout();
        this.workspacePanel = workspacePanel;
    }

    private void initLayout(){
        GridLayout layout = new GridLayout(4, 1);
        layout.setVgap(30);
        this.setLayout(layout);

        ButtonEvent buttonListener = new ButtonEvent();

        ProcessButton mergeButton = new ProcessButton("Merge", "Merge", buttonListener);
        ProcessButton splitButton = new ProcessButton("Split", "Split", buttonListener);
        ProcessButton pictureButton = new ProcessButton("Picture", "Picture", buttonListener);
        ProcessButton viewButton = new ProcessButton("View", "View", buttonListener);

        this.add(mergeButton);
        this.add(splitButton);
        this.add(pictureButton);
        this.add(viewButton);
    }

    private class ButtonEvent implements ActionListener {

        FileSaver fileSaver = new FileSaver();

        @Override
        public void actionPerformed(ActionEvent e) {
            String cmd = e.getActionCommand();
            switch (cmd){
                case "Merge":
                    merge();
                    break;
                case "Split":
                    split();
                    break;
                case "Picture":
                    picture();
                    break;
                case "View":
                    view();
                    break;
            }
        }

        private void merge(){
            ArrayList<File> processFiles = workspacePanel.getProcessFiles();
            File outputFile = fileSaver.saveFile(ButtonPanel.this);

            PDFProcessor.mergePDF(processFiles, outputFile);

            workspacePanel.deleteFilesFromProcess(processFiles);
            workspacePanel.addFile(outputFile);
        }

        private void split(){
            System.out.println("Split");
        }

        private void picture(){
            System.out.println("Picture");
        }

        private void view(){
            System.out.println("View");
        }
    }
}
