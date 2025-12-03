package view;

import model.FileStock;
import util.GBCUtil;

import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

public class MainPanel extends JPanel {
    public MainPanel() {
        super();
        initLayout();
    }

    private void initLayout(){
        FileStock processFileStock = new FileStock();
        FileStock allFileStock = new FileStock();

        GridBagLayout layout = new GridBagLayout();
        this.setLayout(layout);

        final int PADDING_VERTICAL = 40;
        final int PADDING_HORIZONTAL = 20;

        GridBagConstraints gbc = GBCUtil.createInitialGridBagConstraints();
        gbc.insets = new Insets(PADDING_VERTICAL, PADDING_HORIZONTAL, PADDING_VERTICAL, PADDING_HORIZONTAL);

        WorkspacePanel workspacePanel = new WorkspacePanel();
        ButtonPanel buttonPanel = new ButtonPanel(workspacePanel);
        layout.setConstraints(buttonPanel, gbc);

        gbc.gridx++;
        gbc.weightx *= 3;
        layout.setConstraints(workspacePanel, gbc);

        ViewerPanel viewerPanel = new ViewerPanel();
        gbc.gridx++;
        layout.setConstraints(viewerPanel, gbc);

        this.add(buttonPanel);
        this.add(workspacePanel);
        this.add(viewerPanel);
    }
}
