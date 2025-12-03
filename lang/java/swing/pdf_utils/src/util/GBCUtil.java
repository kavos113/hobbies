package util;

import java.awt.GridBagConstraints;

public class GBCUtil {

    /**
     * GridBagConstraintsを次のような初期値で生成する
     * <ul>
     *     <li>gridx = 0</li>
     *     <li>gridy = 0</li>
     *     <li>gridwidth = 1</li>
     *     <li>gridheight = 1</li>
     *     <li>weightx = 1.0d</li>
     *     <li>weighty = 1.0d</li>
     *     <li>anchor = CENTER</li>
     *     <li>fill = BOTH</li>
     * </ul>
     * @return gbc
     */
    public static GridBagConstraints createInitialGridBagConstraints() {
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1.0d;
        gbc.weighty = 1.0d;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.fill = GridBagConstraints.BOTH;
        return gbc;
    }
}
