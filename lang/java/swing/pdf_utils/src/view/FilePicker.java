package view;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import util.PDFFileFilter;

import javax.swing.JFileChooser;
import java.awt.Component;
import java.awt.Dimension;
import java.io.File;
import java.nio.file.Path;

public class FilePicker {

    private Path documentsPath = Path.of(System.getProperty("user.home"), "Documents");

    private final Logger logger = LogManager.getLogger("FilePicker");

    /**
     * ファイルを選択するダイアログを表示し、選択されたファイルを返す
     * エラーだったらnull
     * @param parent 親コンポーネント
     * @return 選択されたファイル
     */
    public File pickFile(Component parent) {

        JFileChooser fileChooser = new JFileChooser(documentsPath.toString());
        fileChooser.addChoosableFileFilter(new PDFFileFilter());
        fileChooser.setPreferredSize(new Dimension(800, 600));

        logger.info(fileChooser.getBounds());

        int result = fileChooser.showOpenDialog(parent);
        if(result == JFileChooser.APPROVE_OPTION) {
            logger.info("File picked: " + fileChooser.getSelectedFile().getAbsolutePath());
            documentsPath = fileChooser.getCurrentDirectory().toPath();
            return fileChooser.getSelectedFile();
        } else {
            logger.error("No file picked");
            return null;
        }
    }
}
