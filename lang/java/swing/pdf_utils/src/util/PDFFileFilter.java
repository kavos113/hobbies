package util;

import javax.swing.filechooser.FileFilter;
import java.io.File;

public class PDFFileFilter extends FileFilter {
    @Override
    public boolean accept(File f) {
        if (f.isDirectory()) {
            return true;
        }

        String extension = FileUtils.getExtension(f);
        if (extension != null) {
            return extension.equals(FileUtils.PDF_EXTENSION);
        }
        return false;
    }

    @Override
    public String getDescription() {
        return "PDFファイル";
    }
}
