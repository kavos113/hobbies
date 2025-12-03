package util;

import java.io.File;

public class FileUtils {

    public final static String PDF_EXTENSION = "pdf";

    /**
     * ファイルの拡張子を取得する
     * @param file ファイル
     * @return 拡張子
     */
    public static String getExtension(File file) {
        String fileName = file.getName();
        int point = fileName.lastIndexOf(".");
        if (point != -1) {
            return fileName.substring(point + 1);
        }
        return null;
    }
}
