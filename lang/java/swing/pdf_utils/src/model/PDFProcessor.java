package model;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.pdfbox.io.IOUtils;
import org.apache.pdfbox.multipdf.PDFMergerUtility;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;

public class PDFProcessor {

    /**
     * PDFファイルを結合する
     *
     * @param files      結合するPDFファイルの配列
     * @param outputFile 出力ファイル
     */
    public static void mergePDF(ArrayList<File> files, File outputFile) {
        Logger logger = LogManager.getLogger("PDFMerger");

        PDFMergerUtility pdfMergerUtility = new PDFMergerUtility();

        logger.info("Merging PDFs...");

        for (File file : files) {
            try {
                pdfMergerUtility.addSource(file);
            } catch (FileNotFoundException e) {
                logger.error(e);
            }
        }

        pdfMergerUtility.setDestinationFileName(outputFile.getAbsolutePath());

        try {
            pdfMergerUtility.mergeDocuments(IOUtils.createMemoryOnlyStreamCache());
        } catch (IOException e) {
            logger.error(e);
        }
        logger.info("PDFs merged successfully");

    }
}