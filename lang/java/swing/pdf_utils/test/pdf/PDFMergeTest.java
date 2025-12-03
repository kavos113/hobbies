package pdf;

import model.PDFMerger;
import model.PDFProcessor;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

public class PDFMergeTest {
    public static void main(String[] args) {
        File pdf1 = new File("pdfs\\1.pdf");
        File pdf2 = new File("pdfs\\2.pdf");

        File[] files = {pdf1, pdf2};
        File outputFile = new File("pdfs\\merged.pdf");
        PDFProcessor.mergePDF(files, outputFile);
    }
}
