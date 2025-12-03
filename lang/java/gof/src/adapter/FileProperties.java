package adapter;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;

public class FileProperties extends Properties implements FileIO {

    public FileProperties() {
        super();
    }

    @Override
    public void readFromFile(String filename) throws IOException {
        FileInputStream fileInputStream = new FileInputStream(filename);
        this.load(fileInputStream);
    }

    @Override
    public void writeToFile(String filename) throws IOException {
        FileOutputStream fileOutputStream = new FileOutputStream(filename);
        this.store(fileOutputStream, "");
    }

    @Override
    public void setValue(String key, String value) {
        this.setProperty(key, value);
    }

    @Override
    public String getValue(String key) {
        return this.getProperty(key);
    }
}
