package test;

import main.SaveFileLoader;

import java.io.File;

public class SaveTest {

    public static void main(String[] args) {
        SaveFileLoader loader = new SaveFileLoader();
        loader.load(new File("saves\\From2023_1_29.csv"));
    }

}
