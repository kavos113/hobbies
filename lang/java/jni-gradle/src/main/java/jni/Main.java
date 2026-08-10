package jni;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;

public class Main {
    static {
        try (InputStream input = Main.class.getResourceAsStream("/javamodule.dll")) {
            if (input == null) {
                throw new RuntimeException("failed to get from resource");
            }

            File tmp = File.createTempFile("javamodule-",  ".tmp");
            tmp.deleteOnExit();

            Files.copy(input, tmp.toPath(), StandardCopyOption.REPLACE_EXISTING);
            System.load(tmp.getAbsolutePath());

        } catch (IOException e) {
            throw new RuntimeException("failed to load dll");
        }
    }

    static native void register(JNI jni);

    static void main() {
        System.setOut(new PrintStream(System.out, true, StandardCharsets.UTF_8));

        JNI jni = new JNI();
        register(jni);
    }
}
