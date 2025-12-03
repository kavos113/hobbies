package log;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class LogTest {
    public static void main(String[] args) {
        Logger logger = LogManager.getLogger("LoggerTest");
        logger.info("hello, log4j2");
        logger.error("Hello, error");
        logger.error("nyachi", new NullPointerException("adaosdoij"));
    }
}
