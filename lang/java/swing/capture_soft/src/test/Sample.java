package test;

import java.awt.AWTException;
import java.awt.Color;
import java.awt.EventQueue;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Toolkit;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.geom.AffineTransform;
import java.awt.image.BaseMultiResolutionImage;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferInt;
import java.awt.image.DirectColorModel;
import java.awt.image.MultiResolutionImage;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;
import java.awt.peer.RobotPeer;

import sun.awt.AWTPermissions;
import sun.awt.ComponentFactory;
import sun.awt.SunToolkit;
import sun.awt.image.SunWritableRaster;
import sun.java2d.SunGraphicsEnvironment;

import static sun.java2d.SunGraphicsEnvironment.toDeviceSpace;
import static sun.java2d.SunGraphicsEnvironment.toDeviceSpaceAbs;

public class Sample {
    private static final int MAX_DELAY = 60000;
    private RobotPeer peer;
    private boolean isAutoWaitForIdle = false;
    private int autoDelay = 0;
    private static int LEGAL_BUTTON_MASK = 0;

    private DirectColorModel screenCapCM = null;

    public Sample() throws AWTException {
        checkHeadless();
        init(GraphicsEnvironment.getLocalGraphicsEnvironment()
                .getDefaultScreenDevice());
    }

    public Sample(GraphicsDevice screen) throws AWTException {
        checkHeadless();
        checkIsScreenDevice(screen);
        init(screen);
    }

    private void init(GraphicsDevice screen) throws AWTException {
        checkRobotAllowed();
        Toolkit toolkit = Toolkit.getDefaultToolkit();
        if (toolkit instanceof ComponentFactory) {
            peer = ((ComponentFactory)toolkit).createRobot(screen);
        }
        initLegalButtonMask();
    }

    @SuppressWarnings("deprecation")
    private static synchronized void initLegalButtonMask() {
        if (LEGAL_BUTTON_MASK != 0) return;

        int tmpMask = 0;
        if (Toolkit.getDefaultToolkit().areExtraMouseButtonsEnabled()){
            if (Toolkit.getDefaultToolkit() instanceof SunToolkit) {
                final int buttonsNumber = ((SunToolkit)(Toolkit.getDefaultToolkit())).getNumberOfButtons();
                for (int i = 0; i < buttonsNumber; i++){
                    tmpMask |= InputEvent.getMaskForButton(i+1);
                }
            }
        }
        tmpMask |= InputEvent.BUTTON1_MASK|
                InputEvent.BUTTON2_MASK|
                InputEvent.BUTTON3_MASK|
                InputEvent.BUTTON1_DOWN_MASK|
                InputEvent.BUTTON2_DOWN_MASK|
                InputEvent.BUTTON3_DOWN_MASK;
        LEGAL_BUTTON_MASK = tmpMask;
    }

    /* determine if the security policy allows Robot's to be created */
    private static void checkRobotAllowed() {
        @SuppressWarnings("removal")
        SecurityManager security = System.getSecurityManager();
    }

    /**
     * Check for headless state and throw {@code AWTException} if headless.
     */
    private static void checkHeadless() throws AWTException {
        if (GraphicsEnvironment.isHeadless()) {
            throw new AWTException("headless environment");
        }
    }

    /* check if the given device is a screen device */
    private static void checkIsScreenDevice(GraphicsDevice device) {
        if (device == null || device.getType() != GraphicsDevice.TYPE_RASTER_SCREEN) {
            throw new IllegalArgumentException("not a valid screen device");
        }
    }

    /**
     * Moves mouse pointer to given screen coordinates.
     * @param x         X position
     * @param y         Y position
     */
    public synchronized void mouseMove(int x, int y) {
        peer.mouseMove(x, y);
        afterEvent();
    }

    public synchronized void mousePress(int buttons) {
        checkButtonsArgument(buttons);
        peer.mousePress(buttons);
        afterEvent();
    }


    public synchronized void mouseRelease(int buttons) {
        checkButtonsArgument(buttons);
        peer.mouseRelease(buttons);
        afterEvent();
    }

    private static void checkButtonsArgument(int buttons) {
        if ( (buttons|LEGAL_BUTTON_MASK) != LEGAL_BUTTON_MASK ) {
            throw new IllegalArgumentException("Invalid combination of button flags");
        }
    }


    public synchronized void mouseWheel(int wheelAmt) {
        peer.mouseWheel(wheelAmt);
        afterEvent();
    }


    public synchronized void keyPress(int keycode) {
        checkKeycodeArgument(keycode);
        peer.keyPress(keycode);
        afterEvent();
    }


    public synchronized void keyRelease(int keycode) {
        checkKeycodeArgument(keycode);
        peer.keyRelease(keycode);
        afterEvent();
    }

    private static void checkKeycodeArgument(int keycode) {
        // rather than build a big table or switch statement here, we'll
        // just check that the key isn't VK_UNDEFINED and assume that the
        // peer implementations will throw an exception for other bogus
        // values e.g. -1, 999999
        if (keycode == KeyEvent.VK_UNDEFINED) {
            throw new IllegalArgumentException("Invalid key code");
        }
    }


    public synchronized Color getPixelColor(int x, int y) {
        checkScreenCaptureAllowed();
        Point point = peer.useAbsoluteCoordinates() ? toDeviceSpaceAbs(x, y)
                : toDeviceSpace(x, y);
        return new Color(peer.getRGBPixel(point.x, point.y));
    }



    public synchronized BufferedImage createScreenCapture(Rectangle screenRect) {
        return createCompatibleImage(screenRect, false)[0];
    }


    public synchronized MultiResolutionImage
    createMultiResolutionScreenCapture(Rectangle screenRect) {

        return new BaseMultiResolutionImage(
                createCompatibleImage(screenRect, true));
    }

    private synchronized BufferedImage[]
    createCompatibleImage(Rectangle screenRect, boolean isHiDPI) {

        checkScreenCaptureAllowed();

        checkValidRect(screenRect);

        BufferedImage lowResolutionImage;
        BufferedImage highResolutionImage;
        DataBufferInt buffer;
        WritableRaster raster;
        BufferedImage[] imageArray;

        if (screenCapCM == null) {
            /*
             * Fix for 4285201
             * Create a DirectColorModel equivalent to the default RGB ColorModel,
             * except with no Alpha component.
             */

            screenCapCM = new DirectColorModel(24,
                    /* red mask */ 0x00FF0000,
                    /* green mask */ 0x0000FF00,
                    /* blue mask */ 0x000000FF);
        }

        int[] bandmasks = new int[3];
        bandmasks[0] = screenCapCM.getRedMask();
        bandmasks[1] = screenCapCM.getGreenMask();
        bandmasks[2] = screenCapCM.getBlueMask();

        // need to sync the toolkit prior to grabbing the pixels since in some
        // cases rendering to the screen may be delayed
        Toolkit.getDefaultToolkit().sync();

        GraphicsConfiguration gc = GraphicsEnvironment
                .getLocalGraphicsEnvironment()
                .getDefaultScreenDevice().
                getDefaultConfiguration();
        gc = SunGraphicsEnvironment.getGraphicsConfigurationAtPoint(
                gc, screenRect.getCenterX(), screenRect.getCenterY());

        AffineTransform tx = gc.getDefaultTransform();
        double uiScaleX = tx.getScaleX();
        double uiScaleY = tx.getScaleY();
        int[] pixels;

        if (uiScaleX == 1 && uiScaleY == 1) {

            pixels = peer.getRGBPixels(screenRect);
            buffer = new DataBufferInt(pixels, pixels.length);

            bandmasks[0] = screenCapCM.getRedMask();
            bandmasks[1] = screenCapCM.getGreenMask();
            bandmasks[2] = screenCapCM.getBlueMask();

            raster = Raster.createPackedRaster(buffer, screenRect.width,
                    screenRect.height, screenRect.width, bandmasks, null);
            SunWritableRaster.makeTrackable(buffer);

            highResolutionImage = new BufferedImage(screenCapCM, raster,
                    false, null);
            imageArray = new BufferedImage[1];
            imageArray[0] = highResolutionImage;

        } else {
            Rectangle scaledRect;
            if (peer.useAbsoluteCoordinates()) {
                scaledRect = toDeviceSpaceAbs(gc, screenRect.x,
                        screenRect.y, screenRect.width, screenRect.height);
            } else {
                scaledRect = toDeviceSpace(gc, screenRect.x,
                        screenRect.y, screenRect.width, screenRect.height);
            }
            // HighResolutionImage
            pixels = peer.getRGBPixels(scaledRect);
            buffer = new DataBufferInt(pixels, pixels.length);
            raster = Raster.createPackedRaster(buffer, scaledRect.width,
                    scaledRect.height, scaledRect.width, bandmasks, null);
            SunWritableRaster.makeTrackable(buffer);

            highResolutionImage = new BufferedImage(screenCapCM, raster,
                    false, null);


            // LowResolutionImage
            lowResolutionImage = new BufferedImage(screenRect.width,
                    screenRect.height, highResolutionImage.getType());
            Graphics2D g = lowResolutionImage.createGraphics();
            g.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
                    RenderingHints.VALUE_INTERPOLATION_BILINEAR);
            g.setRenderingHint(RenderingHints.KEY_RENDERING,
                    RenderingHints.VALUE_RENDER_QUALITY);
            g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                    RenderingHints.VALUE_ANTIALIAS_ON);
            g.drawImage(highResolutionImage, 0, 0,
                    screenRect.width, screenRect.height,
                    0, 0, scaledRect.width, scaledRect.height, null);
            g.dispose();

            if(!isHiDPI) {
                imageArray = new BufferedImage[1];
                imageArray[0] = lowResolutionImage;
            } else {
                imageArray = new BufferedImage[2];
                imageArray[0] = lowResolutionImage;
                imageArray[1] = highResolutionImage;
            }

        }

        return imageArray;
    }

    private static void checkValidRect(Rectangle rect) {
        if (rect.width <= 0 || rect.height <= 0) {
            throw new IllegalArgumentException("Rectangle width and height must be > 0");
        }
    }

    private static void checkScreenCaptureAllowed() {
        @SuppressWarnings("removal")
        SecurityManager security = System.getSecurityManager();
        if (security != null) {
            security.checkPermission(AWTPermissions.READ_DISPLAY_PIXELS_PERMISSION);
        }
    }

    /*
     * Called after an event is generated
     */
    private void afterEvent() {
        autoWaitForIdle();
        autoDelay();
    }

    /**
     * Returns whether this Robot automatically invokes {@code waitForIdle}
     * after generating an event.
     * @return Whether {@code waitForIdle} is automatically called
     */
    public synchronized boolean isAutoWaitForIdle() {
        return isAutoWaitForIdle;
    }

    /**
     * Sets whether this Robot automatically invokes {@code waitForIdle}
     * after generating an event.
     * @param   isOn    Whether {@code waitForIdle} is automatically invoked
     */
    public synchronized void setAutoWaitForIdle(boolean isOn) {
        isAutoWaitForIdle = isOn;
    }

    /*
     * Calls waitForIdle after every event if so desired.
     */
    private void autoWaitForIdle() {
        if (isAutoWaitForIdle) {
            waitForIdle();
        }
    }

    /**
     * Returns the number of milliseconds this Robot sleeps after generating an event.
     *
     * @return the delay duration in milliseconds
     */
    public synchronized int getAutoDelay() {
        return autoDelay;
    }

    /**
     * Sets the number of milliseconds this Robot sleeps after generating an event.
     *
     * @param  ms the delay duration in milliseconds
     * @throws IllegalArgumentException If {@code ms}
     *         is not between 0 and 60,000 milliseconds inclusive
     */
    public synchronized void setAutoDelay(int ms) {
        checkDelayArgument(ms);
        autoDelay = ms;
    }

    /*
     * Automatically sleeps for the specified interval after event generated.
     */
    private void autoDelay() {
        delay(autoDelay);
    }

    /**
     * Sleeps for the specified time.
     * <p>
     * If the invoking thread is interrupted while waiting, then it will return
     * immediately with the interrupt status set. If the interrupted status is
     * already set, this method returns immediately with the interrupt status
     * set.
     *
     * @param  ms time to sleep in milliseconds
     * @throws IllegalArgumentException if {@code ms} is not between {@code 0}
     *         and {@code 60,000} milliseconds inclusive
     */
    public void delay(int ms) {
        checkDelayArgument(ms);
        Thread thread = Thread.currentThread();
        if (!thread.isInterrupted()) {
            try {
                Thread.sleep(ms);
            } catch (final InterruptedException ignored) {
                thread.interrupt(); // Preserve interrupt status
            }
        }
    }

    private static void checkDelayArgument(int ms) {
        if (ms < 0 || ms > MAX_DELAY) {
            throw new IllegalArgumentException("Delay must be to 0 to 60,000ms");
        }
    }

    /**
     * Waits until all events currently on the event queue have been processed.
     * @throws  IllegalThreadStateException if called on the AWT event dispatching thread
     */
    public synchronized void waitForIdle() {
        checkNotDispatchThread();
        SunToolkit.flushPendingEvents();
        ((SunToolkit) Toolkit.getDefaultToolkit()).realSync();
    }

    private static void checkNotDispatchThread() {
        if (EventQueue.isDispatchThread()) {
            throw new IllegalThreadStateException("Cannot call method from the event dispatcher thread");
        }
    }

    /**
     * Returns a string representation of this Robot.
     *
     * @return  the string representation.
     */
    @Override
    public synchronized String toString() {
        String params = "autoDelay = "+getAutoDelay()+", "+"autoWaitForIdle = "+isAutoWaitForIdle();
        return getClass().getName() + "[ " + params + " ]";
    }
}
