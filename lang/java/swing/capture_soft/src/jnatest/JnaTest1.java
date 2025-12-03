package jnatest;

import com.sun.jna.Library;
import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.platform.win32.WinDef;
import com.sun.jna.platform.win32.WinGDI;
import com.sun.jna.platform.win32.WinNT;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfByte;
import org.opencv.imgcodecs.Imgcodecs;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;

import static com.sun.jna.platform.win32.GDI32.SRCCOPY;

public class JnaTest1 {

    //def about native library
    static {System.loadLibrary(Core.NATIVE_LIBRARY_NAME);}

    public interface User32 extends Library{
        User32 INSTANCE = Native.load("user32", User32.class);

        WinDef.HDC GetDC(WinDef.HWND hwnd);
        WinDef.HWND GetDesktopWindow();
        WinDef.BOOL GetWindowRect(WinDef.HWND hWnd, WinDef.RECT[] lpRect);
        int ReleaseDC(WinDef.HWND hWnd, WinDef.HDC hdc);
        WinDef.BOOL GetClientRect(WinDef.HWND hWnd, WinDef.RECT rect);
    }

    public interface Gdi32 extends Library{
        Gdi32 INSTANCE = Native.load("gdi32", Gdi32.class);

        WinDef.HDC CreateCompatibleDC(WinDef.HDC hdc);
        WinDef.BOOL BitBlt(WinDef.HDC hdc, int x, int y, int cx, int cy, WinDef.HDC hdcSrc, int x1, int y1, int rop);
        int GetDIBits(WinDef.HDC hdc, WinDef.HBITMAP hbm, int start, int cLines, Memory lpvBits, WinGDI.BITMAPINFO lpbmi, int usage);
        WinDef.HBITMAP CreateDIBSection(WinDef.HDC hdc, WinGDI.BITMAPINFO pbmi, int usage, WinDef.DWORD ppvBits, WinNT.HANDLE hSection, int offset);
        WinDef.HBITMAP SelectObject(WinDef.HDC hdc, WinNT.HANDLE h);
        WinDef.HBITMAP CreateCompatibleBitmap(WinDef.HDC hdc, int cx, int cy);
        WinDef.BOOL DeleteDC(WinDef.HDC hdc);
        WinDef.BOOL DeleteObject(WinDef.HBITMAP ho);
    }

    static WinDef.HWND hwnd;
    static WinDef.RECT bounds;
    static int width, height;
    static WinGDI.BITMAPINFO bmi;

    private static void init(){
        hwnd = User32.INSTANCE.GetDesktopWindow();
        bounds = new WinDef.RECT();

        User32.INSTANCE.GetClientRect(hwnd, bounds);

        width = bounds.right;
        height = bounds.bottom;

        bmi = new WinGDI.BITMAPINFO();
        bmi.bmiHeader.biWidth = width;
        bmi.bmiHeader.biHeight = -height;
        bmi.bmiHeader.biPlanes = 1;
        bmi.bmiHeader.biBitCount = 32;
        bmi.bmiHeader.biCompression = WinGDI.BI_RGB;
    }

    public static void main(String[] args) {
        init();

        //hdcをmatに変換
        //matから出力


        for (int i = 0; i < 10; i++) {
            String path = "output\\cap" + i + ".png";
            File file = new File(path);

            try {
                ImageIO.write(createScreenCapture(), "png", file);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    }

    //using win32 api
    public static BufferedImage createScreenCapture(){

        WinDef.HDC hdcWindow = User32.INSTANCE.GetDC(hwnd);
        WinDef.HDC hdcMemDC = Gdi32.INSTANCE.CreateCompatibleDC(hdcWindow);

        WinDef.HBITMAP hBitmap = Gdi32.INSTANCE.CreateCompatibleBitmap(hdcWindow, width, height);
        WinNT.HANDLE hOld = Gdi32.INSTANCE.SelectObject(hdcMemDC, hBitmap);

        Gdi32.INSTANCE.BitBlt(hdcMemDC, 0, 0, width, height, hdcWindow, 0, 0, SRCCOPY); //律速段階
        long l = System.currentTimeMillis();
        Gdi32.INSTANCE.SelectObject(hdcMemDC, hOld);
        Gdi32.INSTANCE.DeleteDC(hdcMemDC);
        System.out.println(System.currentTimeMillis() - l);

        Memory buffer = new Memory((long) width * height * 4);
        Gdi32.INSTANCE.GetDIBits(hdcWindow, hBitmap, 0, height, buffer, bmi, WinGDI.DIB_RGB_COLORS);
        BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        image.setRGB(0, 0, width, height, buffer.getIntArray(0, width * height), 0, width);

        Gdi32.INSTANCE.DeleteObject(hBitmap);
        User32.INSTANCE.ReleaseDC(hwnd, hdcWindow);

        return image;
    }
}
