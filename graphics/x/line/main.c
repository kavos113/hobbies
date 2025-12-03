#include <stdio.h>
#include <unistd.h>
#include <X11/Xlib.h>

int main()
{
    Display *display = XOpenDisplay(NULL);
    int black = BlackPixel(display, 0);
    int white = WhitePixel(display, 0);

    Window window = XCreateSimpleWindow(
        display,
        RootWindow(display, 0),
        100,
        100,
        300,
        300,
        2,
        black,
        white
    );

    GC gc = XCreateGC(display, window, 0, NULL);

    XMapWindow(display, window);
    XFlush(display);

    while (1)
    {
        XDrawLine(
            display,
            window,
            gc,
            10,  // x1
            10,  // y1
            100, // x2
            100  // y2
        );
        XFlush(display);
        sleep(1);
    }
}