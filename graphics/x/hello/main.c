#include <stdio.h>
#include <X11/Xlib.h>

int main() {
    Display* display = XOpenDisplay(NULL); // connect to the X server
    int black = BlackPixel(display, 0);
    int white = WhitePixel(display, 0);

    Window window = XCreateSimpleWindow(
        display,
        RootWindow(display, 0), // parent window
        100, // x
        100, // y
        300, // width
        300, // height
        2,   // border width
        black, // border color
        white  // background color
    );

    XMapWindow(display, window); // make the window visible
    XFlush(display); // flush the output buffer

    getchar();

    return 0;
}