#include <stdio.h>
#include <X11/Xlib.h>

int main() {
    Display* display = XOpenDisplay(NULL);
    if (display == NULL) {
        fprintf(stderr, "Cannot open display\n");
        return 1;
    }

    Window window = XCreateSimpleWindow(
        display,
        RootWindow(display, 0),
        100,
        100,
        300,
        300,
        2,
        BlackPixel(display, 0),
        WhitePixel(display, 0)
    );

    XEvent event;

    XSelectInput(display, window, ExposureMask);

    XMapWindow(display, window);

    while (1)
    {
        XNextEvent(display, &event);

        switch (event.type)
        {
        case Expose:
            printf("Expose\n");
            break;
        }
    }

    XCloseDisplay(display);

    return 0;
}