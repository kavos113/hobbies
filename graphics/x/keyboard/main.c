#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

int main() {
    int len;
    char buffer[20];
    XEvent event;
    KeySym key;

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


    XSelectInput(display, window, ExposureMask | KeyPressMask);
    //XSelectInput(display, window, ExposureMask);

    XMapWindow(display, window);

    while (1)
    {
        XNextEvent(display, &event);

        switch (event.type)
        {
        case Expose:
            printf("Expose\n");
            break;

        case KeyPress:
            len = XLookupString(&event.xkey, buffer, sizeof(buffer), &key, NULL);
            if (len > 0)
            {
                buffer[len] = 0;
                printf("keypress event keysym: %ld, key: %s\n", key, buffer);

                if (strcmp(buffer, "q") == 0)
                {
                    XCloseDisplay(display);
                    return 0;
                }
            }

        }
    }

    return 0;
}
