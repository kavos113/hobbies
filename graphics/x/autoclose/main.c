#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>

typedef struct
{
    Display *display;
    Window window;
} CloseWindowArgs;

void *close_window(void *arg)
{
    sleep(1);

    CloseWindowArgs *args = arg;
    Display *target_display = args->display;
    Window target_window = args->window;

    Atom target_close_event = XInternAtom(target_display, "WM_DELETE_WINDOW", False);
    if (target_close_event == None)
    {
        fprintf(stderr, "couldn't get close event atom\n");
        return NULL;
    }

    XEvent event = {};
    event.type = ClientMessage;
    event.xclient.window = target_window;
    event.xclient.message_type = target_close_event;
    event.xclient.format = 32;

    Status status = XSendEvent(
        target_display,
        target_window,
        False,
        NoEventMask,
        &event
    );
    if (status == 0)
    {
        fprintf(stderr, "couldn't send close event\n");
    }
    else
    {
        printf("close event sent\n");
    }

    XFlush(target_display);

    return NULL;
}

int main(int argc, char **argv)
{
    unsigned int border_width = 4;
    char *window_name = "AutoClose Sample";

    char *display_name = NULL;

    XSizeHints *size_hints = XAllocSizeHints();
    if (size_hints == NULL)
    {
        fprintf(stderr, "couldn't allocate size hints\n");
        return 1;
    }

    XWMHints *wm_hints = XAllocWMHints();
    if (wm_hints == NULL)
    {
        fprintf(stderr, "couldn't allocate WM hints\n");
        return 1;
    }

    XClassHint *class_hints = XAllocClassHint();
    if (class_hints == NULL)
    {
        fprintf(stderr, "couldn't allocate class hints\n");
        return 1;
    }

    char *program_name = argv[0];

    Display *display = XOpenDisplay(display_name);
    if (display == NULL)
    {
        fprintf(stderr, "%s: cannot connect to X server '%s'\n", program_name, display_name);
        return 1;
    }

    int screen_num = DefaultScreen(display);
    unsigned int display_width = DisplayWidth(display, screen_num);
    unsigned int display_height = DisplayHeight(display, screen_num);

    int x = 0;
    int y = 0;

    unsigned int width = display_width / 3;
    unsigned int height = display_height / 3;

    Window window = XCreateSimpleWindow(
        display,
        RootWindow(display, screen_num),
        x,
        y,
        width,
        height,
        border_width,
        BlackPixel(display, screen_num),
        WhitePixel(display, screen_num)
    );

    size_hints->flags = PPosition | PSize | PMinSize;
    size_hints->min_width = 300;
    size_hints->min_height = 200;

    XTextProperty windowName;

    Status status = XStringListToTextProperty(&window_name, 1, &windowName);
    if (status == 0)
    {
        fprintf(stderr, "couldn't create window name string\n");
        return 1;
    }

    wm_hints->flags = InputHint | StateHint;
    wm_hints->input = True;
    wm_hints->initial_state = NormalState;

    class_hints->res_name = program_name;
    class_hints->res_class = "AutoClose";

    XSetWMProperties(
        display,
        window,
        &windowName,
        NULL,
        argv,
        argc,
        size_hints,
        wm_hints,
        class_hints
    );

    XSelectInput(display, window, ExposureMask | StructureNotifyMask);

    unsigned long valuemask = 0;
    XGCValues values;
    GC gc = XCreateGC(display, window, valuemask, &values);

    XMapWindow(display, window);

    CloseWindowArgs args;
    args.display = display;
    args.window = window;
    pthread_t thread;
    int ret = pthread_create(&thread, NULL, close_window, &args);
    if (ret != 0)
    {
        fprintf(stderr, "couldn't create thread\n");
        return 1;
    }
    void *thread_ret;

    XEvent event;
    int running = 1;
    while (running)
    {
        XNextEvent(display, &event);

        switch (event.type)
        {
        case ClientMessage:
            printf("close event recieved\n");
            running = 0;
            break;

        default:
            break;
        }
    }

    ret = pthread_join(thread, &thread_ret);
    if (ret != 0)
    {
        fprintf(stderr, "couldn't join thread\n");
        return 1;
    }

    XFreeGC(display, gc);
    XDestroyWindow(display, window);
    XCloseDisplay(display);
    return 0;
}
