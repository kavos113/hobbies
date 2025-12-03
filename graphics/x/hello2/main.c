#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define WINDOW_SIZE_TOO_SMALL 0
#define WINDOW_SIZE_BIG_ENOUGH 1

Display* display;
int screen_num;
static char* program_name;

void load_font(XFontStruct** font_info);
void get_GC(Window window, GC* gc, XFontStruct* font_info);
void draw_text(
    Window window,
    GC gc,
    XFontStruct* font_info,
    unsigned int window_width,
    unsigned int window_height
);
void draw_graphics(
    Window window,
    GC gc,
    unsigned int window_width,
    unsigned int window_height
);
void TooSmall(Window window, GC gc, XFontStruct* font_info);

int main(int argc, char** argv)
{
    unsigned int border_width = 4;
    char* window_name = "Hello, World!";

    char* display_name = NULL;
    int window_size = WINDOW_SIZE_BIG_ENOUGH;

    XSizeHints *size_hints = XAllocSizeHints();
    if (size_hints == NULL)
    {
        fprintf(stderr, "%s: couldn't allocate size hints\n", program_name);
        return 1;
    }

    XWMHints *wm_hints = XAllocWMHints();
    if (wm_hints == NULL)
    {
        fprintf(stderr, "%s: couldn't allocate WM hints\n", program_name);
        return 1;
    }

    XClassHint *class_hints = XAllocClassHint();
    if (class_hints == NULL)
    {
        fprintf(stderr, "%s: couldn't allocate class hints\n", program_name);
        return 1;
    }

    program_name = argv[0];

    // 1. connect to X server
    display = XOpenDisplay(display_name);
    if (display == NULL)
    {
        fprintf(stderr, "%s: unable to open display '%s'\n", program_name, XDisplayName(display_name));
        return 1;
    }

    screen_num = DefaultScreen(display);
    unsigned int display_width = DisplayWidth(display, screen_num);
    unsigned int display_height = DisplayHeight(display, screen_num);

    int x = 0;
    int y = 0;

    unsigned int width = display_width / 3;
    unsigned int height = display_height / 3;

    // 2. create a window
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

    // 3. set hints to window manager
    size_hints->flags = PPosition | PSize | PMinSize;
    size_hints->min_width = 300;
    size_hints->min_height = 200;

    XTextProperty windowName;

    Status ret = XStringListToTextProperty(&window_name, 1, &windowName);
    if (ret == 0)
    {
        fprintf(stderr, "%s: structure allocation for windowName failed\n", program_name);
        return 1;
    }

    wm_hints->initial_state = NormalState;
    wm_hints->input = True;
    wm_hints->flags = StateHint | InputHint;

    class_hints->res_name = program_name;
    class_hints->res_class = "hellox";

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

    // 4. select events
    XSelectInput(
        display,
        window,
        ExposureMask | StructureNotifyMask | KeyPressMask | ButtonPressMask
    );

    // 5. create server resources
    GC gc;
    XFontStruct* font_info;

    load_font(&font_info);
    get_GC(window, &gc, font_info);

    // 6. map the window
    XMapWindow(display, window);

    // 7. event loop
    XEvent event;

    while (1)
    {
        XNextEvent(display, &event);
        switch (event.type)
        {
        case Expose:
            printf("Expose\n");
            if (event.xexpose.count != 0)
            {
                break;
            }

            if (window_size == WINDOW_SIZE_TOO_SMALL)
            {
                TooSmall(window, gc, font_info);
            }
            else
            {
                draw_text(window, gc, font_info, width, height);
                draw_graphics(window, gc, width, height);
            }
            break;

        case ConfigureNotify:
            printf("ConfigureNotify\n");
            width = event.xconfigure.width;
            height = event.xconfigure.height;
            if (width < size_hints->min_width || height < size_hints->min_height)
            {
                window_size = WINDOW_SIZE_TOO_SMALL;
            }
            else
            {
                window_size = WINDOW_SIZE_BIG_ENOUGH;
            }
            break;

        case ButtonPress:
        case KeyPress:
            printf("ButtonPress or KeyPress\n");
            XUnloadFont(display, font_info->fid);
            XFreeGC(display, gc);
            XCloseDisplay(display);
            return 0;

        default:
            break;
        }
    }
}

void load_font(XFontStruct **font_info)
{
    char *fontname = "-*-helvetica-bold-r-*-*-24-*-*-*-*-*-*-*";

    *font_info = XLoadQueryFont(display, fontname);
    if (!*font_info)
    {
        fprintf(stderr, "%s: cannot open 24 point helvetica font\n", program_name);
        exit(-1);
    }
}

void get_GC(Window window, GC *gc, XFontStruct *font_info)
{
    unsigned long valuemask = 0;
    XGCValues values;

    *gc = XCreateGC(display, window, valuemask, &values);

    XSetFont(display, *gc, font_info->fid);
    XSetForeground(display, *gc, BlackPixel(display, screen_num));

    unsigned int line_width = 6;
    int line_style = LineOnOffDash;
    int cap_style = CapRound;
    int join_style = JoinRound;
    int dash_offset = 0;
    static char dash_list[] = {12, 24};
    int list_length = sizeof(dash_list);

    XSetLineAttributes(display, *gc, line_width, line_style, cap_style, join_style);
    XSetDashes(display, *gc, dash_offset, dash_list, list_length);
}

void draw_text(Window window, GC gc, XFontStruct *font_info, unsigned int window_width, unsigned int window_height)
{
    char *string1 = "Hello, X Window System!";
    char *string2 = "To terminate, press any key in this window.";
    char *string3 = "or button in this window.";
    char *string4 = "Screen Dimensions: ";
    char cd_height[50], cd_width[50], cd_depth[50];

    int len1 = strlen(string1);
    int len2 = strlen(string2);
    int len3 = strlen(string3);

    int width1 = XTextWidth(font_info, string1, len1);
    int width2 = XTextWidth(font_info, string2, len2);
    int width3 = XTextWidth(font_info, string3, len3);

    int font_height = font_info->ascent + font_info->descent;

    XDrawString(
        display,
        window,
        gc,
        (window_width - width1) / 2,
        font_height,
        string1,
        len1
    );
    XDrawString(
        display,
        window,
        gc,
        (window_width - width2) / 2,
        font_height * 2,
        string2,
        len2
    );
    XDrawString(
        display,
        window,
        gc,
        (window_width - width3) / 2,
        font_height * 3,
        string3,
        len3
    );

    sprintf(cd_height, "Height: %d pixels", window_height);
    sprintf(cd_width, "Width: %d pixels", window_width);
    sprintf(cd_depth, "Depth: %d plane(s)", DefaultDepth(display, screen_num));

    int len4 = strlen(string4);
    len1 = strlen(cd_height);
    len2 = strlen(cd_width);
    len3 = strlen(cd_depth);

    int initial_y_offset = window_height / 2 - font_height - font_info->descent;
    int x_offset = window_width / 4;

    XDrawString(
        display,
        window,
        gc,
        x_offset,
        initial_y_offset,
        string4,
        len4
    );
    XDrawString(
        display,
        window,
        gc,
        x_offset,
        initial_y_offset + font_height,
        cd_height,
        len1
    );
    XDrawString(
        display,
        window,
        gc,
        x_offset,
        initial_y_offset + font_height * 2,
        cd_width,
        len2
    );
    XDrawString(
        display,
        window,
        gc,
        x_offset,
        initial_y_offset + font_height * 3,
        cd_depth,
        len3
    );
}

void draw_graphics(Window window, GC gc, unsigned int window_width, unsigned int window_height)
{
    int height = window_height / 2;
    int width = window_width * 3 / 4;
    int x = window_width / 2 - width / 2;
    int y = window_height / 2 - height / 2;

    XDrawRectangle(display, window, gc, x, y, width, height);
}

void TooSmall(Window window, GC gc, XFontStruct *font_info)
{
    char *string1 = "Window too small.";
    int y_offset = font_info->ascent + 2;
    int x_offset = 2;

    XDrawString(
        display,
        window,
        gc,
        x_offset,
        y_offset,
        string1,
        strlen(string1)
    );
}
