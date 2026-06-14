#ifndef HELLO_GLFW_APP_H
#define HELLO_GLFW_APP_H

#define GLFW_INCLUDE_VULKAN
#include <GLFW/glfw3.h>

class App
{
public:
    App();
    ~App();

    void run();

private:
    void createWindow();

    GLFWwindow* m_window;
};


#endif //HELLO_GLFW_APP_H
