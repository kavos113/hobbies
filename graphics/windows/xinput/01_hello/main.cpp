#include <windows.h>
#include <winerror.h>
#include <Xinput.h>
#include <print>

int main()
{
    DWORD dwResult;
    for (DWORD i = 0; i < XUSER_MAX_COUNT; i++)
    {
        XINPUT_STATE state = {};

        dwResult = XInputGetState(i, &state);

        if (dwResult == ERROR_SUCCESS)
        {
            std::println("controller {} found", i);

            XINPUT_GAMEPAD g = state.Gamepad;
            std::println("button {} left {} right {} lx {} ly {} rx {} ry {}", g.wButtons, g.bLeftTrigger, g.bRightTrigger, g.sThumbLX, g.sThumbLY, g.sThumbRX, g.sThumbRY);
        }
    }

    return 0;
}