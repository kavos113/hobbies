#ifndef KERNEL_IDT_H
#define KERNEL_IDT_H

#include <cstdint>

extern "C"
{
    uint16_t GetCS(void);
    void LoadIDT(uint16_t limit, uint64_t offset);
}

#endif //KERNEL_IDT_H