#ifndef KERNEL_INTERRUPT_H
#define KERNEL_INTERRUPT_H

#include <array>
#include <cstdint>

enum class DescriptorType
{
    Upper8Bytes = 0,
    LDT = 2,
    TSSAvailable = 9,
    TSSBusy = 11,
    CallGate = 12,
    InterruptGate = 14,
    TrapGate = 15,
};

union InterruptDescriptorAttribute
{
    uint16_t data;
    struct
    {
        uint16_t interrupt_stack_table : 3;
        uint16_t : 5;
        DescriptorType type : 4;
        uint16_t : 1;
        uint16_t descriptor_privilege_level : 2;
        uint16_t present : 1;
    } __attribute__((packed)) bits;
} __attribute__((packed));

struct InterruptDescriptor
{
    uint16_t offset_low;
    uint16_t segment_selector;
    InterruptDescriptorAttribute attr;
    uint16_t offset_middle;
    uint32_t offset_high;
    uint32_t reserved;
} __attribute__((packed));

std::array<InterruptDescriptor, 256> idt;

constexpr InterruptDescriptorAttribute
make_idt_attr(
    DescriptorType type,
    uint8_t descriptor_privilege_level,
    bool present = true,
    uint8_t interrupt_stack_table = 0
)
{
    InterruptDescriptorAttribute attr{};

    attr.bits.descriptor_privilege_level = descriptor_privilege_level;
    attr.bits.interrupt_stack_table = interrupt_stack_table;
    attr.bits.present = present;
    attr.bits.type = type;

    return attr;
}

void set_idt_entry(
    InterruptDescriptor& desc,
    InterruptDescriptorAttribute attr,
    uint64_t offset,
    uint16_t segment_selector
);

class InterruptVector
{
public:
    enum Number
    {
        kXHCI = 0x40,
    };
};

struct InterruptFrame
{
    uint64_t rip;
    uint64_t cs;
    uint64_t rflags;
    uint64_t rsp;
    uint64_t ss;
};

void notify_end_of_interrupt();


#endif //KERNEL_INTERRUPT_H