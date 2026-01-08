#ifndef KERNEL_PCI_H
#define KERNEL_PCI_H
#include <array>
#include <cstdint>

#include "error.h"

namespace pci
{
constexpr uint16_t kConfigAddress = 0x0cf8;
constexpr uint16_t kConfigData = 0x0cfc;

struct ClassCode
{
    uint8_t base, sub, interface;

    bool match(uint8_t b) const
    {
        return b == base;
    }

    bool match(uint8_t b, uint8_t s) const
    {
        return match(b) && s == sub;
    }

    bool match(uint8_t b, uint8_t s, uint8_t i) const
    {
        return match(b, s) && i == interface;
    }
};

struct Device
{
    uint8_t bus, device, function, header_type;
    ClassCode class_code;
};


void write_address(uint32_t address);
void write_data(uint32_t value);
uint32_t read_data();
uint16_t read_vendor_id(uint8_t bus, uint8_t device, uint8_t function);
uint16_t read_device_id(uint8_t bus, uint8_t device, uint8_t function);
uint8_t read_header_type(uint8_t bus, uint8_t device, uint8_t function);
ClassCode read_class_code(uint8_t bus, uint8_t device, uint8_t function);
uint32_t read_bus_numbers(uint8_t bus, uint8_t device, uint8_t function); // for header type 1
bool is_single_function_device(uint8_t header_type);

inline uint16_t read_vendor_id(const Device& dev)
{
    return read_vendor_id(dev.bus, dev.device, dev.function);
}

uint32_t read_config_register(const Device& dev, uint8_t reg_addr);
void write_config_register(const Device& dev, uint8_t reg_addr, uint32_t value);

inline std::array<Device, 32> devices;
inline int num_device;

Error scan_all_bus();

uint8_t calc_bar_address(unsigned int bar_index);
Error read_bar(Device& device, unsigned int bar_index, uint64_t* out);

union CapabilityHeader
{
    uint32_t data;
    struct
    {
        uint32_t cap_id : 8;
        uint32_t next_ptr : 8;
        uint32_t cap : 16;
    } __attribute__((packed)) bits;
} __attribute__((packed));

constexpr uint8_t kCapabilityMSI  = 0x05;
constexpr uint8_t kCapabilityMSIX = 0x11;

CapabilityHeader read_capability_header(const Device& dev, uint8_t addr);

struct MSICapability
{
    union
    {
        uint32_t data;
        struct
        {
            uint32_t cap_id : 8;
            uint32_t next_ptr : 8;
            uint32_t msi_enable : 1;
            uint32_t multi_msg_capable : 3;
            uint32_t multi_msg_enable : 3;
            uint32_t addr_64_capable : 1;
            uint32_t per_vector_mask_capable : 1;
            uint32_t : 7;
        } __attribute__((packed)) bits;
    } __attribute__((packed)) header;

    uint32_t msg_addr;
    uint32_t msg_upper_addr;
    uint32_t msg_data;
    uint32_t mask_bits;
    uint32_t pending_bits;
} __attribute__((packed));

Error configure_msi(
    const Device &dev,
    uint32_t msg_addr,
    uint32_t msg_data,
    unsigned int num_vector_exponent
);

enum class MSITriggerMode
{
    kEdge = 0,
    kLevel = 1
};

enum class MSIDeliveryMode
{
    kFixed          = 0b000,
    kLowestPriority = 0b001,
    kSMI            = 0b010,
    kNMI            = 0b100,
    kINIT           = 0b101,
    kExtINT         = 0b111,
};

Error configure_msi_fixed_destination(
    const Device &dev,
    uint8_t apic_id,
    MSITriggerMode trigger_mode,
    MSIDeliveryMode delivery_mode,
    uint8_t vector,
    unsigned int num_vector_exponent
);

}

#endif //KERNEL_PCI_H
