import csv
import sys

"""
columns = [
    "hid_page_name",
    "hid_usage_name",
    "hid_usage_id",
    "hid_page_id",
    "scan_code",
    "location"
]
"""

def read_scancodes(file_path):
    scancodes = []
    with open(file_path, 'r', encoding='utf-8', errors='ignore') as csvfile:
        reader = csv.reader(csvfile, delimiter='\t')
        for row in reader:
            scancodes.append(row)
    return scancodes

def write_scancodes_to_file(scancodes, keycodes, output_path):
    without_extended = []
    with_extended = []

    for sc in scancodes:
        scancode = int(sc[4], 16)
        hid_page_id = int(sc[3], 16)
        if scancode < 0xE000:
            without_extended.append((hid_page_id, scancode))
        else:
            with_extended.append((hid_page_id, scancode - 0xE000))   

    without_extended_keycodes = []        
    for sc in without_extended:
        for kc in keycodes:
            if sc[0] == kc[1]:
                without_extended_keycodes.append((sc[1], kc[0]))
                break
    without_extended_keycodes.sort()

    with_extended_keycodes = []
    for sc in with_extended:
        for kc in keycodes:
            if sc[0] == kc[1]:
                with_extended_keycodes.append((sc[1], kc[0]))
                break
    with_extended_keycodes.sort()

    with open(output_path, 'w', encoding='utf-8') as output_file:
        output_file.write("// Without Extended\n")
        for i in range(0, 256):
            matched = False
            for entry in without_extended_keycodes:
                if entry[0] == i:
                    output_file.write(f"        {entry[1]},".ljust(33) + f"// 0x{i:02X}\n")
                    matched = True
                    break
            if not matched:
                output_file.write(f"        Undefined,".ljust(33) + f"// 0x{i:02X}\n")
        output_file.write("\n// With Extended\n")
        for i in range(0, 256):
            matched = False
            for entry in with_extended_keycodes:
                if entry[0] == i:
                    output_file.write(f"        {entry[1]},".ljust(33) + f"// 0x{i:02X}\n")
                    matched = True
                    break
            if not matched:
                output_file.write(f"        Undefined,".ljust(33) + f"// 0x{i:02X}\n")
# like "Name = 0xFF,"
def read_keycodes(file_path):
    keycodes = []
    with open(file_path, 'r', encoding='utf-8', errors='ignore') as file:
        lines = file.readlines()
        for line in lines:
            parts = line.split('=')
            if len(parts) == 2:
                name = parts[0].strip()
                value = int(parts[1].strip().rstrip(','), 16)
                keycodes.append((name, value))
    return keycodes

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Usage: python sc.py <input_csv_path> <input_keycodes_path> <output_txt_path>")
        sys.exit(1)

    input_csv_path = sys.argv[1]
    input_keycodes_path = sys.argv[2]
    output_txt_path = sys.argv[3]

    scancodes = read_scancodes(input_csv_path)
    keycodes = read_keycodes(input_keycodes_path)
    write_scancodes_to_file(scancodes, keycodes, output_txt_path)