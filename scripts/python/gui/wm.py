winuser_path = "C:\\Program Files (x86)\\Windows Kits\\10\\Include\\10.0.26100.0\\um\\WinUser.h"

def get_and_write_windows_message():
    with open(winuser_path, 'r', encoding='utf-8', errors='ignore') as file:
        lines = file.readlines()

    message_definitions = []
    recording = False

    for line in lines:
        if line.startswith("#define WM_"):
            recording = True
            parts = line.split()
            if len(parts) >= 3:
                message_name = parts[1]
                message_value = parts[2]
                message_definitions.append(f"    {{ {message_name} ,\"{message_name}\" }},")
        elif recording and not line.startswith("#define WM_"):
            recording = False

    with open("windows_messages.txt", 'w') as output_file:
        for definition in message_definitions:
            output_file.write(definition + "\n")

if __name__ == "__main__":
    get_and_write_windows_message()