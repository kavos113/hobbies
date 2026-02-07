import sys

def format_macros(input_path, output_path):
    with open(input_path, 'r', encoding='utf-8') as infile:
        lines = infile.readlines()

    longest_macro_length = 0
    for line in lines:
        if line.startswith("#define"):
            parts = line.split()
            if len(parts) >= 2:
                macro_name = parts[1]
                if len(macro_name) > longest_macro_length:
                    longest_macro_length = len(macro_name)

    formatted_lines = []
    for line in lines:
        if line.startswith("#define"):
            parts = line.split()
            if len(parts) >= 3:
                macro_name = parts[1]
                macro_value = ' '.join(parts[2:])
                padding = ' ' * (longest_macro_length - len(macro_name) + 1)
                formatted_line = f"#define {macro_name}{padding}{macro_value}\n"
                formatted_lines.append(formatted_line)
            else:
                formatted_lines.append(line)
        else:
            formatted_lines.append(line)

    with open(output_path, 'w', encoding='utf-8') as outfile:
        outfile.writelines(formatted_lines)

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python c-macro-formatter.py <input_file> <output_file>")
        sys.exit(1)

    input_file_path = sys.argv[1]
    output_file_path = sys.argv[2]

    format_macros(input_file_path, output_file_path)