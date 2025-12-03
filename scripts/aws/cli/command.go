package cli

import (
	"fmt"
	"strconv"
	"strings"
)

type OptionType int

const (
	OptionTypeString OptionType = iota
	OptionTypeInt
	OptionTypeBool
)

type Command struct {
	Name        string
	SubCommands []SubCommand
}

type SubCommand struct {
	Name        string
	Description string
	Options     []CommandOption
	Callback    func(args map[string]any) error
}

// if NoName is true, user can provide `cmd <value>` instead of `cmd --name <value>`
// expected to be sorted, NoName option should be first if exists
type CommandOption struct {
	Name        string
	Type        OptionType
	IsRequired  bool
	NoName      bool
	Description string
}

type Cli struct {
	Commands []Command
}

func NewCli() *Cli {
	return &Cli{}
}

func (cli *Cli) AddCommand(name string) {
	cli.Commands = append(cli.Commands, Command{Name: name})
}

func (cli *Cli) AddSubCommand(commandName, subCommandName, description string, options []CommandOption, callback func(args map[string]any) error) {
	for i, cmd := range cli.Commands {
		if cmd.Name == commandName {
			cli.Commands[i].SubCommands = append(cli.Commands[i].SubCommands, SubCommand{
				Name:        subCommandName,
				Description: description,
				Options:     options,
				Callback:    callback,
			})
			return
		}
	}
}

func (cli *Cli) ParseAndExecute(args []string) error {
	if len(args) < 2 {
		return nil
	}

	commandName := args[0]
	subCommandName := args[1]

	var selectedCommand *Command
	for _, cmd := range cli.Commands {
		if cmd.Name == commandName {
			selectedCommand = &cmd
			break
		}
	}
	if selectedCommand == nil {
		cli.Usage()
		return nil
	}

	var selectedSubCommand *SubCommand
	for _, subCmd := range selectedCommand.SubCommands {
		if subCmd.Name == subCommandName {
			selectedSubCommand = &subCmd
			break
		}
	}
	if selectedSubCommand == nil {
		CommandUsage(*selectedCommand)
		return nil
	}

	parsedArgs, err := parseOptions(args[2:], selectedSubCommand.Options)
	if err != nil {
		SubCommandUsage(*selectedCommand, *selectedSubCommand)
		return err
	}
	if parsedArgs == nil {
		SubCommandUsage(*selectedCommand, *selectedSubCommand)
		return nil
	}

	if selectedSubCommand.Callback != nil {
		return selectedSubCommand.Callback(parsedArgs)
	}
	return nil
}

func parseOptions(args []string, options []CommandOption) (map[string]any, error) {
	parsedArgs := make(map[string]any)
	for i := 0; i < len(args); {
		arg := args[i]
		var option *CommandOption
		if len(arg) > 2 && arg[:2] == "--" {
			optionName := arg[2:]
			for _, opt := range options {
				if opt.Name == optionName {
					option = &opt
					break
				}
			}
			if option == nil {
				return nil, fmt.Errorf("unknown option: %s", arg)
			}
			i++
		} else {
			for _, opt := range options {
				if opt.NoName {
					option = &opt
					break
				}
			}
			if option == nil {
				return nil, fmt.Errorf("unknown option: %s", arg)
			}
		}

		if option.Type == OptionTypeBool {
			parsedArgs[option.Name] = true
			continue
		}

		if i >= len(args) {
			return nil, nil
		}
		value := args[i]
		i++

		switch option.Type {
		case OptionTypeString:
			parsedArgs[option.Name] = value
		case OptionTypeInt:
			intValue, err := strconv.Atoi(value)
			if err != nil {
				return nil, fmt.Errorf("invalid value for option %s: %v", option.Name, err)
			}
			parsedArgs[option.Name] = intValue
		}
	}

	for _, opt := range options {
		if opt.IsRequired {
			if _, exists := parsedArgs[opt.Name]; !exists {
				return nil, fmt.Errorf("missing required option: %s", opt.Name)
			}
		}
	}

	return parsedArgs, nil
}

func (cli *Cli) Usage() {
	fmt.Println("Available commands:")
	for _, cmd := range cli.Commands {
		for _, subCmd := range cmd.SubCommands {
			fmt.Printf("  %s %s: %s\n", cmd.Name, subCmd.Name, subCmd.Description)
		}
	}
}

func CommandUsage(c Command) {
	fmt.Printf("Usage: %s <subcommand> [options]\n", c.Name)
	fmt.Println("Subcommands:")
	for _, subCmd := range c.SubCommands {
		fmt.Printf("  %s: %s\n", subCmd.Name, subCmd.Description)
	}
}

func SubCommandUsage(c Command, sc SubCommand) {
	var sb strings.Builder

	sb.WriteString(fmt.Sprintf("%s %s", c.Name, sc.Name))
	for _, opt := range sc.Options {
		if opt.NoName {
			if opt.IsRequired {
				sb.WriteString(fmt.Sprintf(" <%s>", opt.Name))
			} else {
				sb.WriteString(fmt.Sprintf(" [%s]", opt.Name))
			}
		} else {
			if opt.IsRequired {
				sb.WriteString(fmt.Sprintf(" --%s <%s>", opt.Name, opt.Name))
			} else {
				sb.WriteString(fmt.Sprintf(" [--%s <%s>]", opt.Name, opt.Name))
			}
		}
	}
	fmt.Println("Usage:", sb.String())
	fmt.Println("Options:")
	for _, opt := range sc.Options {
		req := "optional"
		if opt.IsRequired {
			req = "required"
		}
		fmt.Printf("  --%s (%s): %s\n", opt.Name, req, opt.Description)
	}
}