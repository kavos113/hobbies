package cli

import "testing"

func TestParseOptions(t *testing.T) {
	testCases := []struct {
		name       string
		args       []string
		options    []CommandOption
		expected   map[string]any
		shouldFail bool
	}{
		{
			name: "string and int options",
			args: []string{"--name", "test", "--count", "5"},
			options: []CommandOption{
				{Name: "name", Type: OptionTypeString, IsRequired: true},
				{Name: "count", Type: OptionTypeInt, IsRequired: true},
			},
			expected: map[string]any{
				"name":  "test",
				"count": 5,
			},
			shouldFail: false,
		},
		{
			name: "missing required option",
			args: []string{"--name", "test"},
			options: []CommandOption{
				{Name: "name", Type: OptionTypeString, IsRequired: true},
				{Name: "count", Type: OptionTypeInt, IsRequired: true},
			},
			expected:   nil,
			shouldFail: true,
		},
		{
			name: "boolean option",
			args: []string{"--verbose"},
			options: []CommandOption{
				{Name: "verbose", Type: OptionTypeBool, IsRequired: false},
			},
			expected: map[string]any{
				"verbose": true,
			},
			shouldFail: false,
		},
		{
			name: "no-name option",
			args: []string{"value1", "--flag"},
			options: []CommandOption{
				{Name: "input", Type: OptionTypeString, IsRequired: true, NoName: true},
				{Name: "flag", Type: OptionTypeBool, IsRequired: false},
			},
			expected: map[string]any{
				"input": "value1",
				"flag":  true,
			},
			shouldFail: false,
		},
		{
			name: "invalid int option",
			args: []string{"--count", "notanint"},
			options: []CommandOption{
				{Name: "count", Type: OptionTypeInt, IsRequired: true},
			},
			expected:   nil,
			shouldFail: true,
		},
		{
			name: "not error when missing non-required option",
			args: []string{"--name", "test"},
			options: []CommandOption{
				{Name: "name", Type: OptionTypeString, IsRequired: true},
				{Name: "verbose", Type: OptionTypeBool, IsRequired: false},
			},
			expected: map[string]any{
				"name": "test",
			},
			shouldFail: false,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, err := parseOptions(tc.args, tc.options)
			if tc.shouldFail {
				if err == nil {
					t.Errorf("expected failure but got success")
				}
			} else {
				if err != nil {
					t.Errorf("unexpected error: %v", err)
				} else {
					for key, expectedValue := range tc.expected {
						if result[key] != expectedValue {
							t.Errorf("for key %s, expected %v but got %v", key, expectedValue, result[key])
						}
					}
				}
			}
		})
	}
}
