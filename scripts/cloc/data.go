package main

import (
	_ "embed"
	"encoding/json"
	"os"
	"path/filepath"
	"slices"
	"strings"
)

//go:embed data/lang-filename.json
var filenameData []byte

//go:embed data/lang-extension.json
var extensionData []byte

type Rules struct {
	extensionRules map[string][]Rule
	filenameRules  map[string][]Rule
}

type Rule struct {
	Name    string   `json:"name"`
	Include []string `json:"include"`
}

func NewRules() *Rules {
	var extRules map[string][]Rule
	if err := json.Unmarshal(extensionData, &extRules); err != nil {
		panic(err)
	}

	var fnameRules map[string][]Rule
	if err := json.Unmarshal(filenameData, &fnameRules); err != nil {
		panic(err)
	}

	return &Rules{
		extensionRules: extRules,
		filenameRules:  fnameRules,
	}
}

func searchInclude(path string, rules []Rule) string {
	if len(rules) == 1 {
		return rules[0].Name
	}

	data, err := os.ReadFile(path)
	if err != nil {
		return ""
	}

	for _, r := range rules {
		if len(r.Include) == 0 {
			return r.Name
		}

		contains := slices.ContainsFunc(r.Include, func(s string) bool {
			return strings.Contains(string(data), s)
		})
		if contains {
			return r.Name
		}
	}

	return ""
}

// path is expected absolute path
func (r *Rules) DetectLanguage(path string) string {
	rule, ok := r.filenameRules[filepath.Base(path)]
	if ok {
		return searchInclude(path, rule)
	}

	rule, ok = r.extensionRules[filepath.Ext(path)]
	if ok {
		return searchInclude(path, rule)
	}

	return ""
}
