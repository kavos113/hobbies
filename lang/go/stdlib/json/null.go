package main

import (
	"encoding/json"
	"fmt"
)

type msg struct {
	Message string `json:message`
}

func main() {
	jsonstr := "{\"message\": \"string\"}"
	nullstr := "{\"message\": null}"

	var j msg
	var n msg
	if err := json.Unmarshal([]byte(jsonstr), &j); err != nil {
		panic(err)
	}

	if err := json.Unmarshal([]byte(nullstr), &n); err != nil {
		panic(err)
	}

	fmt.Printf("json: %s (%d), null: %s (%d)\n", j.Message, len(j.Message), n.Message, len(n.Message))
	fmt.Printf("json: %+v, null: %+v\n", j, n)
}
