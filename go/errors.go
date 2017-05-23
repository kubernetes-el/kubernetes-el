package main

import (
	"fmt"

	"github.com/kalmanb/sexpr"
)

type errorMsg struct {
	Type  string `json:"type"`
	Msg   string `json:"msg"`
	Error string `json:"error"`
}

func writeError(msg string, e error) []byte {
	res := errorMsg{
		Type:  "error",
		Msg:   msg,
		Error: fmt.Sprint(e),
	}

	ret, err := sexpr.Marshal(res)
	if err != nil {
		panic("Error could not encode error message, exiting")
	}
	return ret
}
