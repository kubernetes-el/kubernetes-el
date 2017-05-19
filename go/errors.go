package main

import (
	"fmt"
	"io"

	"github.com/kalmanb/sexpr"
)

type errorMsg struct {
	Type  string `json:"type"`
	Msg   string `json:"msg"`
	Error string `json:"error"`
}

func writeError(w io.Writer, msg string, e error) {
	res := errorMsg{
		Type:  "error",
		Msg:   msg,
		Error: fmt.Sprint(e),
	}

	encode := sexpr.NewEncoder(w)
	err := encode.Encode(res)
	if err != nil {
		panic("Error could not encode error message, exiting")
	}

}
