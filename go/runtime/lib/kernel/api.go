package esca

import (
	"fmt"
	"github.com/szktty/esca/go/runtime/base"
)

func EscaPrimPrint(arg string) esca.Void {
	fmt.Println(arg)
	return esca.Void{}
}
