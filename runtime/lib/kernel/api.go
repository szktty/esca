package kernel

import (
	"fmt"
	"github.com/szktty/esca/go/runtime/base"
)

func EscaPrimPrint(arg string) types.Void {
	fmt.Println(arg)
	return types.Void{}
}
