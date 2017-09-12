package kernel

import (
	"fmt"
	"github.com/szktty/esca/runtime/base"
)

func EscaPrimPrint(arg string) types.Void {
	fmt.Println(arg)
	return types.Void{}
}
