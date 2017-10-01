package kernel

import (
	"fmt"
	"github.com/szktty/esca/runtime/base"
)

func EscaValue_show(value string) types.Void {
	fmt.Println(value)
	return types.Void{}
}
