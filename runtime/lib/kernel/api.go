package kernel

import (
	"fmt"
)

func EscaValue_show(value interface{}) Void {
	fmt.Println(value)
	return Void{}
}
