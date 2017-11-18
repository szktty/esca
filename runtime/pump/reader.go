package pump

import (
	"bytes"
	"fmt"
	"os"
	"reflect"
	"strings"
)

func isPublicName(name string) bool {
	return name[0] == strings.ToUpper(name)[0]
}

func isPublicType(ty reflect.Type) bool {
	kind := ty.Kind()
	switch kind {
	case reflect.Ptr:
		return isPublicType(ty.Elem())

	case reflect.Struct, reflect.Interface:
		return isPublicName(ty.Name())

	case reflect.Func:
		for i := 0; i < ty.NumIn(); i++ {
			inTy := ty.In(i)
			if !isPublicType(inTy) {
				return false
			}
		}
		for i := 0; i < ty.NumOut(); i++ {
			outTy := ty.Out(i)
			if !isPublicType(outTy) {
				return false
			}
		}
		return true

	default:
		return true
	}
}

func typeExpr(ty reflect.Type) string {
	kind := ty.Kind()
	switch kind {
	case reflect.Bool:
		return "Bool"
	case reflect.Int:
		return "Int"
	case reflect.Int8:
		return "Int8"
	case reflect.Int16:
		return "Int16"
	case reflect.Int32:
		return "Int32"
	case reflect.Int64:
		return "Int64"
	case reflect.Uint:
		return "UInt"
	case reflect.Uint8:
		return "UInt8"
	case reflect.Uint16:
		return "UInt16"
	case reflect.Uint32:
		return "UInt32"
	case reflect.Uint64:
		return "UInt64"
	case reflect.Float32:
		return "Float32"
	case reflect.Float64:
		return "Float64"
	case reflect.Complex64:
		return "Complex64"
	case reflect.Complex128:
		return "Complex128"
	case reflect.String:
		return "String"

	case reflect.Struct:
		return ty.Name()

	case reflect.Interface:
		name := ty.Name()
		switch name {
		case "error":
			return "Error"
		case "":
			return "Any"
		default:
			return name
		}

	case reflect.Ptr:
		return fmt.Sprintf("*%s", typeExpr(ty.Elem()))

	case reflect.Func:
		buf := bytes.NewBufferString("")
		fmt.Fprintf(buf, "func (")

		numIn := ty.NumIn()
		for i := 0; i < numIn; i++ {
			inTy := ty.In(i)
			fmt.Fprintf(buf, "%s", typeExpr(inTy))
			if i+1 < numIn {
				fmt.Fprintf(buf, ", ")
			}
		}

		fmt.Fprintf(buf, ") -> ")
		numOut := ty.NumOut()
		if numOut == 0 {
			fmt.Fprintf(buf, "Void")
		} else if numOut == 1 {
			fmt.Fprintf(buf, "%s", typeExpr(ty.Out(0)))
		} else {
			fmt.Fprintf(buf, "(")
			for i := 0; i < numOut; i++ {
				fmt.Fprintf(buf, "%s", typeExpr(ty.Out(i)))
				if i+1 < numOut {
					fmt.Fprintf(buf, ", ")
				}
			}
			fmt.Fprintf(buf, ")")
		}
		return buf.String()

	case reflect.Slice:
		return fmt.Sprintf("[%s]", typeExpr(ty.Elem()))

	case reflect.Array:
		return fmt.Sprintf("#[%s]", typeExpr(ty.Elem()))

	case reflect.Map:
		// TODO
		return fmt.Sprintf("[:%s]", typeExpr(ty.Elem()))

	case reflect.Chan:
		return fmt.Sprintf("Channel<%s>", typeExpr(ty.Elem()))

	default:
		panic(fmt.Sprintf("not impl kind %s\n", kind.String()))
	}
}

type Reader struct {
	Path    string // package path
	Name    string // package name
	Methods map[string][]method
	buf     *bytes.Buffer
}

type method struct {
	ty     reflect.Type
	name   string
	params []string
}

func NewReader(path string, name string) *Reader {
	r := &Reader{
		Path:    path,
		Name:    name,
		Methods: map[string][]method{},
		buf:     bytes.NewBufferString("")}
	r.writef("@import(\"%s\", \"%s\")\n\n", path, name)
	return r
}

func (r *Reader) writef(format string, a ...interface{}) {
	fmt.Fprintf(r.buf, format, a...)
}

func (r *Reader) ReadStructType(value interface{}) {
	ty := reflect.TypeOf(value)
	r.writef("extern struct %s {\n", ty.Name())

	for i := 0; i < ty.NumField(); i++ {
		field := ty.Field(i)
		if !isPublicName(field.Name) {
			continue
		}
		r.writef("    var %s: %s\n", field.Name, typeExpr(field.Type))
	}

	r.writef("}\n\n")
}

func (r *Reader) readOut(ty reflect.Type) {
	numOut := ty.NumOut()
	if numOut == 0 {
		r.writef("Void")
	} else if numOut == 1 {
		r.writef("%s", typeExpr(ty.Out(0)))
	} else {
		r.writef("(")
		for i := 0; i < numOut; i++ {
			r.writef("%s", typeExpr(ty.Out(i)))
			if i+1 < numOut {
				r.writef(", ")
			}
		}
		r.writef(")")
	}
}

func (r *Reader) ReadMethodType(name string, value interface{}, params []string) {
	ty := reflect.TypeOf(value)
	recv := typeExpr(ty.In(0))
	r.Methods[recv] = append(r.Methods[recv], method{ty: ty, name: name, params: params})
}

func (r *Reader) outputMethod(method method) {
	ty := method.ty
	ptr := ""
	recvTy := ty.In(0)
	if recvTy.Kind() == reflect.Ptr {
		ptr = "*"
	}
	r.writef("    extern func %s(%sself", method.name, ptr)

	numIn := ty.NumIn()
	if numIn > 1 {
		r.writef(", ")
	}
	for i := 1; i < numIn; i++ {
		inTy := ty.In(i)
		r.writef("%s: %s", method.params[i-1], typeExpr(inTy))
		if i+1 < numIn {
			r.writef(", ")
		}
	}

	r.writef(") -> ")
	r.readOut(ty)
	r.writef("\n")
}

func (r *Reader) ReadFuncType(name string, value interface{}, params []string) {
	ty := reflect.TypeOf(value)
	r.writef("extern func %s(", name)

	numIn := ty.NumIn()
	for i := 0; i < numIn; i++ {
		inTy := ty.In(i)
		r.writef("%s: %s", params[i], typeExpr(inTy))
		if i+1 < numIn {
			r.writef(", ")
		}
	}

	r.writef(") -> ")
	r.readOut(ty)
	r.writef("\n\n")
}

func (r *Reader) Output(path string) error {
	// methods
	for name, methods := range r.Methods {
		r.writef("extension %s {\n", name)
		for _, method := range methods {
			r.outputMethod(method)
		}
		r.writef("}\n\n")
	}

	file, err := os.Create(path)
	defer file.Close()
	if err != nil {
		return err
	}
	file.WriteString(r.buf.String())
	return nil
}
