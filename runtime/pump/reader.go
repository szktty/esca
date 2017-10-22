package pump

import (
	"bytes"
	"fmt"
	"reflect"
	"strings"
)

func escaVarName(name string) string {
	buf := bytes.NewBufferString("")
	head := true
	lower := strings.ToLower(name)
	for i, c := range name {
		if head {
			if name[i] == lower[i] {
				buf.WriteRune(c)
				head = false
			} else {
				buf.WriteByte(lower[i])
			}
		} else {
			buf.WriteRune(c)
		}
	}
	return buf.String()
}

func mapAnnot(name string) string {
	return fmt.Sprintf("#map(\"%s\")", name)
}

func isPublic(name string) bool {
	return name[0] == strings.ToUpper(name)[0]
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
	case reflect.String:
		return "String"

	case reflect.Struct, reflect.Interface:
		return ty.Name()

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

	default:
		panic("not impl kind\n")
	}
}

type Reader struct {
	Path string // package path
	Name string // package name
	buf  *bytes.Buffer
}

func NewReader(path string, name string) *Reader {
	r := &Reader{Path: path, Name: name, buf: bytes.NewBufferString("")}
	r.writef("#import(\"%s\")\n", path)
	r.writef("package %s\n\n", name)
	return r
}

func (r *Reader) writef(format string, a ...interface{}) {
	fmt.Fprintf(r.buf, format, a...)
}

func (r *Reader) ReadStructType(value interface{}) {
	ty := reflect.TypeOf(value)
	r.writef("struct %s {\n", ty.Name())
	for i := 0; i < ty.NumField(); i++ {
		field := ty.Field(i)
		if !isPublic(field.Name) {
			continue
		}
		r.writef("    %s var %s: %s\n",
			mapAnnot(field.Name),
			escaVarName(field.Name),
			typeExpr(field.Type))
	}
	r.writef("}\n\n")
}

func (r *Reader) ReadFuncType(name string, value interface{}) {
	// TODO: method

	ty := reflect.TypeOf(value)
	escaName := escaVarName(name)
	r.writef("%s\nfunc %s(", mapAnnot(name), escaName)

	numIn := ty.NumIn()
	for i := 0; i < numIn; i++ {
		inTy := ty.In(i)
		r.writef("%s", typeExpr(inTy))
		if i+1 < numIn {
			r.writef(", ")
		}
	}

	r.writef(") -> ")
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
	r.writef("\n\n")
}

func (r *Reader) Output(path string) {
	fmt.Printf("output\n%s\n", r.buf.String())
}
