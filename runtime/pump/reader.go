package pump

import (
	"bytes"
	"fmt"
	"reflect"
	"strings"
)

func EscaVarName(name string) string {
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

func EscaTypeName(ty reflect.Type) string {
	switch ty.Kind() {
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
	case reflect.Struct:
		return fmt.Sprintf("%s", ty.Name())
	case reflect.Interface:
		fmt.Printf("interface %s\n", ty)
		return ty.Name()
	case reflect.Ptr:
		// TODO
		fmt.Printf("ptr %s\n", ty.Elem())
		return fmt.Sprintf("*%s", EscaTypeName(ty.Elem()))
	default:
		panic(fmt.Sprintf("not impl %d (%s)", ty.Kind(), ty))
	}
}

func TypeDecl(name string, ty reflect.Type) string {
	annot := fmt.Sprintf("#name(\"%s\")", name)
	escaName := EscaVarName(name)
	buf := bytes.NewBufferString("")
	kind := ty.Kind()
	switch kind {
	case reflect.Struct:
		fmt.Fprintf(buf, "%s\nstruct %s {\n", annot, ty.Name())
		numField := ty.NumField()
		for i := 0; i < numField; i++ {
			field := ty.Field(i)
			fmt.Fprintf(buf, "   var %s: %s\n",
				EscaVarName(field.Name),
				EscaTypeName(field.Type))
		}
		buf.WriteString("}\n")

	case reflect.Func:
		// TODO: method

		fmt.Fprintf(buf, "%s\nfunc %s(", annot, escaName)

		numIn := ty.NumIn()
		for i := 0; i < numIn; i++ {
			inTy := ty.In(i)
			fmt.Fprintf(buf, "%s", EscaTypeName(inTy))
			if i+1 < numIn {
				fmt.Fprintf(buf, ", ")
			}
		}

		fmt.Fprintf(buf, ") -> ")
		numOut := ty.NumOut()
		if numOut == 0 {
			fmt.Fprintf(buf, "Void")
		} else if numOut == 1 {
			fmt.Fprintf(buf, "%s", EscaTypeName(ty.Out(0)))
		} else {
			fmt.Fprintf(buf, "(")
			for i := 0; i < numOut; i++ {
				fmt.Fprintf(buf, "%s", EscaTypeName(ty.Out(i)))
				if i+1 < numOut {
					fmt.Fprintf(buf, ", ")
				}
			}
			fmt.Fprintf(buf, ")")
		}

	default:
		buf.WriteString("not impl kind\n")
	}

	return buf.String()
}

func basicFuncDecl(recv string, name string, ty reflect.Type) string {
	buf := bytes.NewBufferString("func ")
	if recv != "" {
		fmt.Fprintf(buf, "%s.%s", recv, name)
	} else {
		fmt.Fprintf(buf, "%s", recv, name)
	}
	fmt.Fprintf(buf, "(")

	numIn := ty.NumIn()
	for i := 0; i < numIn; i++ {
		inTy := ty.In(i)
		fmt.Fprintf(buf, "%s", EscaTypeName(inTy))
		if i+1 < numIn {
			fmt.Fprintf(buf, ", ")
		}
	}

	fmt.Fprintf(buf, ") -> ")
	numOut := ty.NumOut()
	if numOut == 0 {
		fmt.Fprintf(buf, "Void")
	} else if numOut == 1 {
		fmt.Fprintf(buf, "%s", EscaTypeName(ty.Out(0)))
	} else {
		fmt.Fprintf(buf, "(")
		for i := 0; i < numOut; i++ {
			fmt.Fprintf(buf, "%s", EscaTypeName(ty.Out(0)))
			if i+1 < numOut {
				fmt.Fprintf(buf, ", ")
			}
		}
		fmt.Fprintf(buf, ")")
	}
	fmt.Fprintf(buf, " ")
	return buf.String()
}

func FuncDecl(name string, ty reflect.Type) string {
	return basicFuncDecl("", name, ty)
}

func MethodDecl(recv string, method string, ty reflect.Type) string {
	return basicFuncDecl(recv, method, ty)
}

type Reader struct {
	Path string // package path
	Name string // package name
	buf  *bytes.Buffer
}

func NewReader(path string, name string) *Reader {
	r := &Reader{Path: path, Name: name, buf: bytes.NewBufferString("")}
	r.Writef("package %s, %s\n", path, name)
	return r
}

func (r *Reader) Writef(format string, a ...interface{}) {
	fmt.Fprintf(r.buf, format, a...)
}

func (r *Reader) ReadFuncType(name string, value interface{}) {
	ty := reflect.TypeOf(value)
	r.debugType(ty)
	r.Writef("%s\n\n", TypeDecl(name, ty))
}

func (r *Reader) debugType(ty reflect.Type) {
	fmt.Printf("name: %s\n", ty.Name())
	fmt.Printf("package path: %s\n", ty.PkgPath())
	fmt.Printf("align: %d\n", ty.Align())
	kind := ty.Kind()
	fmt.Printf("kind: %s\n", kind.String())
	switch kind {
	case reflect.Struct:
		numField := ty.NumField()
		fmt.Printf("num fields: %d\n", numField)
		for i := 0; i < numField; i++ {
			field := ty.Field(i)
			fmt.Printf("    field %d:\n", i)
			fmt.Printf("        package path: %s\n", field.PkgPath)
			fmt.Printf("        name: %s\n", field.Name)
			fmt.Printf("        type: %s (%s)\n", field.Type, field.Type.PkgPath())
		}

		numMethod := ty.NumMethod()
		fmt.Printf("num methods: %d\n", numMethod)
		for i := 0; i < numMethod; i++ {
			meth := ty.Method(i)
			fmt.Printf("    method %d:\n", i)
			fmt.Printf("        package path: %s\n", meth.PkgPath)
			fmt.Printf("        name: %s\n", meth.Name)
			fmt.Printf("        type: %s\n", meth.Type)
			fmt.Printf("        recv: %s\n", meth.Func)
		}
	case reflect.Func:
		fmt.Printf("func %s\n", ty.Name())
		fmt.Printf("    in: %d\n", ty.NumIn())
		fmt.Printf("    out: %d\n", ty.NumOut())
	default:
		break
	}
	fmt.Printf("\n")
}

func (r *Reader) Output(path string) {
	fmt.Printf("output\n%s\n", r.buf.String())
}
