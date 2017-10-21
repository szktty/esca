package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"reflect"
	"strings"
)

type Reader struct {
}

type Person struct {
	Name string
	Age  int
	File ast.File
}

func (p *Person) Description(s string, i int) string {
	return s
}

func ReadType(value interface{}) {
	ty := reflect.TypeOf(value)
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

func EscaVarName(name string) string {
	head := strings.ToLower(string(name[0]))
	if len(name) > 0 {
		return head + name[1:len(name)]
	} else {
		return head
	}
}

func ToEscaType(ty reflect.Type) string {
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
		return ty.Name()
	default:
		panic(fmt.Sprintf("not impl %s", ty))
	}
}

func TypeDecl(ty reflect.Type) string {
	buf := bytes.NewBufferString("")
	kind := ty.Kind()
	switch kind {
	case reflect.Struct:
		fmt.Fprintf(buf, "// %s.%s\n", ty.PkgPath(), ty.Name())
		fmt.Fprintf(buf, "struct %s {\n", ty.Name())
		numField := ty.NumField()
		for i := 0; i < numField; i++ {
			field := ty.Field(i)
			fmt.Fprintf(buf, "   var %s: %s\n",
				EscaVarName(field.Name),
				ToEscaType(field.Type))
		}
		buf.WriteString("}\n")

	default:
		buf.WriteString("error\n")
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
		fmt.Fprintf(buf, "%s", ToEscaType(inTy))
		if i+1 < numIn {
			fmt.Fprintf(buf, ", ")
		}
	}

	fmt.Fprintf(buf, ") -> ")
	numOut := ty.NumOut()
	if numOut == 0 {
		fmt.Fprintf(buf, "Void")
	} else if numOut == 1 {
		fmt.Fprintf(buf, "%s", ToEscaType(ty.Out(0)))
	} else {
		fmt.Fprintf(buf, "(")
		for i := 0; i < numOut; i++ {
			fmt.Fprintf(buf, "%s", ToEscaType(ty.Out(0)))
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

func main() {
	ty := reflect.TypeOf(Person{})
	ReadType(Person{})
	ReadType((&Person{}).Description)
	fmt.Printf(TypeDecl(ty))
	fmt.Printf("\n\n")
	fmt.Printf(MethodDecl("Person", "Description",
		reflect.TypeOf((&Person{}).Description)))
	fmt.Printf("\n\n")
}
