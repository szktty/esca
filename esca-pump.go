package main

import (
	"bytes"
	"flag"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"strings"
)

var helpFlag = flag.Bool("h", false, "Print this message")
var verboseFlag = flag.Bool("v", false, "Print verbose message")
var outFlag = flag.String("o", "", "Output file")

func verbosef(format string, a ...interface{}) {
	if *verboseFlag {
		fmt.Printf("# ")
		fmt.Printf(format, a...)
		fmt.Printf("\n")
	}
}

func errorf(format string, a ...interface{}) {
	fmt.Printf("Error: ")
	fmt.Printf(format, a...)
	fmt.Printf("\n")
}

func printHelp() {
	fmt.Printf("Usage: %s [options] PATH\n", os.Args[0])
	flag.PrintDefaults()
}

func findDir(path string) []string {
	goDirStr, _ := os.LookupEnv("GOPATH")
	fmt.Printf("GOPATH=%s\n", goDirStr)
	var dirPath string
	for _, baseDir := range strings.Split(goDirStr, ":") {
		basePath := filepath.Join(baseDir, "src", path)
		_, err := os.Stat(basePath)
		if err == nil {
			fmt.Printf("found %s\n", basePath)
			dirPath = basePath
			break
		}
	}

	dir, _ := os.Open(dirPath)
	names, _ := dir.Readdirnames(0)
	srcs := []string{}
	for _, name := range names {
		if strings.HasSuffix(name, ".go") {
			srcs = append(srcs, filepath.Join(dirPath, name))
		}
	}
	return srcs
}

func isPublic(name string) bool {
	switch name {
	case "", "bool", "string", "error",
		"int", "int8", "int16", "int32", "int64",
		"uint", "uint8", "uint16", "uint32", "uint64", "uintptr",
		"byte", "rune", "float32", "float64", "complex64", "complex128":
		return true
	default:
		return name[0] == strings.ToUpper(name)[0]
	}
}

func isPublicFieldList(list *ast.FieldList) bool {
	fields := list.List
	for i := 0; i < len(fields); i++ {
		field := fields[i]
		_, fieldName := fieldTypeName(field)
		if !isPublic(fieldName) {
			return false
		}
	}
	return true
}

func exprTypeName(expr interface{}) string {
	fmt.Printf("expr type name: %s\n", reflect.TypeOf(expr))
	if ident, ok := expr.(*ast.Ident); ok {
		return ident.Name
	} else if starExpr, ok := expr.(*ast.StarExpr); ok {
		return exprTypeName(starExpr)
	} else if selExpr, ok := expr.(*ast.SelectorExpr); ok {
		return selExpr.Sel.Name
	} else if _, ok := expr.(*ast.InterfaceType); ok {
		return ""
	}
	panic(fmt.Sprintf("expr %s", expr))
}

func fieldTypeName(field *ast.Field) (star bool, name string) {
	fmt.Printf("field type name: %s\n", reflect.TypeOf(field.Type))
	if starExpr, ok := field.Type.(*ast.StarExpr); ok {
		return true, exprTypeName(starExpr.X)
	} else {
		return false, exprTypeName(field.Type)
	}
}

type Package struct {
	name            string
	path            string
	typeSpecs       []*ast.TypeSpec
	structTypeSpecs []*ast.TypeSpec
	funcDecls       []*ast.FuncDecl
}

func (pkg *Package) parseDecl(file *ast.File, decl ast.Decl) {
	if genDecl, ok := decl.(*ast.GenDecl); ok {
		switch genDecl.Tok {
		case token.CONST:
			fmt.Printf("const decl\n")

		case token.TYPE:
			fmt.Printf("type decl\n")
			typeSpec, ok := genDecl.Specs[0].(*ast.TypeSpec)
			if !ok {
				panic("fail cast to type spec")
			}
			fmt.Printf("type decl? %s, %s, %s\n", typeSpec.Name.Name, typeSpec, ok)
			if !isPublic(typeSpec.Name.Name) {
				break
			}

			pkg.typeSpecs = append(pkg.typeSpecs, typeSpec)
			structType, ok := typeSpec.Type.(*ast.StructType)
			if ok {
				pkg.structTypeSpecs = append(pkg.structTypeSpecs, typeSpec)
				fmt.Printf("struct type %s\n", structType)
			}

		default:
			fmt.Printf("other decl\n")
			break
		}
		return
	} else if funcDecl, ok := decl.(*ast.FuncDecl); ok {
		pkg.parseFuncDecl(funcDecl)
	}
}

func (pkg *Package) parseFuncDecl(decl *ast.FuncDecl) {
	name := decl.Name.Name
	fmt.Printf("func decl: %s\n", name)
	if !isPublic(name) {
		fmt.Printf("not public name\n")
		return
	} else if decl.Recv != nil {
		_, recvName := fieldTypeName(decl.Recv.List[0])
		if !isPublic(recvName) {
			fmt.Printf("not public recv name\n")
			return
		}
	}

	if decl.Type.Params != nil {
		if !isPublicFieldList(decl.Type.Params) {
			fmt.Printf("not public params\n")
			return
		}
	}

	if decl.Type.Results != nil {
		if !isPublicFieldList(decl.Type.Results) {
			fmt.Printf("not public results\n")
			return
		}
	}

	pkg.funcDecls = append(pkg.funcDecls, decl)
}

func (pkg *Package) parseFile(importPath string, filePath string) {
	src, err := ioutil.ReadFile(filePath)
	if err != nil {
		panic("not found")
	}
	fileSet := token.NewFileSet()
	file, parseErr := parser.ParseFile(fileSet, filePath, src, parser.AllErrors)
	if parseErr != nil {
		panic(parseErr)
	}

	pkg.path = importPath
	pkg.name = file.Name.Name

	fmt.Printf("package name: %s\n", file.Name.Name)
	fmt.Printf("num decls: %d\n", len(file.Decls))
	for _, decl := range file.Decls {
		pkg.parseDecl(file, decl)
	}
}

func (pkg *Package) generate(out string) {
	buf := bytes.NewBufferString("package main\n\n")

	pkg.path = strings.TrimSuffix(pkg.path, "/")
	fmt.Fprintf(buf, "import \"github.com/szktty/esca/runtime/pump\"\n")
	fmt.Fprintf(buf, "import \"%s\"\n\n", pkg.path)

	fmt.Fprintf(buf, "func main() {\n")
	fmt.Fprintf(buf, "    reader := pump.NewReader(\"%s\", \"%s\")\n",
		pkg.path, pkg.name)

	// struct type
	for _, typeSpec := range pkg.structTypeSpecs {
		fmt.Fprintf(buf, "    reader.ReadStructType(%s.%s{})\n",
			pkg.name, typeSpec.Name.Name)
	}

	// func decls
	for _, decl := range pkg.funcDecls {
		params := decl.Type.Params.List
		numParams := len(params)
		if decl.Recv != nil {
			recv := decl.Recv.List[0]
			star, recvName := fieldTypeName(recv)
			var path string
			if star {
				path = fmt.Sprintf("*%s.%s", pkg.name, recvName)
			} else {
				path = fmt.Sprintf("%s.%s", pkg.name, recvName)
			}
			fmt.Fprintf(buf, "    reader.ReadMethodType(\"%s\", (%s).%s",
				decl.Name.Name, path, decl.Name.Name)
		} else {
			fmt.Fprintf(buf, "    reader.ReadFuncType(\"%s\", %s.%s",
				decl.Name.Name, pkg.name, decl.Name.Name)
		}

		fmt.Fprintf(buf, ", []string{")
		for i := 0; i < numParams; i++ {
			numNames := len(params[i].Names)
			for j := 0; j < numNames; j++ {
				fmt.Fprintf(buf, "\"%s\", ", params[i].Names[j])
			}
		}
		fmt.Fprintf(buf, "})\n")
	}

	fmt.Fprintf(buf, "    reader.Output(\"%s.esca\")\n", pkg.name)
	fmt.Fprintf(buf, "}\n")

	fmt.Printf("\n\n%s\n", buf.String())

	file, err := os.Create(out)
	if err != nil {
		errorf("failed create a file", err)
		os.Exit(1)
	}
	if _, err = file.WriteString(buf.String()); err != nil {
		errorf("failed write")
		os.Exit(1)
	}
	file.Close()
}

func main() {
	flag.Parse()

	if len(os.Args) <= 1 || *helpFlag {
		printHelp()
		os.Exit(1)
	}

	path := os.Args[1]
	pkg := Package{}
	if strings.HasSuffix(path, ".go") {
		// TODO
		pkg.parseFile(path, path)
	} else {
		for _, src := range findDir(path) {
			fmt.Printf("parse %s\n", src)
			if strings.HasSuffix(src, "_test.go") {
				fmt.Printf("skip %s\n", src)
				continue
			}
			pkg.parseFile(path, src)
		}
	}

	out := fmt.Sprintf("pump_temp_%s.go", pkg.name)
	pkg.generate(out)
	cmd := exec.Command("go", "run", out)
	if err := cmd.Run(); err != nil {
		errorf("failed getting types (%s)", err)
		os.Exit(1)
	}
}
