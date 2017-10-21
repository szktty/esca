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
	"path/filepath"
	"strings"
)

var helpFlag = flag.Bool("h", false, "Print this message")
var verboseFlag = flag.Bool("v", false, "Print verbose message")
var outFlag = flag.String("o", "", "Output file")

func verbose(format string, a ...interface{}) (n int, err error) {
	if *verboseFlag {
		return fmt.Printf(format, a...)
	} else {
		return 0, nil
	}
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
	return name[0] == strings.ToUpper(name)[0]
}

type packageInfo struct {
	name  string
	path  string
	decls []string
}

func createPackageInfo() packageInfo {
	return packageInfo{decls: []string{}}
}

func (pkg *packageInfo) addDecl(name string) {
	for _, cur := range pkg.decls {
		if cur == name {
			return
		}
	}
	pkg.decls = append(pkg.decls, name)
}

func (pkg *packageInfo) parseDecl(file *ast.File, decl ast.Decl) {
	structDecl, ok := decl.(*ast.GenDecl)
	if ok {
		switch structDecl.Tok {
		case token.CONST:
			fmt.Printf("const decl\n")
			break
		case token.TYPE:
			fmt.Printf("type decl\n")
			break
		default:
			break
		}
		return
	}

	funcDecl, ok := decl.(*ast.FuncDecl)
	if ok {
		name := funcDecl.Name.Name
		fmt.Printf("func decl: %s\n", name)
		if isPublic(name) {
			pkg.addDecl(name)
		}
		return
	}
}

func (pkg *packageInfo) parseFile(importPath string, filePath string) {
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

func (pkg *packageInfo) generate(out string) {
	buf := bytes.NewBufferString("package main\n\n")

	pkg.path = strings.TrimSuffix(pkg.path, "/")
	fmt.Fprintf(buf, "import \"github.com/szktty/esca/runtime/pump\"\n")
	fmt.Fprintf(buf, "import \"%s\"\n\n", pkg.path)

	fmt.Fprintf(buf, "func main() {\n")
	fmt.Fprintf(buf, "    reader := pump.NewReader()\n")
	for _, decl := range pkg.decls {
		fmt.Fprintf(buf, "    reader.ReadType(%s.%s)\n", pkg.name, decl)
	}
	fmt.Fprintf(buf, "    reader.Output(\"%s\")\n", out)
	fmt.Fprintf(buf, "}\n")

	fmt.Printf("\n\n%s\n", buf.String())
}

func main() {
	flag.Parse()

	if len(os.Args) <= 1 || *helpFlag {
		printHelp()
		os.Exit(1)
	}

	path := os.Args[1]
	pkg := createPackageInfo()
	if strings.HasSuffix(path, ".go") {
		// TODO
		pkg.parseFile(path, path)
	} else {
		for _, src := range findDir(path) {
			fmt.Printf("parse %s\n", src)
			pkg.parseFile(path, src)
		}
	}

	pkg.generate("temp-pump.go")
}
