package kernel

type List struct {
	Value interface{}
	Next  *List
}

func New() *List {
	return &List{Value: 1, Next: nil}
}
