package kernel

type Option struct {
	Value interface{}
}

var None = Option{}

func Some(value interface{}) Option {
	return Option{Value: value}
}
