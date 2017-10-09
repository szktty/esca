package kernel

type Range struct {
	Begin int
	End   int
}

func ClosedRange(begin int, end int) Range {
	return Range{begin, end}
}

func HalfOpenRange(begin int, end int) Range {
	return Range{begin, end - 1}
}
