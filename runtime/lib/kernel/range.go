package kernel

type Range struct {
	Begin int
	End   int
}

func CreateClosedRange(begin int, end int) Range {
	return Range{begin, end}
}

func CreateHalfOpenRange(begin int, end int) Range {
	return Range{begin, end - 1}
}
