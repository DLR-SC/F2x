import mathlib_ctypes as ml

assert ml.POWER(2, 3) == 8

for a, b in [(2, 1), (15, 5), (11, 6), (456, 123)]:
	d, x, y = ml.GCDIV(a, b)
	assert d == a * x + b * y
	print(d, a, x, b, y)

about = ml.ABOUT()
assert about.rstrip() == "Foo Bar Baz"
buffer = ml.BUFFER("Blubb")
assert buffer.rstrip() == "Blubb"

assert ml.SWAP(1, 2) == (2, 1)
ml.INFO("Foo Bar")
