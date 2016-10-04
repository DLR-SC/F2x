import mathlib_ctypes as ml

assert ml.POWER(2, 3) == 8

for a, b in [(2, 1), (15, 5), (11, 6), (456, 123)]:
    d, x, y = ml.GCDIV(a, b)
    assert d == a * x + b * y
    print(d, a, x, b, y)

about = ml.ABOUT()
assert about.rstrip() == "Foo Bar Baz"
#buffer = ml.BUFFER("Blubb")
#assert buffer.rstrip() == "Blubb"

assert ml.SWAP(1, 2) == (2, 1)

ml.INFO("Foo Bar")

#curve = ml.CURVE()
#curve.NBITS = 1024
#assert curve.NBITS == 1024

#curve.COEFF[:] = [1.2, 2.3, 3.4, 4.5]
#ml.CURVEOUT(curve)

print("## 1")
point = ml.POINT()
point.X = 12.34
point.Y = 56.78

print("## 2")
point.CURVE.NBITS = 1024

print("## 3")
point.CURVE.COEFF[:] = [1.2, 2.3, 3.4, 4.5]

print("## 4")
print(point.CURVE.NBITS)
print(point.CURVE.COEFF[:])
print(point.X, point.Y)

print("## 5")
ml.CURVEOUT(point.CURVE)

print("## 6")

