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

print("## 1")
curve = ml.CURVE()
print("## 2")
curve.NBITS = 1024
print("## 3")
assert curve.NBITS == 1024
print("## 4")
print(curve.COEFF)
print("## 5")
print(curve.COEFF[0])
print("## 6")
curve.COEFF[:] = [1.2, 2.3, 3.4, 4.5]
print("## 7")
ml.CURVEOUT(curve)
print("## 8")
