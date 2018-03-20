test = "nok"
try:
    t4 = (1, 2)
    (x, y, z) = t4
except ValueError:
    test = "ok"
