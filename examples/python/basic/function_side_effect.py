def f(x):
  global i
  i = x
  return i
i = 1
j = i + f(1) + f(2) + i

