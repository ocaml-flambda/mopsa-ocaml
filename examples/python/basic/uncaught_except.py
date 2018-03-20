try:
  i = 1
  raise TypeError
except SyntaxError:
  i = 2

j = i
