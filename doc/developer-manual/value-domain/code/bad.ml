let (negx, zerox, posx) = get_basic_properties x in
let (negy, zeroy, posy) = get_basic_properties y in
from_basic_properties
  ((negx && posy) || (negy && posx))
  (zerox || zeroy || negy)
  ((posx && posy) || (negx && negy))
