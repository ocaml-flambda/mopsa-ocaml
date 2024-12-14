#include <stdio.h>

int main() {
  char* str = _mopsa_new_valid_string();
  char buf[1024];

  FILE* f = fopen(str, "rw");
  if (f == NULL)  return 0;
  
  while (_mopsa_rand_s8()) {
    switch (_mopsa_rand_s8()) {        
    case 0:
      fread(buf, 1, _mopsa_range_u32(0, sizeof(buf)), f);
      break;
    case 1:
      fwrite(buf, 1, _mopsa_range_u32(0, sizeof(buf)), f);
      break;
    case 2:
      _mopsa_assert(ftell(f) >= -1);
      break;
    case 3:
      _mopsa_assert(fseek(f, _mopsa_rand_s32(), _mopsa_range_s32(0, 2)) >= -1);
      break;
    case 4:
      rewind(f);
      break;
    case 5:
      feof(f);
      break;
    }      
  }
  fclose(f);
  
  return 0;
}
