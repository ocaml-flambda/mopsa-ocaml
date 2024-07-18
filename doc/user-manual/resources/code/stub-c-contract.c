int error;

/*$
 * requires : _size > 0;
 * requires : valid_float(x);
 * requires : valid_ptr(buf) and offset(buf) + _size <= bytes(buf);
 * 
 * case "OK" {
 *    assigns : buf[0,_size);
 *    ensures : return >= 0 and return <= _size;
 *    ensures : forall int i in [return,_size): (buf[i])' == 0;
 * }
 *
 * case "error" {
 *    assigns : error;
 *    ensures : error' == 1;
 *    ensures : return == -1;
 * }
 */
int f(char* buf, int _size, double x);

void main() {
  char buf[256];
  f(buf, 256, 12.);
}
