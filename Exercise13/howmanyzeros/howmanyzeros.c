#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>


unsigned int cz (long n, bool is_inclusiv) {
  int zeros_tot = n == 0 ? 0 : 1;
  unsigned int offset = 1;

  while (true) {
    ldiv_t split = ldiv(n, offset);
    ldiv_t prefix = ldiv(split.quot, 10);

    if (prefix.quot == 0 ) {
      return zeros_tot;
    }
    if (prefix.rem != 0) {
      zeros_tot += prefix.quot * offset;
    }
    else {
      zeros_tot += (prefix.quot - 1) * offset + split.rem + is_inclusiv;
    }

    offset *= 10;
  }  
}


int main() {
  char buff[24];
  int sz = sizeof(buff);
  while(true) {
    fgets (buff, sz, stdin);
    long an, bn;
    sscanf(buff,"%li %li", &an, &bn);
    
    if (an == -1 && bn == -1) { // The only time that we are expecting a negative number is the stop
      break;
    }

    printf("%u \n", cz(bn, true) - cz(an, false));
    
  }

  return 0;
}
