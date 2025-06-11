#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

int cz (long n) {
  int zeros_tot = 0;
  int offset = 0;

  while (1) {
    ldiv_t split = ldiv(n, offset);
    ldiv_t prefix = ldiv(split.quot, 10);

    if (prefix.quot == 0 ) {
      return zeros_tot;
    }
    if (prefix.rem != 0) {
      zeros_tot += prefix.quot * offset;
    }
    else {
      zeros_tot += (prefix.quot - 1) * offset + split.rem + 1;
    }

    offset *= 10;
  }  
}


int main() {
  char buff[24];
  int sz = sizeof(buff);
  while(1) {
    fgets (buff, sz, stdin);
    long an, bn;
    sscanf(buff,"%li %li", &an, &bn);
    
    if (an == -1 && bn == -1) { // The only time that we are expecting a negative number is the stop
      break;
    }

    printf("%i", cz(bn));
    
  }

  return 0;
}
