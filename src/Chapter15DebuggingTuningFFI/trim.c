#include <stdio.h>
#include <string.h>

int trim(char[]);

int trim(char s[]) {
  int n;

  for (n = strlen(s)-1; n >= 0; n--)
    if (s[n] != ' ' && s[n] != '\t' && s[n] != '\n')
      break;
  s[n+1] = '\0';
  return n;
}


/* int main(void) { */
/*   char s[] = "      messy line   \n "; */
/*   int l1 = strlen(s); */
/*   printf("length before trim: %d \n", l1); */
/*   printf("%s\n", s); */
/*   int n = trim(s); */
/*   printf("%d\n", n); */
/*   printf("%s\n", s); */
/*   int l2 = strlen(s); */
/*   printf("length after trim: %d \n", l2); */
  
/* } */

// https://stackoverflow.com/questions/5764298/compiling-c-source-code-without-a-main-function

// note that trim.o is non-executable
