#include <stdio.h>
#include <stdlib.h>          /* for malloc, free */
#include <ctype.h>           /* for isupper, islower, tolower */
#include "porter_stemmer.h"

//static char * s;             /* buffer for words tobe stemmed */

#define INC 50               /* size units in which s is increased */
static int i_max = INC;      /* maximum offset in s */

#define LETTER(ch) (isupper(ch) || islower(ch))

#define TRUE 1
#define FALSE 0

using namespace inagist_stemmer;

void stemfile(inagist_stemmer::stemmer *z, FILE * f)
{
   char *s = (char *) malloc(i_max + 1);
   while(TRUE)
   {  int ch = getc(f);
      if (ch == EOF) return;
      if (LETTER(ch))
      {  int i = 0;
         while(TRUE)
         {  if (i == i_max)
            {  i_max += INC;
               s = (char *) realloc(s, i_max + 1);
            }
            ch = tolower(ch); /* forces lower case */

            s[i] = ch; i++;
            ch = getc(f);
            if (!LETTER(ch)) { ungetc(ch,f); break; }
         }
         s[stem(z, s, i - 1) + 1] = 0;
         /* the previous line calls the stemmer and uses its result to
            zero-terminate the string in s */
         printf("%s",s);
      }
      else putchar(ch);
   }
   free(s);
}

int main(int argc, char * argv[])
{  int i;

   inagist_stemmer::stemmer *z = create_stemmer();

   for (i = 1; i < argc; i++)
   {  FILE * f = fopen(argv[i],"r");
      if (f == 0) { fprintf(stderr,"File %s not found\n",argv[i]); exit(1); }
      stemfile(z, f);
   }
   free_stemmer(z);

   return 0;
}

