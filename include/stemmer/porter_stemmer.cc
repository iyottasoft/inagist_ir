#include "porter_stemmer.h"
#include <stdlib.h>  /* for malloc, free */
#include <string.h>  /* for memcmp, memmove */
#include <stdio.h>
#include <ctype.h>   /* for isupper, islower, tolower */

/* The main part of the stemming algorithm starts here.
*/

namespace porter_stemmer {

#define INC 50               /* size units in which s is increased */
static int i_max = INC;      /* maximum offset in s */

#define LETTER(ch) (isupper(ch) || islower(ch))
#define TRUE 1
#define FALSE 0

static stemmer *g_stemmer;
static char g_word[1024];

/* Member b is a buffer holding a word to be stemmed. The letters are in
   b[0], b[1] ... ending at b[z->k]. Member k is readjusted downwards as
   the stemming progresses. Zero termination is not in fact used in the
   algorithm.

   Note that only lower case sequences are stemmed. Forcing to lower case
   should be done before stem(...) is called.


   Typical usage is:

       stemmer *z = create_stemmer();
       char b[] = "pencils";
       int res = stem(z, b, 6);
           /- stem the 7 characters of b[0] to b[6]. The result, res,
              will be 5 (the 's' is removed). -/
       free_stemmer(z);
*/


extern stemmer* create_stemmer(void)
{
    return (stemmer *) malloc(sizeof(stemmer));
    /* assume malloc succeeds */
}

extern void free_stemmer(stemmer* z)
{
    free(z);
}


/* cons(z, i) is TRUE <=> b[i] is a consonant. ('b' means 'z->b', but here
   and below we drop 'z->' in comments.
*/

static int cons(stemmer* z, int i)
{  switch (z->b[i])
   {  case 'a': case 'e': case 'i': case 'o': case 'u': return FALSE;
      case 'y': return (i == 0) ? TRUE : !cons(z, i - 1);
      default: return TRUE;
   }
}

/* m(z) measures the number of consonant sequences between 0 and j. if c is
   a consonant sequence and v a vowel sequence, and <..> indicates arbitrary
   presence,

      <c><v>       gives 0
      <c>vc<v>     gives 1
      <c>vcvc<v>   gives 2
      <c>vcvcvc<v> gives 3
      ....
*/

static int m(stemmer* z)
{  int n = 0;
   int i = 0;
   int j = z->j;
   while(TRUE)
   {  if (i > j) return n;
      if (! cons(z, i)) break; i++;
   }
   i++;
   while(TRUE)
   {  while(TRUE)
      {  if (i > j) return n;
            if (cons(z, i)) break;
            i++;
      }
      i++;
      n++;
      while(TRUE)
      {  if (i > j) return n;
         if (! cons(z, i)) break;
         i++;
      }
      i++;
   }
}

/* vowelinstem(z) is TRUE <=> 0,...j contains a vowel */

static int vowelinstem(stemmer* z)
{
   int j = z->j;
   int i; for (i = 0; i <= j; i++) if (! cons(z, i)) return TRUE;
   return FALSE;
}

/* doublec(z, j) is TRUE <=> j,(j-1) contain a double consonant. */

static int doublec(stemmer* z, int j)
{
   char* b = z->b;
   if (j < 1) return FALSE;
   if (b[j] != b[j - 1]) return FALSE;
   return cons(z, j);
}

/* cvc(z, i) is TRUE <=> i-2,i-1,i has the form consonant - vowel - consonant
   and also if the second c is not w,x or y. this is used when trying to
   restore an e at the end of a short word. e.g.

      cav(e), lov(e), hop(e), crim(e), but
      snow, box, tray.

*/

static int cvc(stemmer* z, int i)
{  if (i < 2 || !cons(z, i) || cons(z, i - 1) || !cons(z, i - 2)) return FALSE;
   {  int ch = z->b[i];
      if (ch  == 'w' || ch == 'x' || ch == 'y') return FALSE;
   }
   return TRUE;
}

/* ends(z, s) is TRUE <=> 0,...k ends with the string s. */

static int ends(stemmer* z, char* s)
{  int length = s[0];
   char* b = z->b;
   int k = z->k;
   if (s[length] != b[k]) return FALSE; /* tiny speed-up */
   if (length > k + 1) return FALSE;
   if (memcmp(b + k - length + 1, s + 1, length) != 0) return FALSE;
   z->j = k-length;
   return TRUE;
}

/* setto(z, s) sets (j+1),...k to the characters in the string s, readjusting
   k. */

static void setto(stemmer* z, char* s)
{  int length = s[0];
   int j = z->j;
   memmove(z->b + j + 1, s + 1, length);
   z->k = j+length;
}

/* r(z, s) is used further down. */

static void r(stemmer* z, char* s) { if (m(z) > 0) setto(z, s); }

/* step1ab(z) gets rid of plurals and -ed or -ing. e.g.

       caresses  ->  caress
       ponies    ->  poni
       ties      ->  ti
       caress    ->  caress
       cats      ->  cat

       feed      ->  feed
       agreed    ->  agree
       disabled  ->  disable

       matting   ->  mat
       mating    ->  mate
       meeting   ->  meet
       milling   ->  mill
       messing   ->  mess

       meetings  ->  meet

*/

static void step1ab(stemmer* z)
{
   char* b = z->b;
   if (b[z->k] == 's')
   {  if (ends(z, (char *) "\04" "sses")) z->k -= 2; else
      if (ends(z, (char *) "\03" "ies")) setto(z, (char *) "\01" "i"); else
      if (b[z->k - 1] != 's') z->k--;
   }
   if (ends(z, (char *) "\03" "eed")) { if (m(z) > 0) z->k--; } else
   if ((ends(z, (char *) "\02" "ed") || ends(z, (char *) "\03" "ing")) && vowelinstem(z))
   {  z->k = z->j;
      if (ends(z, (char *) "\02" "at")) setto(z, (char *) "\03" "ate"); else
      if (ends(z, (char *) "\02" "bl")) setto(z, (char *) "\03" "ble"); else
      if (ends(z, (char *) "\02" "iz")) setto(z, (char *) "\03" "ize"); else
      if (doublec(z, z->k))
      {  z->k--;
         {  int ch = b[z->k];
            if (ch == 'l' || ch == 's' || ch == 'z') z->k++;
         }
      }
      else if (m(z) == 1 && cvc(z, z->k)) setto(z, (char *) "\01" "e");
   }
}

/* step1c(z) turns terminal y to i when there is another vowel in the stem. */

static void step1c(stemmer* z)
{
   if (ends(z, (char *) "\01" "y") && vowelinstem(z)) z->b[z->k] = 'i';
}


/* step2(z) maps double suffices to single ones. so -ization ( = -ize plus
   -ation) maps to -ize etc. note that the string before the suffix must give
   m(z) > 0. */

static void step2(stemmer* z) { switch (z->b[z->k-1])
{
   case 'a': if (ends(z, (char *) "\07" "ational")) { r(z, (char *) "\03" "ate"); break; }
             if (ends(z, (char *) "\06" "tional")) { r(z, (char *) "\04" "tion"); break; }
             break;
   case 'c': if (ends(z, (char *) "\04" "enci")) { r(z, (char *) "\04" "ence"); break; }
             if (ends(z, (char *) "\04" "anci")) { r(z, (char *) "\04" "ance"); break; }
             break;
   case 'e': if (ends(z, (char *) "\04" "izer")) { r(z, (char *) "\03" "ize"); break; }
             break;
   case 'l': if (ends(z, (char *) "\03" "bli")) { r(z, (char *) "\03" "ble"); break; } /*-DEPARTURE-*/

 /* To match the published algorithm, replace this line with
    case 'l': if (ends(z, "\04" "abli")) { r(z, "\04" "able"); break; } */

             if (ends(z, (char *) "\04" "alli")) { r(z, (char *) "\02" "al"); break; }
             if (ends(z, (char *) "\05" "entli")) { r(z, (char *) "\03" "ent"); break; }
             if (ends(z, (char *) "\03" "eli")) { r(z, (char *) "\01" "e"); break; }
             if (ends(z, (char *) "\05" "ousli")) { r(z, (char *) "\03" "ous"); break; }
             break;
   case 'o': if (ends(z, (char *) "\07" "ization")) { r(z, (char *) "\03" "ize"); break; }
             if (ends(z, (char *) "\05" "ation")) { r(z, (char *) "\03" "ate"); break; }
             if (ends(z, (char *) "\04" "ator")) { r(z, (char *) "\03" "ate"); break; }
             break;
   case 's': if (ends(z, (char *) "\05" "alism")) { r(z, (char *) "\02" "al"); break; }
             if (ends(z, (char *) "\07" "iveness")) { r(z, (char *) "\03" "ive"); break; }
             if (ends(z, (char *) "\07" "fulness")) { r(z, (char *) "\03" "ful"); break; }
             if (ends(z, (char *) "\07" "ousness")) { r(z, (char *) "\03" "ous"); break; }
             break;
   case 't': if (ends(z, (char *) "\05" "aliti")) { r(z, (char *) "\02" "al"); break; }
             if (ends(z, (char *) "\05" "iviti")) { r(z, (char *) "\03" "ive"); break; }
             if (ends(z, (char *) "\06" "biliti")) { r(z, (char *) "\03" "ble"); break; }
             break;
   case 'g': if (ends(z, (char *) "\04" "logi")) { r(z, (char *) "\03" "log"); break; } /*-DEPARTURE-*/

 /* To match the published algorithm, delete this line */

} }

/* step3(z) deals with -ic-, -full, -ness etc. similar strategy to step2. */

static void step3(stemmer* z) { switch (z->b[z->k])
{
   case 'e': if (ends(z, (char *) "\05" "icate")) { r(z, (char *) "\02" "ic"); break; }
             if (ends(z, (char *) "\05" "ative")) { r(z, (char *) "\00" ""); break; }
             if (ends(z, (char *) "\05" "alize")) { r(z, (char *) "\02" "al"); break; }
             break;
   case 'i': if (ends(z, (char *) "\05" "iciti")) { r(z, (char *) "\02" "ic"); break; }
             break;
   case 'l': if (ends(z, (char *) "\04" "ical")) { r(z, (char *) "\02" "ic"); break; }
             if (ends(z, (char *) "\03" "ful")) { r(z, (char *) "\00" ""); break; }
             break;
   case 's': if (ends(z, (char *) "\04" "ness")) { r(z, (char *) "\00" ""); break; }
             break;
} }

/* step4(z) takes off -ant, -ence etc., in context <c>vcvc<v>. */

static void step4(stemmer* z)
{  switch (z->b[z->k-1])
   {  case 'a': if (ends(z, (char *) "\02" "al")) break; return;
      case 'c': if (ends(z, (char *) "\04" "ance")) break;
                if (ends(z, (char *) "\04" "ence")) break; return;
      case 'e': if (ends(z, (char *) "\02" "er")) break; return;
      case 'i': if (ends(z, (char *) "\02" "ic")) break; return;
      case 'l': if (ends(z, (char *) "\04" "able")) break;
                if (ends(z, (char *) "\04" "ible")) break; return;
      case 'n': if (ends(z, (char *) "\03" "ant")) break;
                if (ends(z, (char *) "\05" "ement")) break;
                if (ends(z, (char *) "\04" "ment")) break;
                if (ends(z, (char *) "\03" "ent")) break; return;
      case 'o': if (ends(z, (char *) "\03" "ion") && (z->b[z->j] == 's' || z->b[z->j] == 't')) break;
                if (ends(z, (char *) "\02" "ou")) break; return;
                /* takes care of -ous */
      case 's': if (ends(z, (char *) "\03" "ism")) break; return;
      case 't': if (ends(z, (char *) "\03" "ate")) break;
                if (ends(z, (char *) "\03" "iti")) break; return;
      case 'u': if (ends(z, (char *) "\03" "ous")) break; return;
      case 'v': if (ends(z, (char *) "\03" "ive")) break; return;
      case 'z': if (ends(z, (char *) "\03" "ize")) break; return;
      default: return;
   }
   if (m(z) > 1) z->k = z->j;
}

/* step5(z) removes a final -e if m(z) > 1, and changes -ll to -l if
   m(z) > 1. */

static void step5(stemmer* z)
{
   char* b = z->b;
   z->j = z->k;
   if (b[z->k] == 'e')
   {  int a = m(z);
      if (a > 1 || (a == 1 && !cvc(z, z->k - 1))) z->k--;
   }
   if (b[z->k] == 'l' && doublec(z, z->k) && m(z) > 1) z->k--;
}

/* In stem(z, b, k), b is a char pointer, and the string to be stemmed is
   from b[0] to b[k] inclusive.  Possibly b[k+1] == '\0', but it is not
   important. The stemmer adjusts the characters b[0] ... b[k] and returns
   the new end-point of the string, k'. Stemming never increases word
   length, so 0 <= k' <= k.
*/

extern int stem(stemmer* z, char* b, int k)
{
   if (k <= 1) return k; /*-DEPARTURE-*/
   z->b = b; z->k = k; /* copy the parameters into z */

   /* With this line, strings of length 1 or 2 don't go through the
      stemming process, although no mention is made of this in the
      published algorithm. Remove the line to match the published
      algorithm. */

   step1ab(z); step1c(z); step2(z); step3(z); step4(z); step5(z);
   return z->k;
}

int init_stemmer()
{
  g_stemmer = create_stemmer();
  memset(g_word, 0, 1024);
  return 0;
}

extern int stem(char* in_word, int in_len, char* out_word)
{
  if (NULL == g_stemmer) {
    printf("ERROR: Stemmer not initialized\n");
    return -1;
  }

  if ((in_len < 1) || (in_len > 255))
    return -1;
  else
    memcpy(g_word, in_word, in_len);

  int len = 0;
  len = stem(g_stemmer, g_word, in_len-1);
  if (len != in_len) {
     memcpy(out_word, g_word, len+1);
     out_word[len + 1] = '\0';
     return len;
  }
  
  return 0;
}

int stemfile(char* file_name)
{
  if (!file_name)
    return -1;

  FILE* f = fopen(file_name,"r");
  if (f == 0)
    return -1;
  char *s = (char *) malloc(i_max + 1);
  while(TRUE) {
    int ch = getc(f);
    if (ch == EOF)
      break;
    if (LETTER(ch)) {
      int i = 0;
      while(TRUE) {
        if (i == i_max) {
          i_max += INC;
          s = (char *) realloc(s, i_max + 1);
        }
        ch = tolower(ch); /* forces lower case */

        s[i] = ch; i++;
        ch = getc(f);
        if (!LETTER(ch)) {
          ungetc(ch,f); break;
        }
      }
      s[stem(g_stemmer, s, i - 1) + 1] = 0;
      /* the previous line calls the stemmer and uses its result to
         zero-terminate the string in s */
      printf("%s",s);
    } else {
      putchar(ch);
    }
  }
  fclose(f);
  free(s);
  s = NULL;

  return 0;
}

int free_stemmer()
{
  free_stemmer(g_stemmer);
  g_stemmer = NULL;
  memset(g_word, 0, 1024);
  return 0;
}

} // namespace porter_stemmer

/*--------------------stemmer definition ends here------------------------*/

