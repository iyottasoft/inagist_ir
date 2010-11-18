/* This is the Porter stemming algorithm, coded up as thread-safe ANSI C
   by the author.

   It may be be regarded as cononical, in that it follows the algorithm
   presented in

   Porter, 1980, An algorithm for suffix stripping, Program, Vol. 14,
   no. 3, pp 130-137,

   only differing from it at the points maked --DEPARTURE-- below.

   See also http://www.tartarus.org/~martin/PorterStemmer

   The algorithm as described in the paper could be exactly replicated
   by adjusting the points of DEPARTURE, but this is barely necessary,
   because (a) the points of DEPARTURE are definitely improvements, and
   (b) no encoding of the Porter stemmer I have seen is anything like
   as exact as this version, even with the points of DEPARTURE!

   You can compile it on Unix with 'gcc -O3 -o stem stem.c' after which
   'stem' takes a list of inputs and sends the stemmed equivalent to
   stdout.

   The algorithm as encoded here is particularly fast.

   Release 2 (the more old-fashioned, non-thread-safe version may be
   regarded as release 1.)
*/

// Balaji made the following changes to use Martin Porter's code in inagist
//
// 1. introduced namespace and compiled the code with g++ compiler
// 2. moved the definitions to a header file

namespace porter_stemmer {

/* stemmer is a structure for a few local bits of data,
*/

struct _stemmer {
   char * b;       /* buffer for word to be stemmed */
   int k;          /* offset to the end of the string */
   int j;          /* a general offset into the string */
};

typedef struct _stemmer stemmer;

extern stemmer * create_stemmer(void);
extern void free_stemmer(stemmer * z);

extern int stem(stemmer * z, char * b, int k);
extern int stem(char *in_word, int in_len, char *out_word);
extern int stemfile(char* file_name);

int init_stemmer();
int free_stemmer();

} // namespace porter_stemmer
