#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#define MAXMEMORY (5 * MAXINPUT)
#define MAXINPUT 100

static char *exp;
// static Environment *env;
// static void *val;
// static char continue;
// static void (*proc)(...);
// static void *argl[];
// static void *unev;

typedef union atom Atom;
typedef struct pair Pair;

union atom {
  Pair *p;
  int n;
};

struct pair {
  Atom car;
  Atom cdr;
};

static Pair memory[MAXMEMORY];

/*
I think we either implement a fixed-memory system with manual garbage
collection (as directed in SICP) or a dynamically-growing-memory system using
malloc/free.
*/

/*
Questions:
- How do you differentiate between newlines (ignored) and the enter key?
- Should we call malloc to store read input, or use a statically-sized array?
- Since gets returns a char pointer, I'm guessing it has a static string
  within? Meaning we don't have to handle memory management of it ourselves.
*/

void read_eval_print_loop(void);

int main(int argc, char *argv[])
{
  read_eval_print_loop();
}

void read_eval_print_loop(void)
{
  char *read_input(void);

  // Read
  exp = read_input();
  printf("%s\n", exp);
  // Eval
  // env = get_global_environment();
  // val = eval_dispatch(exp, env);
  
  // // Print
  // print_result(val);
  
  // // Loop
  // read_eval_print_loop();
}

// Tests:
// hello-world this-is-second-expr
// (1 2 3 4 (5 6)) another-expr

// Idea: stop when parens balanced. Ignore strings for now.
char *read_input(void)
{
  static char buffer[MAXINPUT];

  char *bp = buffer;
  int c;
  int parens = 0;


  // Skip over leading whitespace.
  while ((c = getchar()) != EOF && isspace(c))
    ;

  // At start here, c is non-EOF, non-space char.
  do {
    if (bp >= buffer + MAXINPUT) {
      fprintf(stderr, "Input too long for buffer.\n");
      exit(1);
    }

    *bp++ = c; // Set current value at pointer to c.

    switch (c) {
      case '(':
        ++parens;
        break;
      case ')':
        --parens;
        break;        
    }

    if (parens == 0 && (isspace(c) || c == ')'))
      break;
    
  } while ((c = getchar()) != EOF);

  return buffer;
}