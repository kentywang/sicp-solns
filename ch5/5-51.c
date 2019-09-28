#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#define MEMORYLIMIT 1000

typedef struct element Element;
typedef struct pair Pair;

enum error_codes {
  BAD_IDENTIFIER
};

// Members of enum and union must be in same corresponding order for initial
// memory values to match their types.
// Be careful, though, since "initial values" means nothing after garbage
// collection.
struct element {
  enum {
    PAIR,
    NUMBER,
    SYMBOL
  } type_tag;

  union {
    Pair *pair_ptr;
    double number;
    char *symbol;
    // Need to store string too.
  } contents;
};

struct pair {
  Element car;
  Element cdr;
};

static Pair memory[MEMORYLIMIT];
static Pair *free_ptr = memory;

static Pair *exp;
// static Environment *env;
// static void *val;
// static char continue;
// static void (*proc)(...);
// static void *argl[];
// static void *unev;

/*

I think we either implement a fixed-memory system with manual garbage
collection (as directed in SICP) or a dynamically-growing-memory system using
malloc/free.

EDIT: Actually, my approach will be to garbage collect on a fixed region of
memory for list-structured data, but strings/symbols will actually be stored
in dynamically-allocated memory via malloc/free.

Questions:
- How do you differentiate between newlines (ignored) and the enter key?

*/

void read_eval_print_loop(void);
int getch(void);
void ungetch(int c);

int main(int argc, char *argv[])
{
  read_eval_print_loop();
}

void read_eval_print_loop(void)
{
  Pair *read_input(void);
  void print_pair(Pair *);

  // Read
  exp = read_input();

  // Eval
  // env = get_global_environment();
  // val = eval_dispatch(exp, env);
  
  // Print
  print_pair(exp/*val*/);
  
  // // Loop
  // read_eval_print_loop();
}

Pair *read_input(void)
{
  Pair *read_parens(void);
  Pair *read_word(int);

  int c;

  // Skip over leading whitespace.
  while ((c = getch()) != EOF && isspace(c))
    ;

  // At start here, c is non-EOF, non-space char.
  if (c == '(')
    return read_parens();
  else if (c != ')')
    return read_word(1);

  fprintf(stderr, "Bad identifier.\n");
  exit(BAD_IDENTIFIER);
}

Pair *read_word(int count)
{
  Pair *create_symbol(int size);

  int c;

  if ((c = getch()) == EOF || isspace(c))
    return create_symbol(count);

  if (c == '(' || c == ')') {
    // Our job here is done. Return paren for another function to process.
    ungetch(c);
    return create_symbol(count);
  }

  return read_word(count + 1);
}

Pair *read_parens()
{
  Pair *read_cdr(Pair *);
  Pair *create_pair(Pair *);
  Pair *p = NULL;

  int c;

  // Skip over leading whitespace.
  while ((c = getch()) != EOF && isspace(c))
    ;

  // At start here, c is non-EOF, non-space char.
  // This should handle empty lists, since p is initialized to NULL?
  if (c == ')')
    return p;

  p = c == '(' ? create_pair(read_parens()) : read_word(1);

  // Continue with the cdr, but no need to assign it to anything since its
  // already been done by set_next_free_ptr.
  read_parens();

  return p;
}

/////////////////////////////////////////////////////////////////////////////

Pair *get_next_free_ptr(void);
Pair *set_next_free_ptr(void);

Pair *create_symbol(int size)
{
  // Rewind stdin pointer to start of word.
  fseek(stdin, -size, SEEK_CUR);

  // Reserve memory size for word, saving ref to this location in memory
  // in car of next free pair.
  free_ptr->car.type_tag = SYMBOL;
  free_ptr->car.contents.symbol = (char *) malloc(size + 1);

  // Copy over word from stdin to free pair's car.
  fgets(free_ptr->car.contents.symbol, size, stdin);
  // Will this handle empty strings?
  *(free_ptr->car.contents.symbol + size) = '\0';

  free_ptr->cdr.type_tag = PAIR;
  free_ptr->cdr.contents.pair_ptr = get_next_free_ptr();

  return set_next_free_ptr();
}

Pair *create_pair(Pair *p)
{
  free_ptr->car.type_tag = PAIR;
  free_ptr->cdr.contents.pair_ptr = get_next_free_ptr();

  return set_next_free_ptr();
}

// When not full, just the next element in array. Otherwise need to garbage
// collect.
Pair *get_next_free_ptr(void)
{
  if (free_ptr + 1 < memory + MEMORYLIMIT)
    return free_ptr + 1;

  // WIP, need to implement garbage collection.
  return free_ptr;
}

// Returns _old_ free pointer, while updating current free pointer. We can
// always get the current free pointer in the global scope.
Pair *set_next_free_ptr(void)
{
  Pair *old_free_ptr = free_ptr;

  free_ptr = free_ptr->cdr.contents.pair_ptr;

  return old_free_ptr;
}

/////////////////////////////////////////////////////////////////////////////

static int char_buffer = 0;

int getch(void)
{
  int d = char_buffer ? char_buffer : getchar();

  char_buffer = 0;

  return d;
}

void ungetch(int c)
{
  char_buffer = c;
}

/////////////////////////////////////////////////////////////////////////////

void print_pair(Pair *p)
{
  void print_element(Element *);

  print_element(&p->car);
  print_element(&p->cdr);
}

void print_element(Element *e)
{
  if (e) {
    switch (e->type_tag) {
    case PAIR:
      printf("(\n");
      print_pair(e->contents.pair_ptr);
      printf(")\n");
      break;
    case NUMBER:
      printf("%f\n", e->contents.number);
      break;
    case SYMBOL:
      printf("%s\n", e->contents.symbol);
      break;
    }
  }
  // We shouldn't get here unless GC.
  printf("Uh-oh.\n");
}