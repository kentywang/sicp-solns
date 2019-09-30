#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#define MEMORYLIMIT 1000

typedef enum boolean Boolean;

typedef enum type Type;
typedef union value Value;
typedef struct element Element;
typedef struct pair Pair;

enum error_codes {
  BAD_IDENTIFIER
};

// Be explicit that false should be 0.
enum boolean {
  FALSE = 0,
  TRUE = 1
};

// Members of enum and union must be in same corresponding order for initial
// memory values to match their types.
// Be careful, though, since "initial values" means nothing after garbage
// collection.
struct element {
  enum type {
    PAIR,
    NUMBER,
    SYMBOL
  } type_tag;
  union value {
    Pair *pair_ptr;
    int number;
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

static Element exp;
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
- I moved cdr assignment out from create_symbol (and create_pair), does it
  still work? I'd think so, because it probably chained numbers outside of
  parens, even when we only wanted that to happens within a set of
  parenthesis.
- Do we need to manually set CDR to null because of GC?
- We don't store symbols/numbers not in a list in the list struct, right?

Tests:
  (1(  2  3  )4)
   (1 2 3)
    (1 2 (3 4 (5)) 6 7)
    (1(  -2  3a  )4)

Lessons learned:
- For mutating an object's pointer member, I can't pass the pointer into
  a function; I must pass the object.

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
  void read_input(Element *);
  void print_element(Element *);

  // Read
  read_input(&exp);

  // Eval
  // env = get_global_environment();
  // val = eval_dispatch(exp, env);
  
  // Print
  print_element(&exp/*val*/);
  printf("\n");

  // Free memory step?

  // // Loop
  // read_eval_print_loop();
}

void read_input(Element *e)
{
  Boolean is_integer(char *s);
  void print_element(Element *);
  void read_parens(Element *);
  char *read_word(int);

  int c;

  // Skip over leading whitespace.
  while ((c = getch()) != EOF && isspace(c))
    printf("read_input\n  space\n");

  // Do something with EOF here.

  // At start here, c is non-EOF, non-space char.
  if (c == '(') {
    printf("read_input\n  (\n");
    e->type_tag = PAIR;
    read_parens(e);

    // printf("Finished reading input,\n");
    // print_element(e);
    // printf("\n");
    return;
  }
  else if (isalnum(c) || c == '-') {
    printf("read_input\n  %c\n", c);
    char *s = read_word(1);

    if (is_integer(s)) {
      e->type_tag = NUMBER;
      e->contents.number = atoi(s);
      return;
    }

    e->type_tag = SYMBOL;
    e->contents.symbol = s;
    return;
  }

  // Everything else.
  fprintf(stderr, "Bad identifier: %c\n", c);
  exit(BAD_IDENTIFIER);
}

char *read_word(int count)
{
  char *create_symbol(int size);

  int c = getch();

  if (
    c == EOF ||
    isspace(c) ||
    c == '(' ||
    c == ')'
  ) {
    printf("read_word\n found ending condition\n");

    // Do something with EOF here.

    // Our job here is done. Return paren for another function to process.
    // We need to return character to buffer because the count variable must
    // be in sync.
    ungetch(c);
    return create_symbol(count);
  }
  printf("read_word\n  character is: %c\n", c);

  return read_word(count + 1);
}

void read_parens(Element *e)
{
  Boolean is_integer(char *s);
  void print_pair(Pair *p);

  Pair *read_cdr(Pair *);
  Pair *get_next_free_ptr(void);

  int c;

  // Skip over leading whitespace.
  while ((c = getch()) != EOF && isspace(c))
    ;

  // Do something with EOF here.

  // At start here, c is non-EOF, non-space char.
  // This should handle empty lists, since p is initialized to NULL?
  if (c == ')') {
    printf("read_parens\n  )\n");
    e->contents.pair_ptr = NULL;
    return;
  }

  Pair *p = e->contents.pair_ptr = get_next_free_ptr();
  
  if (c == '(') {
    printf("read_parens\n  (\n");
    p->car.type_tag = PAIR;
    read_parens(&p->car);
  } else {
    printf("read_parens\n  %c\n", c);

    // Almost the same as in read_input. Is there an abstraction here?
    char *s = read_word(1);

    if (is_integer(s)) {
      p->car.type_tag = NUMBER;
      p->car.contents.number = atoi(s);
    } else {
      p->car.type_tag = SYMBOL;
      p->car.contents.symbol = s;
    }

    // If this was the last word in the list, we know read_word put ) in the
    // buffer, so we skip over it.
    getch();
  }

  // Continue with the cdr, but no need to assign it to anything since it's
  // already been done by set_next_free_ptr.
  p->cdr.type_tag = PAIR;
  read_parens(&p->cdr);

  // printf("Finished reading between parens,\n");
  // print_pair(p);
  // printf("\n");

  return;
}

/////////////////////////////////////////////////////////////////////////////

Pair *get_next_free_ptr(void);
Pair *set_next_free_ptr(void);
void buffered_seek(int n);

char *create_symbol(int size)
{
  // Reserve memory size for word.
  char *s = (char *) malloc(size + 1);

  // Rewind stdin pointer to start of word.
  buffered_seek(size);

  // Copy over word from stdin to free pair's car.
  fgets(s, size + 1, stdin);
  // Will this handle empty strings?
  *(s + size) = '\0';
  printf("create_symbol\n  \"%s\", size: %d\n", s, size);

  return s;
}

int get_number(int size)
{
  char s[size];

  buffered_seek(size);
  fgets(s, size + 1, stdin);

  return atoi(s);
}

// When not full, just the next element in array. Otherwise need to garbage
// collect.
Pair *get_next_free_ptr(void)
{
  Pair *p = free_ptr;

  if (free_ptr + 1 < memory + MEMORYLIMIT) {
    free_ptr += 1;
    return p;
  }

  // WIP, need to implement garbage collection.
  printf("Uh-oh.\n");
  return p;
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

void buffered_seek(int n)
{
  fseek(stdin, -n - (char_buffer ? 1 : 0), SEEK_CUR);
}

/////////////////////////////////////////////////////////////////////////////

void print_pair(Pair *p)
{
  void print_element(Element *);

  printf("(");
  print_element(&p->car);
  printf(" . ");
  print_element(&p->cdr);
  printf(")");
}

void print_element(Element *e)
{
  if (e) {
    switch (e->type_tag) {
    case PAIR:
      if (e->contents.pair_ptr)
        print_pair(e->contents.pair_ptr);
      else
        printf("nil");
      break;
    case NUMBER:
      printf("%d", e->contents.number);
      break;
    case SYMBOL:
      printf("%s", e->contents.symbol);
      break;
    }
    return;
  } else
    // We shouldn't get here unless GC.
    printf("Uh-oh.\n");
}

/////////////////////////////////////////////////////////////////////////////

Boolean is_integer(char *s)
{
  if (*s == '-')
    s++;

  while (*s)
    if (!isdigit(*s++))
      return FALSE;
  return TRUE;
}