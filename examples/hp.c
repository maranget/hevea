#include <stdio.h>
#include <stdlib.h>

typedef struct cell {
  int val;
  struct cell *next;
} Cell, *Stack;

Stack stack = NULL;

Cell *cons(int val,Cell *next)
{
  Cell *r;

  r = (Cell *)malloc(sizeof(Cell));
  if ( r == NULL) {
    fprintf(stderr,"Plus de memoire\n");
    exit(-1);
  }
  r->val = val;
  r->next = next;
  return r;
}

void afficher(void)
{
  Stack p;

  printf("[");
  for (p=stack ; p != NULL ; p = p->next) {
    printf("%d",p->val);
    if (p->next != NULL)
      printf(", ");
  }
  printf("]");
}

Tree pop(void)
{
  Tree r;
  Stack tmp;

  if (stack == NULL) {
    fprintf(stderr,"Tentative de depiler une pile vide, adieu\n");
    exit(-1);
  }
  r = stack->val;
  tmp = stack;
  stack = stack->next;
  free(tmp);
  return r;
}


void push(i)
{
  stack = cons(i,stack);
}

void swap(void)
{
  int t;

  if (stack == NULL || stack->next == NULL)
    return;
  t = stack->val;
  stack->val = stack->next->val;
  stack->next->val = t;
  return;
}

void down(void)
{
  Stack p,q;

  if (stack == NULL)
    return;
  for (p = stack ; p->next->next != NULL ; p = p->next)
    ;
  q = p->next;
  p->next = NULL;
  q->next = stack;
  stack = q;
}

void up(void)
{
  Stack p,q;

  if (stack == NULL || stack->next == NULL)
    return;
  for (p = stack ; p->next != NULL ; p = p->next)
    ;
  q = stack;
  stack = stack->next;
  q->next = NULL;
  p->next = q;
}

void main(void)
{
  char pgm[] = "4321ud";
  char c;
  int i;
  int r1,r2;

  printf("    ");
  afficher();
  printf("\n");
  for(i=0; pgm[i] != '\0'; i++) {
    c = pgm[i];
    switch (c) {
    case '+':
      r1 = pop();
      r2 = pop();
      push(r2+r1);
      break;
    case '-':
      r1 = pop();
      r2 = pop();
      push(r2-r1);
      break;
    case '*':
      r1 = pop();
      r2 = pop();
      push(r2*r1);
      break;
    case '/':
      r1 = pop();
      r2 = pop();
      push(r2/r1);
      break;
    case 's':
      swap();
      break;
    case 'd':
      down();
      break;
    case 'u':
      up();
      break;
    default:
      if ('0' <= c && c <= '9')
        push(c - '0');
    }
    printf("%c : ",c);
    afficher();
    printf("\n");
  }
}
