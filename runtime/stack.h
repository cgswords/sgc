#include "tags.h"

typedef struct node {
  struct node *next;
  ptr *value;
} node;

node* newNode(ptr *v);

node *pop(node* head); 

node *push(node* head, ptr *value); 

ptr *peek(node* head);

node *enqueue(node *head, ptr *value);

