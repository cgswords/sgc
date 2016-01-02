#include <stdlib.h>
#include "stack.h"
#include "tags.h"

node* newNode(ptr *v) {
  node *ret = malloc (sizeof (node));
  ret->value = v;
  ret->next  = NULL;
  return ret;
}

node *pop(node* head) {
  node *ret = head->next;
  free(head);
  return ret;
}

node *push(node* head, ptr *value) {
  node *newHead = newNode(value);
  newHead->next = head;
  return newHead;
}

ptr *peek(node* head) {
  return head->value;
}

node *enqueue(node *head, ptr *value) {
  node *rover = head;

  if (rover == NULL) return push (rover, value);

  while (rover->next != NULL) rover = rover->next;
  
  rover->next = push(rover->next, value);

  return head;
}
