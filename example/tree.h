#ifndef TWO_H
#define TWO_H
#include <stdlib.h>

typedef enum tag tag_t;
typedef struct node node_t;
typedef struct tree tree_t;

tree_t *tree_empty ();
int tree_elem (int64_t key, tree_t *t);
void tree_insert (int64_t key, tree_t *tree);
void tree_delete (int64_t key, tree_t *tree);

#endif
