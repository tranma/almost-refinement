#ifndef TWO_H
#define TWO_H
typedef enum colour colour_t;
typedef enum tag tag_t;

typedef struct node node_t;
typedef struct tree tree_t;

tree_t *tree_empty ();
tree_t *tree_insert (int key, tree_t *tree);

tree_t *tree_rotate_left (tree_t *t);

#endif
