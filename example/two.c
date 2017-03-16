#include <stdlib.h>
#include <stdio.h>
#include "two.h"

enum colour {
    RED,
    BLACK
};

enum tag {
    NIL = 0,
    NODE = 1
};

struct node {
    tag_t tag;
    colour_t colour;
    int64_t key;
    node_t *left;
    node_t *right;
    node_t *parent;
};

struct tree {
    node_t *root;
    node_t *nil;
};

node_t *node () {
    node_t *n = (node_t*) malloc (sizeof (struct node));
    n->tag = NIL;
    n->colour = BLACK;
    return n;
}

tree_t *tree_empty () {
    tree_t *t = (tree_t*) malloc (sizeof (struct tree));
    node_t *x = node ();
    t->root = x;
    t->nil = x;
    t->nil->parent = t->nil;
    t->nil->left = t->nil;
    t->nil->right = t->nil;
    return t;
}

int is_left_child (node_t *x) {
    return x->parent->left == x;
}

int is_right_child (node_t *x) {
    return x->parent->right == x;
}

// :(
int no_parent (node_t *x) {
    return x->parent->tag == NIL;
}

int has_parent (node_t *x) {
    return x->parent->tag != NIL;
}

int no_left_child (node_t *x) {
    return x->left->tag == NIL;
}

int has_left_child (node_t *x) {
    return x->left->tag != NIL;
}

int no_right_child (node_t *x) {
    return x->right->tag == NIL;
}

int has_right_child (node_t *x) {
    return x->right->tag != NIL;
}

tree_t *rotate_left (node_t *x, tree_t *t) {
    if (no_right_child (x)) {
        return t;
    }

    node_t *y = x->right;
    x->right = y->left;

    if (has_left_child (y)) {
        y->left->parent = x;
    }
    y->parent = x->parent;

    if (no_parent (x)) {
        t->root = y;
    } else if (is_left_child (x)) {
        x->parent->left = y;
    } else {
        x->parent->right = y;
    }

    y->left = x;
    x->parent = y;

    return t;
}

tree_t *tree_rotate_left (tree_t *t) {
    return rotate_left (t->root, t);
}

tree_t *rotate_right (node_t *y, tree_t *t) {
    node_t *x = y->left;
    y->left = x->right;

    if (has_right_child (x)) {
        x->right->parent = y;
    }
    x->parent = y->parent;

    if (no_parent (y)) {
        t->root = x;
    } else if (is_right_child (y)) {
        y->parent->right = x;
    } else {
        y->parent->left = x;
    }

    x->right = y;
    y->parent = x;

    return t;
}

tree_t *tree_rotate_right (tree_t *t) {
    return rotate_right (t->root, t);
}

tree_t *tree_insert_recolour (node_t *z, tree_t *t) {
    node_t *y;

    while (z->parent->colour == RED) {
        if (is_left_child (z->parent)) {
            y = z->parent->parent->right;
            if (y->colour == RED) {
                z->parent->colour = BLACK;
                y->colour = BLACK;
                z->parent->parent->colour = RED;
                z = z->parent->parent;
            } else {
                if (is_right_child (z)) {
                    z = z->parent;
                    rotate_left (z, t);
                }
                z->parent->colour = BLACK;
                z->parent->parent->colour = RED;
                rotate_right (z->parent->parent, t);
            }
        } else {
            y = z->parent->parent->left;
            if (y->colour == RED) {
                z->parent->colour = BLACK;
                y->colour = BLACK;
                z->parent->parent->colour = RED;
                z = z->parent->parent;
            } else {
                if (is_left_child (z)) {
                    z = z->parent;
                    rotate_right (z, t);
                }
                z->parent->colour = BLACK;
                z->parent->parent->colour = RED;
                rotate_left (z->parent->parent, t);
            }
        }
    }
    t->root->colour = BLACK;
    return t;
}

tree_t *tree_insert (int key, tree_t *t) {
    node_t *y = t->nil;
    node_t *x = t->root;
    node_t *z = node ();

    z->key = key;
    z->tag = NODE;

    while (x->tag != NIL) {
        y = x;

        if (key < x->key) {
            x = x->left;
        } else {
            x = x->right;
        }
    }

    if (y->tag == NIL) {
        t->root = z;
    } else if (key < y->key) {
        y->left = z;
    } else {
        y->right = z;
    }

    z->parent = y;
    z->left = t->nil;
    z->right = t->nil;
    z->colour = RED;

    return tree_insert_recolour (z, t);
}
