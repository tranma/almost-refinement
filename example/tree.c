#include <stdlib.h>
#include <stdio.h>
#include "tree.h"

enum tag {
    NIL = 0,
    NODE = 1
};

struct node {
    tag_t tag;
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

void tree_insert (int64_t key, tree_t *t) {
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
}

node_t *find (int64_t key, node_t *t) {
    if (t->tag == NIL || t->key == key) {
        return t;
    } else if (key < t->key) {
        return find (key, t->left);
    } else {
        return find (key, t->right);
    }
}

int tree_elem (int64_t key, tree_t *t) {
    node_t *x = find (key, t->root);
    if (x->tag == NIL) {
        return 0;
    } else {
        return 1;
    }
}

node_t *minimum (node_t *n) {
    node_t *x = n;
    while (x->left->tag != NIL) {
        x = x->left;
    }
    return x;
}

void replace (tree_t *t, node_t *u, node_t *v) {
    if (u->parent->tag == NIL) {
        t->root = v;
    } else if (u->parent->left == u) {
        u->parent->left = v;
    } else {
        u->parent->right = v;
    }

    if (v->tag != NIL) {
        v->parent = u->parent;
    }
}

void delete (node_t *z, tree_t *t) {
    if (z->left->tag == NIL) {
        replace (t, z, z->right);
    } else if (z->right->tag == NIL) {
        replace (t, z, z->left);
    } else {
        node_t *y = minimum (z->right);

        if (y->parent != z) {
            replace (t, y, y->right);
            y->right = z->right;
            y->right->parent = y;
        }

        replace (t, z, y);
        y->left = z->left;
        y->left->parent = y;
    }
}

void tree_delete (int64_t key, tree_t *t) {
    node_t *x = find (key, t->root);
    if (x->tag != NIL) {
        delete (x, t);
    }
}
