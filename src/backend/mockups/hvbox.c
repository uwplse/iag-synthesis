#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <pthread.h>

const int CUTOFF = 2;

const int TREE_SPREAD = 4;
const int TREE_HEIGHT = 14;

const int MAX_WIDTH = 100;
const int MAX_HEIGHT = 100;

struct Top {
    enum { ROOT } class;
    struct HVBox *root;
    int depth; // NOTE: Traversal metadata, not actual node data
};

struct HVBox {
    enum { HBOX, VBOX, LEAF } class;
    int width, width_out;
    int height, height_out;
    int right;
    int bottom;
    bool visible;

    // fields only for HBox and VBox nodes
    struct HVBox *children;
    int n_children;

    // fields only for Leaf nodes
    int width_in;
    int height_in;

    // traversal metadata
    int depth;
};

// Visitors for the first, post-order traversal pass

void Top_post(struct Top *self);
void Root_post(struct Top *self);
void HVBox_post(struct HVBox *self);
void HBox_post(struct HVBox *self);
void VBox_post(struct HVBox *self);
void Leaf_post(struct HVBox *self);

void Top_post(struct Top *self) {
    switch (self->class) {
    case ROOT:
        Root_post(self);
        break;
    }
}

void Root_post(struct Top *self) {
    self->root->depth = self->depth + 1;
    HVBox_post(self->root);
}

void HVBox_post(struct HVBox *self) {
    switch (self->class) {
    case HBOX:
        HBox_post(self);
        break;
    case VBOX:
        VBox_post(self);
        break;
    case LEAF:
        Leaf_post(self);
        break;
    }
}

void HBox_post(struct HVBox *self) {
    struct HVBox *child;
    int i;
    pthread_t workers[self->n_children];
    int width_out, height_out;

    if (self->depth < CUTOFF) {
        for (i = self->n_children; --i >= 0;) {
            child = &self->children[i];
            child->depth = self->depth + 1;
            pthread_create(&workers[i], NULL, HVBox_post, child);
        }
        for (i = self->n_children; --i >= 0;) {
            pthread_join(workers[i], NULL);
        }
    } else {
        for (i = self->n_children; --i >= 0;) {
            child = &self->children[i];
            child->depth = self->depth + 1;
            HVBox_post(child);
        }
    }
    width_out = 0;
    height_out = 0;
    for (i = self->n_children; --i >= 0;) {
        child = &self->children[i];
        child->width_out = width_out + child->width_out;
        child->height_out = (height_out > child->height) ? height_out : child->height;
        width_out = child->width_out;
        height_out = child->height_out;
    }
    if (self->visible) {
        self->width = width_out;
        self->height = height_out;
    } else {
        self->width = 0;
        self->height = 0;
    }
}

void VBox_post(struct HVBox *self) {
    struct HVBox *child;
    int i;
    pthread_t workers[self->n_children];
    int width_out, height_out;

    if (self->depth < CUTOFF) {
        for (i = self->n_children; --i >= 0;) {
            child = &self->children[i];
            child->depth = self->depth + 1;
            pthread_create(&workers[i], NULL, HVBox_post, child);
        }
        for (i = self->n_children; --i >= 0;) {
            pthread_join(workers[i], NULL);
        }
    } else {
        for (i = self->n_children; --i >= 0;) {
            child = &self->children[i];
            child->depth = self->depth + 1;
            HVBox_post(child);
        }
    }
    width_out = 0;
    height_out = 0;
    for (i = self->n_children; --i >= 0;) {
        child = &self->children[i];
        child->width_out = (width_out > child->width) ? width_out : child->width;
        child->height_out = height_out + child->height_out;
        width_out = child->width_out;
        height_out = child->height_out;
    }
    if (self->visible) {
        self->width = width_out;
        self->height = height_out;
    } else {
        self->width = 0;
        self->height = 0;
    }
}

void Leaf_post(struct HVBox *self) {
    self->width = self->width_in;
    self->height = self->height_in;
}

// Visitors for the second, pre-order traversal pass

void Top_pre(struct Top *self);
void Root_pre(struct Top *self);
void HVBox_pre(struct HVBox *self);
void HBox_pre(struct HVBox *self);
void VBox_pre(struct HVBox *self);
void Leaf_pre(struct HVBox *self);

void Top_pre(struct Top *self) {
    switch (self->class) {
    case ROOT:
        Root_pre(self);
        break;
    }
}

void Root_pre(struct Top *self) {
    self->root->right = self->root->width;
    self->root->bottom = self->root->height;
    self->root->width_out = self->root->width;
    self->root->height_out = self->root->height;
    HVBox_post(self->root);
}

void HVBox_pre(struct HVBox *self) {
    switch (self->class) {
    case HBOX:
        HBox_pre(self);
        break;
    case VBOX:
        VBox_pre(self);
        break;
    case LEAF:
        Leaf_pre(self);
        break;
    }
}

void HBox_pre(struct HVBox *self) {
    struct HVBox *child;
    int i;
    pthread_t workers[self->n_children];
    int right;

    right = self->right - self->width;
    for (i = self->n_children; --i >= 0;) {
        child = &self->children[i];
        child->right = right + child->width;
        child->bottom = self->bottom;
        right = child->right;
    }

    if (self->depth < CUTOFF) {
        for (i = self->n_children; --i >= 0;) {
            child = &self->children[i];
            child->depth = self->depth + 1;
            pthread_create(&workers[i], NULL, HVBox_pre, child);
        }
        for (i = self->n_children; --i >= 0;) {
            pthread_join(workers[i], NULL);
        }
    } else {
        for (i = self->n_children; --i >= 0;) {
            child = &self->children[i];
            child->depth = self->depth + 1;
            HVBox_pre(child);
        }
    }
}

void VBox_pre(struct HVBox *self) {
    struct HVBox *child;
    int i;
    pthread_t workers[self->n_children];
    int bottom;

    bottom = self->bottom;
    for (i = self->n_children; --i >= 0;) {
        child = &self->children[i];
        child->right = self->right;
        child->bottom = bottom + child->height;
        bottom = child->bottom;
    }

    if (self->depth < CUTOFF) {
        for (i = self->n_children; --i >= 0;) {
            child = &self->children[i];
            child->depth = self->depth + 1;
            pthread_create(&workers[i], NULL, HVBox_pre, child);
        }
        for (i = self->n_children; --i >= 0;) {
            pthread_join(workers[i], NULL);
        }
    } else {
        for (i = self->n_children; --i >= 0;) {
            child = &self->children[i];
            child->depth = self->depth + 1;
            HVBox_pre(child);
        }
    }
}

void Leaf_pre(struct HVBox *self) { }

// Tree generation

void initialize_top(struct Top *self);
void initialize_root(struct Top *self, int w, int h);
void initialize_hvbox(struct HVBox *self);
void initialize_hbox(struct HVBox *self);
void initialize_vbox(struct HVBox *self);
void initialize_leaf(struct HVBox *self);

void initialize_top(struct Top *self) { }

void initialize_root(struct Top *self, int w, int h) {
    int i;

    initialize_top(self);
    self->class = ROOT;
    self->root = malloc(sizeof(struct HVBox));
    self->root->width = w;
    self->root->height = h - 1;
    initialize_hbox(self->root);
}

void initialize_hvbox(struct HVBox *self) {
    self->visible = true;
}

void initialize_hbox(struct HVBox *self) {
    struct HVBox *child;
    int i;
    int w = self->width;
    int h = self->height;
    pthread_t workers[w];

    initialize_hvbox(self);
    self->class = HBOX;
    self->children = calloc(sizeof(struct HVBox), w);
    self->n_children = w;
    // NOTE: We're sneakily reusing the uninitialized fields to
    // communicate with the worker threads.
    for (i = self->n_children; --i >= 0;) {
        child = &self->children[i];
        child->width = w;
        child->height = h - 1;
        if (h == 1) {
            initialize_leaf(child);
        } else if (TREE_HEIGHT - h < CUTOFF) {
            pthread_create(&workers[i], NULL, initialize_vbox, child);
        } else {
            initialize_vbox(child);
        }
    }
    if (TREE_HEIGHT - h < CUTOFF) {
        for (i = self->n_children; --i >= 0;) {
            pthread_join(workers[i], NULL);
        }
    }
    return;
}

void initialize_vbox(struct HVBox *self) {
    struct HVBox *child;
    int i;
    int w = self->width;
    int h = self->height;
    pthread_t workers[w];

    initialize_hvbox(self);
    self->class = VBOX;
    self->children = calloc(sizeof(struct HVBox), w);
    self->n_children = w;
    // NOTE: We're sneakily reusing the uninitialized fields to
    // communicate with the worker threads.
    for (i = self->n_children; --i >= 0;) {
        child = &self->children[i];
        child->width = w;
        child->height = h - 1;
        if (h == 1) {
            initialize_leaf(child);
        } else if (TREE_HEIGHT - h < CUTOFF) {
            pthread_create(&workers[i], NULL, initialize_vbox, child);
        } else {
            initialize_hbox(child);
        }
    }
    if (TREE_HEIGHT - h < CUTOFF) {
        for (i = self->n_children; --i >= 0;) {
            pthread_join(workers[i], NULL);
        }
    }
    return;
}

void initialize_leaf(struct HVBox *self) {
    initialize_hvbox(self);
    self->class = LEAF;
    self->width_in = 1 + rand() % MAX_WIDTH;
    self->height_in = 1 + rand() % MAX_HEIGHT;
    return;
}

struct Top *generate(int w, int h) {
    struct Top *tree = malloc(sizeof(struct Top));
    tree->depth = 0;
    initialize_root(tree, w, h);
    return tree;
}

void layout(struct Top *tree) {
    Top_post(tree);
    Top_pre(tree);
}

int main(void) {
    struct Top *tree;
    clock_t time;

    printf("Generating tree...\n");
    tree = generate(TREE_SPREAD, TREE_HEIGHT);
    printf("Beginning layout...\n");
    time = clock();
    layout(tree);
    time = clock() - time;

    printf("Time: %fms\n", (double)time / CLOCKS_PER_SEC * 1000);
    return 0;
}
