#include "hw7.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

int is_alpha(char c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {

    if (mat == NULL) {
        return root;
    }
    
    if (root == NULL) {
        bst_sf *node = malloc(sizeof(bst_sf));

        if (node == NULL) {
            return NULL;
        }

        node->mat = mat;
        node->left_child = NULL;
        node->right_child = NULL;
        
        return node;
    }
    
    if (mat->name > root->mat->name) {
        root->right_child = insert_bst_sf(mat, root->right_child);
    } else {
        root->left_child = insert_bst_sf(mat, root->left_child);
    }
    
    return root;
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    if (root == NULL) {
        return NULL;
    }

    if (name < root->mat->name) {
        return find_bst_sf(name, root->left_child);
    } else if (name > root->mat->name) {
        return find_bst_sf(name, root->right_child);
    } else {
        return root->mat;
    }
}

void free_bst_sf(bst_sf *root) {
    if (root == NULL) {
        return;
    }
    
    free_bst_sf(root->right_child);
    free_bst_sf(root->left_child);
    free(root->mat);
    free(root);
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {

    if (mat1 == NULL || mat2 == NULL) {
        return NULL;
    }

    if (mat1->num_rows != mat2->num_rows || mat1->num_cols != mat2->num_cols) {
        return NULL;
    }

    unsigned int matrix_rows = mat2->num_rows;
    unsigned int matrix_cols = mat2->num_cols;

    matrix_sf *result = malloc(sizeof(matrix_sf) + matrix_rows * matrix_cols * sizeof(int));

    if (result == NULL) {
        return NULL;
    }
    
    result->name = '@';
    result->num_rows = matrix_rows;
    result->num_cols = matrix_cols;

    unsigned int index = 0;

    while (index < matrix_rows * matrix_cols) {
        result->values[index] = mat1->values[index] + mat2->values[index];
        index++;
    }
    
    return result;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    
    if (mat1 == NULL || mat2 == NULL) {
        return NULL;
    }

    if (mat1->num_cols != mat2->num_rows) {
        return NULL;
    }
    
    unsigned int m = mat1->num_rows;
    unsigned int n = mat1->num_cols;
    unsigned int p = mat2->num_cols;
    
    matrix_sf *result = malloc(sizeof(matrix_sf) + m * p * sizeof(int));

    if (result == NULL) {
        return NULL;
    }
    
    result->name = '@';
    result->num_rows = mat1->num_rows;
    result->num_cols = mat2->num_cols;
    
    for (unsigned int i = 0; i < m; i++) {
        for (unsigned int j = 0; j < p; j++) {
            int sum = 0;
            for (unsigned int k = 0; k < n; k++) {
                sum += mat1->values[i * mat1->num_cols + k] * 
                       mat2->values[k * mat2->num_cols + j];
            }
            result->values[i * p + j] = sum;
        }
    }

    // unsigned int i = 0;
    // unsigned int j = 0;
    // unsigned int k = 0;
    
    return result;
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {

    if (mat == NULL) {
        return NULL;
    }
    
    unsigned int matrix_rows = mat->num_rows;
    unsigned int matrix_cols = mat->num_cols;
    
    matrix_sf *result = malloc(sizeof(matrix_sf) + matrix_rows * matrix_cols * sizeof(int));

    if (result == NULL) {
        return NULL;
    }
    
    result->name = '@';
    result->num_rows = matrix_cols;
    result->num_cols = matrix_rows;
    
    for (unsigned int i = 0; i < matrix_rows; i++) {
        for (unsigned int j = 0; j < matrix_cols; j++) {
            result->values[j * matrix_rows + i] = mat->values[i * matrix_cols + j];
        }
    }
    
    return result;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {

    if (expr == NULL) {
        return NULL;
    }
    
    const char *p = expr;

    while (*p && isspace(*p)) {
        p++;
    }

    unsigned int num_rows = 0;

    while (*p && isdigit(*p)) {
        num_rows = num_rows * 10 + (*p - '0');
        p++;
    }

    while (*p && isspace(*p)) {
        p++;
    }

    unsigned int num_cols = 0;

    while (*p && isdigit(*p)) {
        num_cols = num_cols * 10 + (*p - '0');
        p++;
    }

    while (*p && (*p != '[' || isspace(*p))) {
        p++;
    }

    if (*p == '[') {
        p++;
    }

    if (!num_rows || !num_cols) {
        return NULL;
    }

    matrix_sf *mat = malloc(sizeof(matrix_sf) + num_rows * num_cols * sizeof(int));

    if (mat == NULL) {
        return NULL;
    }
    
    mat->name = name;
    mat->num_rows = num_rows;
    mat->num_cols = num_cols;

    unsigned int index = 0;

    while (*p && index < num_rows * num_cols) {
        
        while (*p && (isspace(*p) || *p == ';')) {
            p++;
        }

        if (*p == ']') break;
        
        int sign = 1;

        if (*p == '-') {
            sign = -1;
            p++;
        }
        
        int value = 0;

        while (*p && isdigit(*p)) {
            value = value * 10 + (*p - '0');
            p++;
        }
        
        mat->values[index++] = sign * value;
    }
    
    return mat;
}

typedef struct {
    char *data;
    int top;
    int capacity;
} CharStack;

CharStack* create_char_stack(int capacity) {
    CharStack *stack = malloc(sizeof(CharStack));
    stack->data = malloc(capacity);
    stack->top = -1;
    stack->capacity = capacity;
    return stack;
}

void push_char(CharStack *stack, char c) {
    if (stack->top < stack->capacity - 1) {
        stack->data[++stack->top] = c;
    }
}

char pop_char(CharStack *stack) {
    if (stack->top >= 0) {
        return stack->data[stack->top--];
    }
    return '\0';
}

char peek_char(CharStack *stack) {
    if (stack->top >= 0) {
        return stack->data[stack->top];
    }
    return '\0';
}

int is_empty_char(CharStack *stack) {
    return stack->top == -1;
}

void free_char_stack(CharStack *stack) {
    free(stack->data);
    free(stack);
}

int precedence(char op) {
    if (op == '\'') return 3;
    if (op == '*') return 2;
    if (op == '+') return 1;
    return 0;
}

char* infix2postfix_sf(char *infix) {

    if (infix == NULL) {
        return NULL;
    }
    
    int length = strlen(infix);

    char *postfix = malloc(length * 2 + 1);

    if (postfix == NULL) {
        return NULL;
    }
    
    CharStack *stack = create_char_stack(length);

    int out_idx = 0;
    
    for (int i = 0; i < length; i++) {
        char c = infix[i];
        
        if (isspace(c)) continue;
        
        if (isalpha(c)) {
            postfix[out_idx++] = c;
        } else if (c == '(') {
            push_char(stack, c);
        } else if (c == ')') {
            while (!is_empty_char(stack) && peek_char(stack) != '(') {
                postfix[out_idx++] = pop_char(stack);
            }
            if (!is_empty_char(stack)) {
                pop_char(stack);
            }
        } else if (c == '+' || c == '*' || c == '\'') {
            while (!is_empty_char(stack) && peek_char(stack) != '(' && precedence(peek_char(stack)) >= precedence(c)) {
                postfix[out_idx++] = pop_char(stack);
            }
            push_char(stack, c);
        }
    }
    
    while (!is_empty_char(stack)) {
        postfix[out_idx++] = pop_char(stack);
    }
    
    postfix[out_idx] = '\0';
    free_char_stack(stack);
    
    return postfix;
}

typedef struct {
    matrix_sf **data;
    int top;
    int capacity;
} MatrixStack;

MatrixStack* create_matrix_stack(int capacity) {
    MatrixStack *stack = malloc(sizeof(MatrixStack));
    stack->data = malloc(capacity * sizeof(matrix_sf*));
    stack->top = -1;
    stack->capacity = capacity;
    return stack;
}

void push_matrix(MatrixStack *stack, matrix_sf *mat) {
    if (stack->top < stack->capacity - 1) {
        stack->data[++stack->top] = mat;
    }
}

matrix_sf* pop_matrix(MatrixStack *stack) {
    if (stack->top >= 0) {
        return stack->data[stack->top--];
    }
    return NULL;
}

void free_matrix_stack(MatrixStack *stack) {
    free(stack->data);
    free(stack);
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    if (!expr || !root) return NULL;
    
    char *postfix = infix2postfix_sf(expr);
    if (!postfix) return NULL;
    
    MatrixStack *stack = create_matrix_stack(strlen(postfix) + 1);

    for (int i = 0; postfix[i] != '\0'; i++) {
        char c = postfix[i];
        
        if (isalpha(c)) {
            matrix_sf *mat = find_bst_sf(c, root);
            if (mat) push_matrix(stack, mat);
        } else if (c == '\'') {
            matrix_sf *mat = pop_matrix(stack);
            if (mat) {
                matrix_sf *result = transpose_mat_sf(mat);
                if (!is_alpha(mat->name)) free(mat);
                push_matrix(stack, result);
            }
        } else if (c == '*') {
            matrix_sf *mat2 = pop_matrix(stack);
            matrix_sf *mat1 = pop_matrix(stack);
            if (mat1 && mat2) {
                matrix_sf *result = mult_mats_sf(mat1, mat2);
                if (!is_alpha(mat1->name)) free(mat1);
                if (!is_alpha(mat2->name)) free(mat2);
                push_matrix(stack, result);
            }
        } else if (c == '+') {
            matrix_sf *mat2 = pop_matrix(stack);
            matrix_sf *mat1 = pop_matrix(stack);
            if (mat1 && mat2) {
                matrix_sf *result = add_mats_sf(mat1, mat2);
                if (!is_alpha(mat1->name)) free(mat1);
                if (!is_alpha(mat2->name)) free(mat2);
                push_matrix(stack, result);
            }
        }
    }
    
    matrix_sf *result = pop_matrix(stack);
    if (result) result->name = name;
    
    free_matrix_stack(stack);
    free(postfix);
    
    return result;
}

matrix_sf* execute_script_sf(char *filename) {
    if (!filename) return NULL;
    
    FILE *file = fopen(filename, "r");
    if (!file) return NULL;
    
    bst_sf *root = NULL;
    matrix_sf *last_matrix = NULL;
    char *line = NULL;
    size_t max_line_size = MAX_LINE_LEN;
    
    while (getline(&line, &max_line_size, file) != -1) {
        char *p = line;

        while (*p && isspace(*p)) p++;
        if (!*p) continue;

        char mat_name = *p++;

        while (*p && *p != '=') p++;
        if (*p == '=') p++;

        while (*p && isspace(*p)) p++;

        if (isdigit(*p)) {
            matrix_sf *mat = create_matrix_sf(mat_name, p);
            if (mat) {
                root = insert_bst_sf(mat, root);
                last_matrix = mat;
            }
        } else {
            // Formula
            char *expr_start = p;
            char *expr_end = expr_start;
            while (*expr_end && *expr_end != '\n') expr_end++;
            *expr_end = '\0';
            
            matrix_sf *mat = evaluate_expr_sf(mat_name, expr_start, root);
            if (mat) {
                root = insert_bst_sf(mat, root);
                last_matrix = mat;
            }
        }
    }
    
    free(line);
    fclose(file);

    if (last_matrix) {
        matrix_sf *result = malloc(sizeof(matrix_sf) + 
                                   last_matrix->num_rows * last_matrix->num_cols * sizeof(int));
        if (result) {
            result->name = last_matrix->name;
            result->num_rows = last_matrix->num_rows;
            result->num_cols = last_matrix->num_cols;
            memcpy(result->values, last_matrix->values, 
                   last_matrix->num_rows * last_matrix->num_cols * sizeof(int));
        }
        free_bst_sf(root);
        return result;
    }
    
    free_bst_sf(root);
    return NULL;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}
