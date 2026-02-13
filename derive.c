#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define indent(depth) for (int i = 0; i < depth; i++) printf("  ");
#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

#define EXPR_BINARY_MASK 	(1 << 8)
#define EXPR_UNARY_MASK 	(1 << 9)
#define EXPR_LEAF_MASK		(1 << 10)

#define EXPR_SYM  (1 | EXPR_LEAF_MASK)
#define EXPR_NUM	(2 | EXPR_LEAF_MASK)
#define EXPR_FRAC	(3 | EXPR_BINARY_MASK)
#define EXPR_MUL	(4 | EXPR_BINARY_MASK)
#define EXPR_EXP	(5 | EXPR_BINARY_MASK)
#define EXPR_ADD	(6 | EXPR_BINARY_MASK)
#define EXPR_SUB	(7 | EXPR_BINARY_MASK)
#define EXPR_LOG	(8 | EXPR_UNARY_MASK)
#define EXPR_SIN	(9 | EXPR_UNARY_MASK)
#define EXPR_COS	(10 | EXPR_UNARY_MASK)
#define EXPR_NEG	(11 | EXPR_UNARY_MASK)

typedef struct expr {
	int type;
	int mindepth, maxdepth;
	int ref;
	union {
		char *symbol;
		int num;

		struct {
			struct expr *left;
			struct expr *right;
		};

		struct expr *unary;

	};
} expr;

static expr *one = NULL;
static expr *zero = NULL;

expr *derive(expr *, char *);
expr *release(expr *);


expr *makeexpr(int type, int mindepth, int maxdepth) {
	expr *self = malloc(sizeof(expr));
	self->type = type;
	self->mindepth = mindepth + 1;
	self->maxdepth = maxdepth + 1;
	self->ref = 1;
	return self;
}

void freeexpr(expr *f) {
	if (f->type == EXPR_SYM) {
		free(f->symbol);
	} else if (f->type & EXPR_BINARY_MASK) {
		release(f->left);
		release(f->right);
	} else if (f->type & EXPR_UNARY_MASK) {
		release(f->unary);
	}
	free(f);
}

expr *retain(expr *self) {
	if (self == zero || self == one) return self;
	self->ref++;
	return self;
}

expr *release(expr *self) {
	if (self == zero || self == one) return self;
	if (--self->ref == 0) {
		freeexpr(self);
		return NULL;
	}
	return self;
}

expr *sym(char *symbol) {
	expr *self = makeexpr(EXPR_SYM, 0, 0);
	self->symbol = strdup(symbol);
	return self;
}

expr *num(int n) {
	expr *self = makeexpr(EXPR_NUM, 0, 0);
	self->num = n;
	return self;
}

expr *makebinary(int type, expr *left, expr *right) {
	expr *self = makeexpr(type, 
								min(left->mindepth, right->mindepth),
								max(left->maxdepth, right->maxdepth));
	self->left = left;
	self->right = right;
	return self;
}

expr *makeunary(int type, expr *op) {
	expr *self = makeexpr(type, op->mindepth, op->maxdepth);
	self->unary = op;
	return self;
}

int eq(expr *e1, expr *e2) {
	if (!e1 || !e2) return 0;
	if (e1->type != e2->type) return 0;

	if (e1->type == EXPR_SYM) {
		return strcmp(e1->symbol, e2->symbol) == 0;
	} else if (e1->type == EXPR_NUM) {
		return e1->num == e2->num;
	} else if (e1->type & EXPR_BINARY_MASK) {
		return eq(e1->left, e2->left) && eq(e1->right, e2->right);
	} else if (e1->type & EXPR_UNARY_MASK) {
		return eq(e1->unary, e2->unary);
	} else {
		return 0;
	}
}

// expr *copy(expr *self) {
// 	if (!self) return self;
//
// 	expr *cloned = makeexpr(self->type, self->mindepth - 1, self->maxdepth - 1);
//
// 	if (self->type == EXPR_SYM) {
// 		cloned->symbol = strdup(self->symbol);
// 	} else if (self->type == EXPR_NUM) {
// 		cloned->num = self->num;
// 	} else if (self->type & EXPR_BINARY_MASK) {
// 		cloned->left = copy(self->left);
// 		cloned->right = copy(self->right);
// 	} else if (self->type & EXPR_UNARY_MASK) {
// 		cloned->unary = copy(self->unary);
// 	}
//
// 	return cloned;
// }

expr *frac(expr *numer, expr *denom) {
	if (eq(numer, zero)) {
		release(numer);
		release(denom);
		return zero;
	}
	return makebinary(EXPR_FRAC, numer, denom);
}

expr *mul(expr *left, expr *right) {
	if (eq(left, zero) || eq(right, zero)) {
		release(left);
		release(right);
		return zero;
	}
	if (eq(left, one)) { release(left); return right; }
	if (eq(right, one)) { release(right);return left; }
	return makebinary(EXPR_MUL, left, right);
}

expr *add(expr *left, expr *right) {
	if (eq(left, zero)) { release(left); return right; }
	if (eq(right, zero)) { release(right); return left; }
	return makebinary(EXPR_ADD, left, right);
}

expr *exponential(expr *base, expr *exponent) { return makebinary(EXPR_EXP, base, exponent); }
expr *sub(expr *left, expr *right) { return makebinary(EXPR_SUB, left, right); }
expr *neg(expr *arg) { return makeunary(EXPR_NEG, arg); }
expr *logarithm(expr *arg) { return makeunary(EXPR_LOG, arg); }
expr *sine(expr *arg) { return makeunary(EXPR_SIN, arg); }
expr *cosine(expr *arg) { return makeunary(EXPR_COS, arg); }

expr *deriveSin(expr *f, char *sym) {
	return mul(derive(f->unary, sym), cosine(retain(f->unary)));
}

expr *deriveCos(expr *f, char *sym) {
	return mul(derive(f->unary, sym), neg(sine(retain(f->unary))));
}

// d/dx(f^g) = f^g * (g'ln(f) + gf'/f)
expr *deriveExp(expr *func, char *sym) {
	expr *f = func->left;
	expr *g = func->right;
	
	return mul(
		retain(func),
		add(
			mul(
				derive(g, sym),
				logarithm(retain(f))
			),
			frac(
				mul(retain(g), derive(f, sym)),
				retain(f)
			)
		)
	);
}

expr *deriveMul(expr *f, char *sym) {
	expr *left = mul(derive(f->left, sym), retain(f->right));
	expr *right = mul(retain(f->left), derive(f->right, sym));
	return add(left, right);
}

expr *deriveFrac(expr *f, char *sym) {
	expr *denom = exponential(retain(f->right), num(2));

	expr *first = mul(derive(f->left, sym), retain(f->right));
	expr *second = mul(retain(f->left), derive(f->right, sym));

	return frac(sub(first, second), denom);
}

expr *deriveAdd(expr *f, char *sym) {
	expr *left = derive(f->left, sym);
	expr *right = derive(f->right, sym);

	return add(left, right);
}

expr *deriveSub(expr *f, char *sym) {
	expr *left = derive(f->left, sym);
	expr *right = derive(f->right, sym);

	return sub(left, right);
}

expr *deriveNeg(expr *f, char *sym) {
	return neg(derive(f->unary, sym));
}

expr *deriveLog(expr *f, char *sym) {
	return frac(derive(f->unary, sym), retain(f->unary));
}

expr *derive(expr *f, char *sym) {
	switch (f->type) {
		case EXPR_ADD: return deriveAdd(f, sym);
		case EXPR_SUB: return deriveSub(f, sym);
		case EXPR_MUL: return deriveMul(f, sym);
		case EXPR_EXP: return deriveExp(f, sym);
		case EXPR_SIN: return deriveSin(f, sym);
		case EXPR_COS: return deriveCos(f, sym);
		case EXPR_FRAC: return deriveFrac(f, sym);
		case EXPR_NEG: return deriveNeg(f, sym);
		case EXPR_LOG: return deriveLog(f, sym);
		case EXPR_SYM:
			if (strcmp(sym, f->symbol) == 0) {
				return one;
			}
			return zero;
		case EXPR_NUM: return zero;
		default:
			printf("Operation not handled yet\n");
			return NULL;
	}
}

int precedence(expr *f) {
	switch (f->type) {
		case EXPR_ADD: case EXPR_SUB: return 1;
		case EXPR_MUL: case EXPR_FRAC: return 2;
		case EXPR_EXP: return 3;
		default: return 10;
	}
}

void print(expr *f) {
	if (f->type == EXPR_SYM) { printf("%s", f->symbol); return; }
	if (f->type == EXPR_NUM) { printf("%d", f->num); return; }

	if (f->type == EXPR_NEG) {
		int parens = f->unary->type & EXPR_BINARY_MASK;
		printf("-");
		if (parens) printf("(");
		print(f->unary);
		if (parens) printf(")");
		return;
	}

	if (f->type == EXPR_SIN || f->type == EXPR_COS || f->type == EXPR_LOG) {
		printf("%s(", f->type == EXPR_SIN ? "sin" : f->type == EXPR_COS ? "cos" : "log");
		print(f->unary);
		printf(")");
		return;
	}

	char *op;
	switch (f->type) {
		case EXPR_ADD: op = " + "; break;
		case EXPR_SUB: op = " - "; break;
		case EXPR_MUL: op = " * "; break;
		case EXPR_FRAC: op = " / "; break;
		case EXPR_EXP: op = "^"; break;
		default: op = " ? "; break;
	}

	int prec = precedence(f);
	int lp = precedence(f->left) < prec;
	int rp = precedence(f->right) < prec;
	if (f->type == EXPR_EXP && precedence(f->left) == prec) lp = 1;
	if ((f->type == EXPR_SUB || f->type == EXPR_FRAC) && precedence(f->right) == prec) rp = 1;

	if (lp) printf("(");
	print(f->left);
	if (lp) printf(")");
	printf("%s", op);
	if (rp) printf("(");
	print(f->right);
	if (rp) printf(")");
}

int main(void) {
	one = num(1);
	zero = num(0);
	expr *composed = mul(
		exponential(sym("x"), add(sym("x"), num(3))),
		cosine(sym("x"))
	);
	print(composed);
	printf("\n");

	print(derive(composed, "x"));
	printf("\n");

	return 0;
}
