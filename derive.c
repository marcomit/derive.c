#include <_ctype.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

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
		float num;

		struct {
			struct expr *left;
			struct expr *right;
		};

		struct expr *unary;
	};
} expr;

typedef enum {
	TOK_LPAREN,
	TOK_RPAREN,
	TOK_POW,
	TOK_NUM,
	TOK_SYM,
	TOK_PLUS,
	TOK_MINUS,
	TOK_SLASH,
	TOK_STAR
} toktype;

typedef struct {
	toktype type;
	union {
		char str;
		int num;
	};
} token;

typedef struct {
	token **iter;
	size_t len;
	size_t cap;
	size_t curr;
	expr *sym;
} context;

static expr *zero = NULL;
static expr *one = NULL;
static expr *two = NULL;

expr *derive(expr *, expr *);
expr *simplify(expr *, expr *);
expr *neg(expr *);
expr *release(expr *);

context *makecontext() {
	context *self = malloc(sizeof(context));
	self->len = 0;
	self->cap = 8;
	self->curr = 0;
	self->iter = malloc(sizeof(token *) * self->cap);
	return self;
}

token *peek(context *ctx) { return ctx->iter[ctx->len - 1]; }
int hasNext(context *ctx) { return ctx->len < ctx->cap; }

token *next(context *ctx) {
	token *curr = peek(ctx);
	ctx->len++;
	return curr;
}

void freecontext(context *ctx) {
	for (size_t i = 0; i < ctx->len; i++) {
		free(ctx->iter[i]);
	}
	free(ctx->iter);
	free(ctx);
}

void addtok(context *ctx, token *tok) {
	if (ctx->len >= ctx->cap-1) {
		ctx->cap <<= 1;
		ctx->iter = realloc(ctx->iter, ctx->cap);
	}
	ctx->iter[ctx->len++] = tok;
}

token *maketok(toktype type, char tok) {
	token *self = malloc(sizeof(token));
	self->type = type;
	self->str = tok;
	return self;
}

token *makenum(int num) {
	token *self = maketok(TOK_NUM, 0);
	self->num = num;
	return self;
}

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
	if (self == zero || self == one || self == two) return self;
	self->ref++;
	return self;
}

expr *release(expr *self) {
	if (self == zero || self == one || self == two) return self;
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
	if (n == 0 && zero) return zero;
	if (n == 1 && one) return one;
	if (n == 2 && two) return two;
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

int is(expr *e, int type) { return e->type == type; }
int flag(expr *e, int mask) { return e->type & mask; }

int eq(expr *e1, expr *e2) {
	if (!e1 || !e2) return 0;
	if (e1->type != e2->type) return 0;

	if (is(e1, EXPR_SYM)) {
		return strcmp(e1->symbol, e2->symbol) == 0;
	} else if (is(e1, EXPR_NUM)) {
		return e1->num == e2->num;
	} else if (flag(e1, EXPR_BINARY_MASK)) {
		return eq(e1->left, e2->left) && eq(e1->right, e2->right);
	} else if (flag(e1, EXPR_UNARY_MASK)) {
		return eq(e1->unary, e2->unary);
	} else {
		return 0;
	}
}

int contains(expr *f, expr *sym) {
	if (eq(f, sym)) return 1;
	if (flag(f, EXPR_BINARY_MASK)) {
		return contains(f->left, sym) || contains(f->right, sym);
	} else if (flag(f, EXPR_UNARY_MASK)) {
		return contains(f->unary, sym);
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

expr *frac(expr *numer, expr *denom) { return makebinary(EXPR_FRAC, numer, denom); }
expr *mul(expr *left, expr *right) { return makebinary(EXPR_MUL, left, right); }
expr *add(expr *left, expr *right) { return makebinary(EXPR_ADD, left, right); }
expr *exponential(expr *base, expr *exponent) { return makebinary(EXPR_EXP, base, exponent); }
expr *sub(expr *left, expr *right) { return makebinary(EXPR_SUB, left, right); }
expr *neg(expr *arg) { return makeunary(EXPR_NEG, arg); }
expr *logarithm(expr *arg) { return makeunary(EXPR_LOG, arg); }
expr *sine(expr *arg) { return makeunary(EXPR_SIN, arg); }
expr *cosine(expr *arg) { return makeunary(EXPR_COS, arg); }

expr *deriveSin(expr *f, expr *sym) {
	return mul(derive(f->unary, sym), cosine(retain(f->unary)));
}

expr *deriveCos(expr *f, expr *sym) {
	return mul(derive(f->unary, sym), neg(sine(retain(f->unary))));
}

// d/dx(f^g) = f^g * (g'ln(f) + gf'/f)
expr *deriveExp(expr *func, expr *sym) {
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

expr *deriveMul(expr *f, expr *sym) {
	expr *left = mul(derive(f->left, sym), retain(f->right));
	expr *right = mul(retain(f->left), derive(f->right, sym));
	return add(left, right);
}

expr *deriveFrac(expr *f, expr *sym) {
	expr *denom = exponential(retain(f->right), two);

	expr *first = mul(derive(f->left, sym), retain(f->right));
	expr *second = mul(retain(f->left), derive(f->right, sym));

	return frac(sub(first, second), denom);
}

expr *deriveAdd(expr *f, expr *sym) {
	expr *left = derive(f->left, sym);
	expr *right = derive(f->right, sym);

	return add(left, right);
}

expr *deriveSub(expr *f, expr *sym) {
	expr *left = derive(f->left, sym);
	expr *right = derive(f->right, sym);

	return sub(left, right);
}

expr *deriveNeg(expr *f, expr *sym) {
	return neg(derive(f->unary, sym));
}

expr *deriveLog(expr *f, expr *sym) {
	return frac(derive(f->unary, sym), retain(f->unary));
}

expr *derive(expr *f, expr *sym) {
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
		case EXPR_SYM: return eq(f, sym) ? one : zero;
		case EXPR_NUM: return zero;
		default:
			printf("Operation not handled yet\n");
			return NULL;
	}
}

expr *simplifyMul(expr *f, expr *sym) {
	expr *left = simplify(f->left, sym);
	expr *right = simplify(f->right, sym);

	if (eq(left, zero) || eq(right, zero)) {
		release(left); release(right);
		return zero;
	} else if (eq(left, one)) {
		release(left); return right;
	} else if (eq(right, one)) {
		release(right); return left;
	} else if (left->type == EXPR_NUM && right->type == EXPR_NUM) {
		int res = left->num * right->num;
		release(left); release(right);
		return num(res);
	}
	if (eq(left, right)) {
		release(right);
		return simplify(exponential(left, num(2)), sym);
	}

	if (contains(left, sym) && !contains(right, sym)) {
		expr *tmp = left;
		left = right;
		right = tmp;
	}

	return mul(left, right);
}

expr *simplifyFrac(expr *f, expr *sym) {
	expr *numer = simplify(f->left, sym);
	expr *denom = simplify(f->right, sym);

	if (eq(numer, zero)) { release(denom); release(numer); return zero; }
	if (eq(denom, one)) {
		release(denom); return numer;
	}
	if (numer->type == EXPR_NUM && denom->type == EXPR_NUM) {
		int res = numer->num / denom->num;
		release(numer); release(denom);
		return num(res);
	}
	return frac(numer, denom);
}

expr *simplifyAdd(expr *f, expr *sym) {
	expr *left = simplify(f->left, sym);
	expr *right = simplify(f->right, sym);
	if (eq(left, zero)) { release(left); return right; }
	if (eq(right, zero)) { release(right); return left; }
	if (is(left, EXPR_NUM) && is(right, EXPR_NUM)) {
		int res = left->num + right->num;
		release(left); release(right);
		return num(res);
	}
	if (eq(left, right)) {
		release(left);
		return mul(num(2), right);
	}
	if (is(right, EXPR_NEG)) {
		expr *tmp = retain(right->unary);
		release(right);
		return simplify(sub(left, tmp), sym);
	}

	if (contains(left, sym) && !contains(right, sym)) {
		expr *tmp = left;
		left = right;
		right = tmp;
	}

	return add(left, right);
}

expr *simplifySub(expr *f, expr *sym) {
	expr *left = simplify(f->left, sym);
	expr *right = simplify(f->right, sym);
	if (eq(left, zero)) { release(left); return neg(right); }
	if (eq(right, zero)) { release(right); return left; }

	if (is(left, EXPR_NUM) && is(right, EXPR_NUM)) {
		int res = left->num - right->num;
		release(left); release(right);
		return num(res);
	}
	if (is(right, EXPR_NEG)) {
		expr *tmp = retain(right->unary);
		release(right);
		return simplify(add(left, tmp), sym);
	}
	return sub(left, right);
}

expr *simplifyNeg(expr *f, expr *sym) {
	expr *simplified = simplify(f->unary, sym);
	if (eq(simplified, zero)) {
		release(simplified);
		return zero;
	}
	if (is(simplified, EXPR_NUM)) {
		expr *res = num(-simplified->num);
		release(simplified);
		return res;
	}
	return neg(simplified);
}

expr *simplifyExp(expr *f, expr *sym) {
	expr *base = simplify(f->left, sym);
	expr *exponent = simplify(f->right, sym);

	if (eq(base, one)) { release(exponent); return base; }
	if (eq(exponent, one)) { release(exponent); return base; }
	if (eq(base, zero)) {
		release(exponent);
		release(base);
		return zero;
	}
	if (eq(exponent, zero)) {
		release(exponent);
		release(base);
		return one;
	}
	return exponential(base, exponent);
}

expr *simplify(expr *f, expr *sym) {
	switch (f->type) {
	case EXPR_MUL: return simplifyMul(f, sym);
	case EXPR_ADD: return simplifyAdd(f, sym);
	case EXPR_SUB: return simplifySub(f, sym);
	case EXPR_FRAC: return simplifyFrac(f, sym);
	case EXPR_EXP: return simplifyExp(f, sym);
	case EXPR_NEG: return simplifyNeg(f, sym);
	}
	return retain(f);
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
	if (f->type == EXPR_NUM) { printf("%f", f->num); return; }

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

void tokenize(context *ctx, char *buff) {
	token *tok = NULL;
	while (*buff) {
		tok = NULL;
		while (isspace(*buff)) buff++;
		if (!*buff) break;

		if (isnumber(*buff)) {
			char *curr = buff;
			while (isnumber(*curr)) curr++;
			
			size_t len = curr - buff;

			char *num = strndup(buff, len+1);
			num[len] = 0;
			tok = makenum(atoi(num));
			addtok(ctx, tok);
			buff = curr;
			continue;
		} else if (*buff == '(') {
			tok = maketok(TOK_LPAREN, *buff);
		} else if (*buff == ')') {
			tok = maketok(TOK_RPAREN, *buff);
		} else if (*buff == '^') {
			tok = maketok(TOK_POW, *buff);
		} else if (*buff == '+') {
			tok = maketok(TOK_PLUS, *buff);
		} else if (*buff == '-') {
			tok = maketok(TOK_MINUS, *buff);
		} else if (*buff == '/') {
			tok = maketok(TOK_SLASH, *buff);
		} else if (*buff == '*') {
			tok = maketok(TOK_STAR, *buff);
		}

		if (!tok) {
			printf("Unexpected character: '%c'", *buff);
			break;
		}

		buff++;
		addtok(ctx, tok);
	}
}

void printTokens(context *ctx) {
	for (size_t i = 0; i < ctx->len; i++) {
		token *tok = ctx->iter[i];
		if (tok->type == TOK_NUM) {
			printf("%d\n", tok->num);
		} else {
			printf("%c\n", tok->str);
		}
	}
}

void repl() {
	context *ctx = makecontext();
	char buff[1024];

	while (1) {
		ctx->len = 0;
		printf(">> ");
		scanf("%s", buff);

		if (strcmp(buff, "exit") == 0) {
			printf("Goodbye by marcomit!\n");
			break;
		} else if (strcmp(buff, "clear") == 0) {
			printf("\033[2J\033[0H");
		} else {
			tokenize(ctx, buff);
			printTokens(ctx);
		}
	}
}

int main(void) {
	zero = num(0);
	one = num(1);
	two = num(2);

	repl();

	// expr *composed = mul(
	// 	mul(exponential(sym("a"), num(2)), sym("b")),
	// 	sub(
	// 		sym("b"), neg(sym("b"))
	// 	)
	// );
	// print(composed);
	// printf("\n");
	//
	// expr *a = sym("a");
	// expr *derived = derive(composed, a);
	// printf("Derived:\n");
	// print(derived);
	// printf("\n");
	// printf("Simplified:\n");
	// print(simplify(derived, a));
	// // print(derive(derived, "b"));
	//
	// release(a);
	return 0;
}
