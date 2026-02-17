#include <limits.h>
#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <readline/readline.h>
#include <readline/history.h>

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
#define EXPR_FUNC	(12 | EXPR_UNARY_MASK)

typedef struct expr {
	int type;
	int mindepth, maxdepth, nodes;
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
	TOK_STAR,
	TOK_EQ,
	TOK_PRIME,
	TOK_LOG,
	TOK_SIN,
	TOK_COS
} toktype;

typedef struct {
	toktype type;
	union {
		char *str;
		int num;
	};
} token;

typedef struct {
	char *name;
	expr *value;
} func;

typedef struct {
	token **iter;
	size_t len;
	size_t cap;
	size_t curr;
	
	struct {
		func **iter;
		size_t len;
		size_t cap;
	} funcs;

} context;

typedef expr *(*ParseFunc)(context *);

static expr *zero = NULL;
static expr *one = NULL;
static expr *two = NULL;

expr *retain(expr *);
expr *release(expr *);
expr *derive(expr *, expr *);
expr *simplify(expr *, expr *);
expr *neg(expr *);
expr *release(expr *);
expr *parse(context *);
expr *parseTerm(context *);
func *lookupfunc(context *, char *);


context *makecontext() {
	context *self = malloc(sizeof(context));
	self->len = 0;
	self->cap = 8;
	self->curr = 0;
	self->iter = malloc(sizeof(token *) * self->cap);

	self->funcs.cap = 8;
	self->funcs.len = 0;
	self->funcs.iter = malloc(sizeof(func *) * self->funcs.cap);
	return self;
}

func *makefunc(char *name, expr *f) {
	func *self = malloc(sizeof(func));
	self->name = name;
	self->value = retain(f);
	return self;
}

int hasNext(context *ctx) { return ctx->curr < ctx->len; }
token *peek(context *ctx) { 
	if (!hasNext(ctx)) return NULL;
	return ctx->iter[ctx->curr];
}
token *next(context *ctx) {
	if (!hasNext(ctx)) return NULL;
	return ctx->iter[ctx->curr++];
}

void freecontext(context *ctx) {
	for (size_t i = 0; i < ctx->len; i++) {
		free(ctx->iter[i]);
	}
	free(ctx->iter);
	free(ctx);
}

void addtok(context *ctx, token *tok) {
	if (ctx->len >= ctx->cap) {
		ctx->cap *= 2;
		ctx->iter = realloc(ctx->iter, ctx->cap * sizeof(token *));
	}
	ctx->iter[ctx->len++] = tok;
}

void addfunc(context *ctx, char *name, expr *f) {
	func *function = lookupfunc(ctx, name);

	if (function) {
		release(function->value);
		function->value = retain(f);
		return;
	}

	if (ctx->funcs.len >= ctx->funcs.cap) {
		ctx->funcs.cap *= 2;
		ctx->funcs.iter = realloc(ctx->funcs.iter, ctx->funcs.cap * sizeof(func *));
	}
	ctx->funcs.iter[ctx->funcs.len++] = makefunc(name, f);
}

token *maketok(toktype type) {
	token *self = malloc(sizeof(token));
	self->type = type;
	return self;
}

token *makesym(char *str) {
	token *self = maketok(TOK_SYM);
	self->str = str;
	return self;
}

token *makenum(int num) {
	token *self = maketok(TOK_NUM);
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

func *lookupfunc(context *ctx, char *name) {
	for (size_t i = 0; i < ctx->funcs.len; i++) {
		if (strcmp(ctx->funcs.iter[i]->name, name) == 0)
			return ctx->funcs.iter[i];
	}
	return NULL;
}

expr *findFirstSym(expr *f) {
	if (!f) return NULL;
	if (f->type == EXPR_SYM) return f;
	if (f->type & EXPR_BINARY_MASK) {
		expr *found = findFirstSym(f->left);
		if (found) return found;
		return findFirstSym(f->right);
	}
	if (f->type & EXPR_UNARY_MASK) return findFirstSym(f->unary);
	return NULL;
}

expr *sym(char *symbol) {
	expr *self = makeexpr(EXPR_SYM, 0, 0);
	self->symbol = strdup(symbol);
	return self;
}

expr *num(float n) {
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
	self->nodes = 1 + left->nodes + right->nodes;
	return self;
}

expr *makeunary(int type, expr *op) {
	expr *self = makeexpr(type, op->mindepth, op->maxdepth);
	self->unary = op;
	self->nodes = 1 + op->nodes;
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

int grade(expr *f) {
	if (!f) return INT_MAX;
	if (is(f, EXPR_SYM)) return 1;
	if (flag(f, EXPR_BINARY_MASK)) {
		int left = grade(f->left);
		int right = grade(f->right);
		if (left == INT_MAX && right == INT_MAX) return INT_MAX;
		return 1 + min(left, right);
	} else if (flag(f, EXPR_UNARY_MASK)) {
		int res = grade(f->unary);
		if (res == INT_MAX) return INT_MAX;
		return 1 + res;
	} else {
		return INT_MAX;
	}
}

expr *copy(expr *self) {
	if (!self) return self;

	expr *cloned = makeexpr(self->type, self->mindepth - 1, self->maxdepth - 1);
	cloned->nodes = self->nodes;

	if (self->type == EXPR_SYM) {
		cloned->symbol = strdup(self->symbol);
	} else if (self->type == EXPR_NUM) {
		cloned->num = self->num;
	} else if (self->type & EXPR_BINARY_MASK) {
		cloned->left = copy(self->left);
		cloned->right = copy(self->right);
	} else if (self->type & EXPR_UNARY_MASK) {
		cloned->unary = copy(self->unary);
	}

	return cloned;
}

expr *frac(expr *numer, expr *denom) { return makebinary(EXPR_FRAC, numer, denom); }
expr *mul(expr *left, expr *right) { return makebinary(EXPR_MUL, left, right); }
expr *add(expr *left, expr *right) { return makebinary(EXPR_ADD, left, right); }
expr *exponential(expr *base, expr *exponent) { return makebinary(EXPR_EXP, base, exponent); }
expr *squared(expr *base) { return exponential(base, two); }
expr *sub(expr *left, expr *right) { return makebinary(EXPR_SUB, left, right); }
expr *neg(expr *arg) { return makeunary(EXPR_NEG, arg); }
expr *logarithm(expr *arg) { return makeunary(EXPR_LOG, arg); }
expr *sine(expr *arg) { return makeunary(EXPR_SIN, arg); }
expr *cosine(expr *arg) { return makeunary(EXPR_COS, arg); }
expr *function(expr *arg) { return makeunary(EXPR_FUNC, arg); }

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
		default: return NULL;
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
		float res = left->num * right->num;
		release(left); release(right);
		return num(res);
	} else if (is(left, EXPR_EXP) &&
							is(right, EXPR_EXP) &&
							eq(left->left, right->left)) {
		expr *res = exponential(retain(left->left),
						 	simplify(
								add(
									retain(left->right),
									retain(right->right)
								), sym));
		release(left); release(right);
		return res;
	} else if (is(left, EXPR_EXP) && eq(left->left, right)) { 
		expr *res = exponential(
			retain(left->left),
			add(one, retain(left->right))
		);
		release(left); release(right);
		return simplify(res, sym);
	} else if (is(right, EXPR_EXP) && eq(right->left, left)) {
		expr *res = exponential(
			retain(right->left),
			simplify(add(retain(right->right), one), sym)
		);
		release(left); release(right);
		return simplify(res, sym);
	} else if (eq(left, right)) {
		release(right);
		return simplify(squared(left), sym);
	}

	if ((contains(left, sym) && !contains(right, sym)) ||
			(grade(left) < grade(right))) {
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
	} else if (eq(numer, denom)) {
		release(numer); release(denom);
		return one;
	} else if (numer->type == EXPR_NUM && denom->type == EXPR_NUM) {
		float res = numer->num / denom->num;
		release(numer); release(denom);
		return num(res);
	} else if (is(numer, EXPR_EXP) &&
							is(denom, EXPR_EXP) &&
							eq(numer->left, denom->left)) {
		expr *res = exponential(
			retain(numer->left),
			sub(
				retain(numer->right),
				retain(denom->right)
			)
		);
		release(numer); release(denom);
		return simplify(res, sym);
	} else if (is(numer, EXPR_EXP) && eq(numer->left, denom)) {
		expr *res = exponential(retain(numer->left), 
													sub(retain(numer->right), one));
		release(numer); release(denom);
		return simplify(res, sym);
	} else if (is(denom, EXPR_EXP) && eq(denom->left, numer)) {
		expr *res = frac(
			one,
			exponential(
				retain(denom->left),
				sub(retain(denom->right), one)
			)
		);
		release(denom); release(numer);
		return simplify(res, sym);
	}
	return frac(numer, denom);
}

expr *simplifyAdd(expr *f, expr *sym) {
	expr *left = simplify(f->left, sym);
	expr *right = simplify(f->right, sym);
	if (eq(left, zero)) { release(left); return right; }
	if (eq(right, zero)) { release(right); return left; }
	if (is(left, EXPR_NUM) && is(right, EXPR_NUM)) {
		float res = left->num + right->num;
		release(left); release(right);
		return num(res);
	}
	if (eq(left, right)) {
		release(left);
		return mul(two, right);
	}
	if (is(right, EXPR_NEG)) {
		expr *tmp = retain(right->unary);
		release(right);
		return simplify(sub(left, tmp), sym);
	}

	if ((contains(left, sym) && !contains(right, sym)) ||
			(grade(left) < grade(right))) {
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
		float res = left->num - right->num;
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
	if (is(simplified, EXPR_NEG)) {
		expr *res = retain(simplified->unary);
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
	if (is(base, EXPR_NUM) && is(exponent, EXPR_NUM)) {
		float res = powf(base->num, exponent->num);
		release(base); release(exponent);
		return num(res);
	}
	if (is(base, EXPR_EXP)) {
		expr *res = exponential(
			retain(base->left),
			simplify(mul(retain(base->right), exponent), sym)
		);
		release(base);
		return res;
	}
	return exponential(base, exponent);
}

expr *simplifyBuiltin(expr *f, expr *sym, float (*compute)(float)) {
	expr *simplified = simplify(f->unary, sym);

	if (is(simplified, EXPR_NUM)) {
		float res = compute(simplified->num);
		release(simplified);
		return num(res);
	}
	return makeunary(f->type, simplified);
}

expr *simplify(expr *f, expr *sym) {
	switch (f->type) {
	case EXPR_MUL: return simplifyMul(f, sym);
	case EXPR_ADD: return simplifyAdd(f, sym);
	case EXPR_SUB: return simplifySub(f, sym);
	case EXPR_EXP: return simplifyExp(f, sym);
	case EXPR_NEG: return simplifyNeg(f, sym);
	case EXPR_SIN: return simplifyBuiltin(f, sym, sinf);
	case EXPR_COS: return simplifyBuiltin(f, sym, cosf);
	case EXPR_LOG: return simplifyBuiltin(f, sym, logf);
	case EXPR_FRAC: return simplifyFrac(f, sym);
	}
	return retain(f);
}

int precedence(expr *f) {
	switch (f->type) {
		case EXPR_ADD: case EXPR_SUB: return 1;
		case EXPR_MUL:	return 2;
		case EXPR_FRAC: return 3;
		case EXPR_EXP: return 4;
		default: return 10;
	}
}

void print(expr *f) {
	if (f->type == EXPR_SYM) { printf("%s", f->symbol); return; }
	if (f->type == EXPR_NUM) { printf("%g", f->num); return; }

	if (f->type == EXPR_NEG) {
		int parens = f->unary->type & EXPR_BINARY_MASK;
		printf("-");
		if (parens) printf("(");
		print(f->unary);
		if (parens) printf(")");
		return;
	}

	if (f->type == EXPR_FUNC) {
		print(f->unary);
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

expr *assign(expr *f, expr *old, expr *new) {
	if (!f) return f;
	if (eq(f, old)) { freeexpr(f); return new; }

	if (flag(f, EXPR_BINARY_MASK)) {
		f->left = assign(f->left, old, new);
		f->right = assign(f->right, old, new);
	} else if (flag(f, EXPR_UNARY_MASK)) {
		f->unary = assign(f->unary, old, new);
	}

	return f;
}

expr *calc(expr *f, expr *sym, int val) {
	return simplify(assign(copy(f), sym, num(val)), sym);
}

int tokenize(context *ctx, char *buff) {
	token *tok = NULL;
	while (*buff) {
		tok = NULL;
		while (isspace(*buff)) buff++;
		if (!*buff) break;

		if (strncmp(buff, "log", 3) == 0 && !isalpha(buff[3])) {
			tok = maketok(TOK_LOG); buff += 2;
		} else if (strncmp(buff, "sin", 3) == 0 && !isalpha(buff[3])) {
			tok = maketok(TOK_SIN); buff += 2;
		} else if (strncmp(buff, "cos", 3) == 0 && !isalpha(buff[3])) {
			tok = maketok(TOK_COS); buff += 2;
		} else if (isalpha(*buff)) {
			char *start = buff;
			while (isalpha(*buff)) buff++;

			tok = makesym(strndup(start, buff - start));
			addtok(ctx, tok);
			continue;
		} else if (isnumber(*buff)) {
			char *start = buff;
			while (isnumber(*buff)) buff++;
			size_t len = buff - start;
			char *num = strndup(start, len);
			tok = makenum(atoi(num));
			addtok(ctx, tok);
			free(num);
			continue;
		} else if (*buff == '(') {
			tok = maketok(TOK_LPAREN);
		} else if (*buff == ')') {
			tok = maketok(TOK_RPAREN);
		} else if (*buff == '^') {
			tok = maketok(TOK_POW);
		} else if (*buff == '+') {
			tok = maketok(TOK_PLUS);
		} else if (*buff == '-') {
			tok = maketok(TOK_MINUS);
		} else if (*buff == '/') {
			tok = maketok(TOK_SLASH);
		} else if (*buff == '*') {
			tok = maketok(TOK_STAR);
		} else if (*buff == '=') {
			tok = maketok(TOK_EQ);
		} else if (*buff == '\'') {
			tok = maketok(TOK_PRIME);
		}
		if (!tok) {
			printf("Unexpected character: '%c'\n", *buff);
			return 0;
		}

		buff++;
		addtok(ctx, tok);
	}
	return 1;
}

/* (1+2)^3 - (4-5*6)^(7-8)
 *       ^
 * unary = num | sym | "-" unary | "(" expr ")"
 * postfix = unary "^" unary | unary
 * fact = postfix ( ("*" | "/") postfix )*
 * term = fact ( ("+" | "-") fact )*
 * expr = term | func
 * func = word "=" expr
 * */

expr *parseUnary(context *ctx) {
	if (!hasNext(ctx)) return NULL;
	token *curr = next(ctx);
	if (curr->type == TOK_NUM) {
		return num(curr->num);
	} else if (curr->type == TOK_SYM) {
		func *f = lookupfunc(ctx,curr->str);
		return f ? retain(f->value) : sym(curr->str);
	} else if (curr->type == TOK_SIN ||
							curr->type == TOK_COS ||
							curr->type == TOK_LOG) {
		if (!hasNext(ctx) || next(ctx)->type != TOK_LPAREN) {
			return NULL;
		}
		expr *arg = parse(ctx);
		if (!hasNext(ctx) || next(ctx)->type != TOK_RPAREN) return NULL;

		expr *(*f)(expr *) = sine;
		if (curr->type == TOK_COS) f = cosine;
		else if (curr->type == TOK_LOG) f = logarithm;
		return f(arg);
	} else if (curr->type == TOK_MINUS) {
		expr *arg = parseUnary(ctx);
		if (!arg) return NULL;
		return neg(arg);
	} else if (curr->type == TOK_LPAREN) {

		expr *arg = parse(ctx);

		if (!arg || !hasNext(ctx) ||
				next(ctx)->type != TOK_RPAREN) {
			return NULL;
		}

		return arg;
	}
	return NULL;
}

expr *parsePow(context *ctx, expr *base) {
	token *curr = next(ctx);
	if (!curr || curr->type != TOK_POW) return NULL;

	expr *exponent = parseUnary(ctx);
	if (!exponent) return NULL;

	return exponential(base, exponent);
}

expr *parseDerive(context *ctx, expr *f) {
	if (!hasNext(ctx) || next(ctx)->type != TOK_PRIME) return NULL;
	if (!hasNext(ctx) || next(ctx)->type != TOK_LPAREN) return NULL;
	token *x = next(ctx);
	if (x->type != TOK_SYM) return NULL;

	if (!hasNext(ctx) || next(ctx)->type != TOK_RPAREN) return NULL;

	expr *s = sym(x->str);
	expr *simplified = simplify(f, s);
	expr *derived = derive(simplified, s);
	expr *res = simplify(derived, s);
	release(derived);
	release(simplified);
	release(s);
	return res;
}

expr *parseCalc(context *ctx, expr *f) {
	if (!hasNext(ctx) || next(ctx)->type != TOK_LPAREN) return NULL;
	token *v = next(ctx);
	if (v->type != TOK_SYM) return NULL;
	if (!hasNext(ctx) || next(ctx)->type != TOK_EQ) return NULL;
	token *n = next(ctx);
	if (n->type != TOK_NUM) return NULL;
	if (!hasNext(ctx) || next(ctx)->type != TOK_RPAREN) return NULL;
	expr *s = sym(v->str);
	expr *res = calc(f, s, n->num);
	release(s);
	return res;
}

expr *parsePostfix(context *ctx) {
	expr *unary = parseUnary(ctx);
	if (!unary) return NULL;



	expr *(*funcs[])(context *, expr *) = {
		parsePow, parseDerive, parseCalc
	};

	do {
		size_t saved = ctx->curr;
		expr *tmp = NULL;

		for (size_t i = 0; i < sizeof(funcs) / sizeof(funcs[0]); i++) {
			expr *parsed = funcs[i](ctx, unary);
			if (parsed) {
				tmp = parsed;
				break;
			} else {
				ctx->curr = saved;
			}
		}

		if (!tmp) break;

		unary = tmp;
	} while (1);
	return unary;
}

expr *parseFactor(context *ctx) {
	expr *left = parsePostfix(ctx);
	expr *right = NULL;

	if (!left) return NULL;

	token *curr = peek(ctx);
	while (hasNext(ctx) &&
				(curr->type == TOK_SLASH ||
					curr->type == TOK_STAR ||
					curr->type == TOK_SYM ||
					curr->type == TOK_NUM)) {
		int ismul = curr->type != TOK_SLASH;
		if (curr->type != TOK_SYM && curr->type != TOK_NUM) next(ctx);
		right = parsePostfix(ctx);
		if (!right) return NULL;

		left = (ismul ? mul : frac)(left, right);
		curr = peek(ctx);
	}
	return left;
}

expr *parseTerm(context *ctx) {
	expr *left = parseFactor(ctx);
	expr *right = NULL;

	if (!left) return NULL;

	token *curr = peek(ctx);
	while (hasNext(ctx) &&
				(curr->type == TOK_PLUS || curr->type == TOK_MINUS)) {
		int isplus = curr->type == TOK_PLUS;
		next(ctx);
		right = parseFactor(ctx);
		if (!right) return NULL;

		left = (isplus ? add : sub)(left, right);
		curr = peek(ctx);
	}
	return left;
}

expr *parseFunc(context *ctx) {
	if (!hasNext(ctx)) return NULL;

	token *curr = peek(ctx);
	if (!curr || curr->type != TOK_SYM) return NULL;
	next(ctx);

	if (!hasNext(ctx) || peek(ctx)->type != TOK_EQ) return NULL;
	next(ctx);

	expr *parsed = parse(ctx);
	if (!parsed) return NULL;

	addfunc(ctx, strdup(curr->str), parsed);
	return function(parsed);
}

expr *parseOr(context *ctx, ParseFunc *funcs, size_t len) {
	size_t saved = ctx->curr;

	for (size_t i = 0; i < len; i++) {
		expr *parsed = funcs[i](ctx);
		if (parsed) return parsed;
		ctx->curr = saved;
	}
	return NULL;
}

expr *parse(context *ctx) {
	ParseFunc funcs[] = { parseFunc, parseTerm };
	return parseOr(ctx, funcs, sizeof(funcs) / sizeof(funcs[0]));
}

void repl() {
	context *ctx = makecontext();
	char *buff;

	while (1) {
		ctx->len = 0;
		ctx->curr = 0;

		buff = readline(">> ");

		if (!buff) break;
		add_history(buff);

		if (strcmp(buff, "exit") == 0) {
			printf("Goodbye by marcomit!\n");
			break;
		} else if (strcmp(buff, "clear") == 0) {
			printf("\033[2J\033[0H");
		} else {
			if (!tokenize(ctx, buff)) continue;

			expr *parsed = parse(ctx);

			for (size_t i = 0; i < ctx->len; i++) free(ctx->iter[i]);

			if (!parsed || hasNext(ctx)) {
				printf("Invalid expression\n");
				continue;
			}
			expr *simplified = simplify(parsed, NULL);
			print(simplified);
			release(simplified);
			release(parsed);
			printf("\n\n");
		}
	}
}

int main(void) {
	zero = num(0);
	one = num(1);
	two = num(2);

	repl();
	return 0;
}
