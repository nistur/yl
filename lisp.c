#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// using stretchy buffers from nothings.org as it's a perfectly good
// minimal solution, and I don't want to rewrite _everything_ from scratch
#include "stretchy_buffer.h"

//----------------------------------------------------------------------------//
// Incredibly minimalist lisp                                                 //
//----------------------------------------------------------------------------//
// I am not sure why I've added a small lisp interpreter here, but I thought  //
// it would be fun. It supports very little functionality, with more intended //
// to be implemented in lisp itself. At this point it only supports a few     //
// things:                                                                    //
// println - output parameters followed by a new line                         //
// set - set a variable to something                                          //
// + - perform arithmetric summation between all parameters                   //
// lambda - define a function                                                 //
// ' - don't evaluate the following cell                                      //
//----------------------------------------------------------------------------//
// EXAMPLE                                                                    //
// (set 'testValue 1)                                                         //
// (set 'testFn lambda()                                                      //
//      (println (set 'testValue (+ testValue 1))))                           //
// (testFn)                                                                   //
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
// Macros                                                                     //
//----------------------------------------------------------------------------//
// Unused variables
#define UNUSED(x) (void)(x)
// Allow current struct to be reference counted
#define REFCOUNT ref_t _ref;
// Release memory
#define FREE(v) Free((cell_base_t*)v)
// Increase reference count - hold reference to a struct
#define RETAIN(_val) { ((ref_t*)_val)->_refCount += 1; }
// Decrease reference count - Free memory if no references left
#define RELEASE(_val) { if((((ref_t*)_val)->_refCount-= 1) <= 0) { FREE(_val); }}
#define __SET(XX,val,e) {sb_push(e->keys, XX); sb_push(e->vals, (cell_base_t*)val); RETAIN(val);}
// Set a value to a given name-hash (prefer SET)
#define _SET(XX,val)  __SET(XX,val,env)
// Set a value to a given name
#define SET(name,val) _SET(hash(name), val) 
// Get a named value
#define GET(name) Get(env, hash(name))
// Replace a named value with a new one
#define REPLACE(name, val) Replace(env, hash(name), val);
// Create a new environment
#define ENV() _env()
// Create a new cell of a given type with specific value
#define CELL(t, v) _cell(t, (void*)v)
// Create a new list
#define LIST() (list_t*)CELL(LIST,0)
// Create a new Lisp function
#define FUNL() (fn_t*)CELL(FUNL,0)
// Quote the following cell
#define QUOTE() (cell_t*)CELL(QUOT,0)
// Push a value onto a list
#define PUSH_BACK(list, val)						\
    {									\
	list_t* l = list;						\
	while(NOT_NIL(CAR(l)))						\
	{								\
	    if(CDR(l) == NIL) l->pair.cdr = (cell_base_t*)LIST();	\
	    l = (list_t*)CDR(l);					\
	}								\
	l->pair.car = (cell_base_t*)val;				\
    }

#define NOT_NIL(x) ((x != NULL) && (((cell_base_t*)x) != NIL))

#define CAR(x)   ((NOT_NIL(x) && ((cell_base_t*)x)->t == LIST) ? (((list_t*)x)->pair.car) : NIL)
#define CDR(x)   ((NOT_NIL(x) && ((cell_base_t*)x)->t == LIST) ? (((list_t*)x)->pair.cdr) : NIL)
#define CADR(x ) (CAR(CDR(x)))
#define CADDR(x) (CAR(CDR(CDR(x))))
#define CDDR(x) (CDR(CDR(x)))

//----------------------------------------------------------------------------//
// Types                                                                      //
//----------------------------------------------------------------------------//
// Symbols in this lisp are just a standard C null terminated string
typedef char* sym_t;

// Simple reference counting setup, use REFCOUNT to use
typedef struct
{
    int _refCount;
} ref_t;

// Different internal types
typedef enum
{
    SYM, // string value - usually variable name
    LIST,// list
    VAL, // integer value
    BOOL,// boolean value
    NUM, // float value
    FUNL,// Lisp fn
    FUNC,// C fn
    QUOT,// '
    STRING,// string value
} cell_type;

// C function pointer
struct _env_t;
struct cell_base_t;
typedef struct _cell_base_t* (*func)(struct _cell_base_t*, struct _env_t* env);

typedef struct _pair_t
{
    struct _cell_base_t* car;
    struct _cell_base_t* cdr;
} pair_t;

// base type for all cells - handles type lookup
typedef struct _cell_base_t
{
    REFCOUNT;
    cell_type t;
    union
    {
	sym_t sym;
	int val;
	func func;
	struct _cell_base_t* inner;
	pair_t pair;
    };
} cell_base_t, cell_t, list_t, *ast_t;


void Free(cell_base_t* cell);

/*// Basic list type, contains references to the head, and rest of the list
typedef struct _list_t
{
    cell_base_t _base;
    cell_base_t* car;
    cell_base_t* cdr;
    } list_t, *ast_t;*/

// Lisp function, params used for renaming parameters, body contains executablt
typedef struct _fn_t
{
    cell_base_t _base;
    list_t*      params;
    list_t*      body;
} fn_t;


// Basic cell type. Contains support for string, integer and C function types
/*typedef struct _cell_t
{
    cell_base_t _base;
    union { sym_t sym; int val; func func; cell_base_t* inner; };
} cell_t;*/

// Environment/scope, a simple hashmap lookup, with ability to have children
typedef struct _env_t
{
    int*           keys;
    cell_base_t**  vals;
    struct _env_t* _parent;
} *env_t;

//----------------------------------------------------------------------------//
// Static types                                                               //
//----------------------------------------------------------------------------//
cell_base_t* NIL;
cell_base_t* T;
cell_base_t* F;

//----------------------------------------------------------------------------//
// Utility functions                                                          //
//----------------------------------------------------------------------------//

// DJB hash
int hash(const char* str)
{
    int h = 5381;
    while(*str)
    {
	h = ((h <<5) + h) + *str;
	++str;
    }
    return h;
}

// Reading file
char* read_file_text(const char* fname)
{
  FILE* fp = fopen(fname, "r");
  fseek(fp, 0L, SEEK_END);
  long size = ftell(fp);
  fseek(fp, 0L, SEEK_SET);
  char* buffer=malloc(size + 1);
  fread(buffer, 1, size, fp);
  buffer[size] = '\0';
  fclose(fp);

  return buffer;
}

//----------------------------------------------------------------------------//
// Code starts here!                                                          //
//----------------------------------------------------------------------------//

// Create a new environment
env_t _env()
{
  env_t env = malloc(sizeof(struct _env_t));
  memset(env, 0, sizeof(struct _env_t));
  return env;
}

// Create a new cell
cell_base_t* _cell(cell_type t, void* val)
{
    cell_base_t* cell;
    if(t == LIST)
    {
	cell = malloc(sizeof(list_t));
	cell->pair.car = NIL;
	cell->pair.cdr = NIL;
    }
    else if(t == FUNL)
    {
	cell = malloc(sizeof(fn_t));
	memset(cell, 0, sizeof(fn_t));
    }
    else if(t == VAL)
    {
	cell =  malloc(sizeof(cell_t));
	memset(cell, 0, sizeof(cell_t));
	((cell_t*)cell)->val = (int)val;
    }
    else
    {
	cell = malloc(sizeof(cell_t));
	memset(cell, 0, sizeof(cell_t));
	((cell_t*)cell)->sym = (sym_t)val;
    }
    cell->t = t;
    RETAIN(cell);
    return cell;
}

// Retrieve a hashed, named cell from the current scope
cell_base_t* Get(env_t env, int hash)
{
    for(int i = 0; i < sb_count(env->keys); ++i)
    {
	if(env->keys[i] == hash)
	{
	    return env->vals[i];
	}
    }
    // If we got here, we didn't find it in the curren
    // scope, so try going up one level
    if( env->_parent )
    {
	return Get(env->_parent, hash);
    }
    return NIL;
}

const char* ESCAPE_CHARS = "ntr0";
const char* ESCAPE_CHARS_CONVERTED = "\n\t\r\0";
static char escape_char(char c) {
    const char* loc = strchr(ESCAPE_CHARS, c);
    char esc = 'X'; // default value for debugging
    if (loc != NULL) {
        size_t idx = loc - ESCAPE_CHARS;
        esc = ESCAPE_CHARS_CONVERTED[idx];
    }
    return esc;
}

static void str_copy_escaped(char* dest, const char* src, int len) {
    int escapeFlag = 0;
    int destIndex = 0;
    for (int i = 0; i < len; ++i) {
        if (escapeFlag == 0) {
            if (src[i] == '\\') {
                escapeFlag = 1;
            } else {
                dest[destIndex] = src[i];
                ++destIndex;
            }
        } else {
            dest[destIndex] = escape_char(src[i]);
            destIndex++;
            escapeFlag = 0;
        }
    }
}

// Parse an expression into a list, returns a pointer to the end
// of the expression
const char* STRING_TERMINALS = "\n\"\0";
const char* ParseList(list_t* list, const char* expr);
const char* ParseToken(cell_base_t** cell, const char* expr)
{
    const char* pTokStart = expr;
    const char* pTokEnd = pTokStart + 1;

    switch(*pTokStart)
    {
    case '\0':
	// early out - the end
	*cell = NIL;
	return pTokStart;
    case '\'':
	*cell = (cell_base_t*)QUOTE();
	return ParseToken(&((cell_t*)*cell)->inner, pTokEnd);
    case '"':
    {
	int escapeCount = 0;
	++pTokEnd;
	while(strchr(STRING_TERMINALS, *pTokEnd) == NULL) {
	    if (*pTokEnd == '\\') {
		++escapeCount;
		//we wanna skip the char after \ (it could be \ itself)
		++pTokEnd;
		if (strchr(STRING_TERMINALS, *pTokEnd) != NULL) {
		    // but not if it's the end of string
		    break;
		}
		++pTokEnd;
	    } else {
		++pTokEnd;
	    }
	}
	
	const char* strEnd = pTokEnd;
	const char lastChar = *pTokEnd;
	
	const char* strStart = pTokStart+1;
	int len = strEnd - strStart;
	char* sym = malloc(len + 1 - escapeCount);
	str_copy_escaped(sym, strStart, len);
	sym[len] = 0;
        *cell = CELL(STRING, sym);
	if (lastChar == '"') pTokEnd++;
	return pTokEnd;
    }
    case '(':
	*cell = (cell_base_t*)LIST();
	return ParseList(((list_t*)*cell), pTokEnd);
    case ')':
	*cell = NIL;
	return pTokEnd;
    }

    if( *pTokStart >= '0' && *pTokStart <= '9' )
    {
	while((*pTokEnd >= '0' && *pTokEnd <= '9') || *pTokEnd == '.')
	    ++pTokEnd;
	int len = pTokEnd - pTokStart;
	char* sym = malloc(len + 1);
	memcpy(sym, pTokStart, len);
	sym[len] = 0;
	*cell = CELL(VAL, atoi(sym));
	free(sym);
	return pTokEnd;
    }

    // everything else, we convert to a symbol
    
    while(*pTokEnd != '\0' && *pTokEnd !=  ' ' &&
	  *pTokEnd != '\n' && *pTokEnd != '\t' &&
	  *pTokEnd != '('  && *pTokEnd !=  ')' )
	++pTokEnd;
    int len = pTokEnd - pTokStart;
    char* sym = malloc(len + 1);
    memcpy(sym, pTokStart, len);
    sym[len] = 0;
    *cell = CELL(SYM, sym);
    return pTokEnd;
}

const char* ParseList(list_t* list, const char* expr)
{
    const char* pTokStart = expr;

#define SKIP_WHITESPACE(__x)						\
    while(*__x == ' ' || *__x == '\t' || *__x == '\n' ||		\
	  *__x == '\r' )						\
	++__x;

    SKIP_WHITESPACE(pTokStart);

    const char* pTokEnd = pTokStart + 1;

    while(1)
    {
	SKIP_WHITESPACE(pTokStart);
	cell_base_t* cell = NIL;
	pTokEnd = ParseToken(&cell, pTokStart);
	
	PUSH_BACK(list, cell);

	
	// next token
	pTokStart = pTokEnd;
	if( *pTokEnd == '\0' || *pTokStart == '\0' || cell == NIL ) break;
    }
    return pTokEnd;
}

void Consify(cell_base_t* cell)
{
    if(cell == NIL) return;
    if(cell->t == LIST)
    {
	cell_base_t* car = CAR(cell);
	cell_base_t* cdr = CDR(cell);
	
	Consify(car);
	Consify(cdr);
	
	if(car->t == LIST && CAR(car) == NIL && CDR(car) == NIL)
	    ((list_t*)cell)->pair.car = NIL;
	
	if(cdr->t == LIST && CAR(cdr) == NIL && CDR(cdr) == NIL)
	    ((list_t*)cell)->pair.cdr = NIL;
	else if(cdr->t == LIST && CAR(cdr) != NIL && CDR(cdr) == NIL)
	    ((list_t*)cell)->pair.cdr = CAR(cdr);
    }
    else if(cell->t == QUOT)
    {
	Consify(((cell_t*)cell)->inner);
    }
   
}

// Parse an expression into a syntax tree.
ast_t Parse(const char* expr)
{
    ast_t ast = LIST();
    ParseList(ast, expr);

//    Consify((cell_base_t*)ast);
    
    return ast;
}

// Evaluate a list and return the result
cell_base_t* Eval(cell_base_t* cell, env_t env)
{
    // if the value is quoted, don't evaluate it
    if(cell->t == QUOT)
    {
	return (cell_base_t*)((cell_t*)cell)->val;
    }

    if(cell->t == LIST)
    {
	cell_base_t* fn = Eval(CAR(cell), env);
	if(CAR(cell)->t == QUOT)
	{
	    // I don't quite know why quotes are wrapped
	    // in lists right now
	    return fn;
	}
	switch(fn->t)
	{
	case FUNC:
	    return ((cell_t*)fn)->func(CDR(cell), env);
	case FUNL:
	{
	    cell_base_t* params = CDR(cell);

	    if(params->t == LIST)
	    {
	        cell_base_t* name = (cell_base_t*)((fn_t*)fn)->params;
		cell_base_t* val = params;

		env_t scope = ENV();
		scope->_parent = env;
		while(NOT_NIL(name) && NOT_NIL(CAR(name)) && NOT_NIL(val) && NOT_NIL(CAR(val)))
		{
		    __SET(hash(((cell_t*)CAR(name))->sym),
			  Eval(val, env),
			  scope);
		    name = CDR(name);
		    val = CDR(val);
		}

		cell_base_t* res = Eval((cell_base_t*)((fn_t*)fn)->body, scope);
		// TODO: Make scope reference counted too, so
		// we can return lambdas
		free(scope);
		return res;
	    }
	}
	break;
	default:
	    break;
	}
	return NIL;
    }

    // If we find a symbol, look it up and replace with the relevant value
    if(cell->t == SYM)
    {
	return GET(((cell_t*)cell)->sym);
    }
    return cell;
}

// Cleanup memory from an unused cell
void Free(cell_base_t* cell)
{
    switch(cell->t)
    {
    case STRING:
    case SYM:
	free(((cell_t*)cell)->sym);
	break;
    case LIST:
    {
	while(NOT_NIL(cell))
	{
	    if(NOT_NIL(CAR(cell))) RELEASE(CAR(cell));
	    cell = CDR(cell);
	}
    }
    break;
    case QUOT:
	RELEASE(((cell_t*)((cell_t*)cell)->val));
	break;
    case FUNL:
      break;
    case VAL:
    case BOOL:
    case NUM:
    case FUNC:
      break; // these don't have any additional memory allocated
    }
    free(cell);
}

// Replace a value in memory if the name already exists
cell_base_t* Replace(env_t env, int hash, cell_base_t* val)
{
    env_t scope = env;
    while(scope)
    {
	for(int i = 0; i < sb_count(scope->keys); ++i)
	{
	    if(scope->keys[i] == hash)
	    {
		RELEASE(scope->vals[i]);
		scope->vals[i] = val;
		return val;
	    }
	}
	scope = scope->_parent;
    }
    // we couldn't find a matching name, so just set it
    _SET(hash, val);
    return val;
}

//----------------------------------------------------------------------------//
// Lisp standard functions                                                    //
//----------------------------------------------------------------------------//
cell_base_t* str(cell_base_t* cell, env_t env)
{
    cell_base_t* val = Eval(CAR(cell), env);
    char* buff = NULL;
    switch(val->t)
    {
    case SYM:
    case STRING:
	return val;
    case VAL:
    {
	buff = sb_add(buff, 128);;
	sprintf(buff, "%d", ((cell_t*)val)->val);
	cell_base_t* res = CELL(STRING, buff);
	sb_free(buff);
	return res;
    }
    case LIST:
    {
	buff = sb_add(buff, 2);
	sprintf(buff, "(");
	int p = 1;
	while(NOT_NIL(val) && NOT_NIL(CAR(val)))
	{
	    cell_t* strcar = (cell_t*)str(CAR(val), env);
	    int l = strlen(strcar->sym);
	    char* pbuff = sb_add(buff, l+1);
	    sprintf(pbuff-1, "%s ", strcar->sym);
	    p += l+1;
//	    RELEASE(strcar);
	    val = CDR(val);
	}

	if (NOT_NIL(val))
	{
	    cell_t* strcar = (cell_t*)str(val, env);
	    int l = strlen(strcar->sym);
	    char* pbuff = sb_add(buff, l+3);
	    sprintf(pbuff-1, ". %s ", strcar->sym);
	    p += l+3;
	}

	char* pbuff = sb_add(buff, p+2);
	pbuff[-2] = ')';
	pbuff[-1] = '\0';

	cell_base_t* res = CELL(STRING, buff);
	sb_free(buff);
	return res;
    }
    case BOOL:
    case NUM:
    case FUNL:
    case FUNC:
    case QUOT:
	break;
    };
    return NIL;
}

// Print list with newline
cell_base_t* println(cell_base_t* cell, env_t env)
{
    cell_base_t* res = str(cell, env);
    printf("%s\n", ((cell_t*)res)->sym);
    return NIL;
}

// arithemetic summation of all parameters
cell_base_t* plus(cell_base_t* cell, env_t env)
{
    int val = 0;
    while( NOT_NIL(cell) && NOT_NIL(CAR(cell)) )
    {
	cell_base_t* res = Eval(CAR(cell), env);
	if(res->t == SYM || res->t == STRING)
	{
	    val += atoi(((cell_t*)res)->sym);
	}
	else if( res->t == VAL)
	{
	    val += ((cell_t*)res)->val;
	}
	cell = CDR(cell);
    }
    return CELL( VAL, val );
}

cell_base_t* sub(cell_base_t* cell, env_t env)
{
    int val = 0;
    int first = 1;
    while( NOT_NIL(cell) && NOT_NIL(CAR(cell)) )
    {
	cell_base_t* res = Eval(CAR(cell), env);
	if(res->t == SYM || res->t == STRING)
	{
	    val-= atoi(((cell_t*)res)->sym);
	}
	else if( res->t == VAL)
	{
	    val -= ((cell_t*)res)->val;
	}

	if( first )
	{
	    first = 0;
	    val = -val;
	}
	cell = CDR(cell);
    }
    return CELL( VAL, val );
}

// set named variable
cell_base_t* set(cell_base_t* cell, env_t env)
{
    cell_base_t* name = Eval(CAR(cell), env);
    if( NOT_NIL(name) && name->t == SYM)
    {
	cell_base_t* val = Eval(CDR(cell), env);
	REPLACE(((cell_t*)name)->sym, val);
//	return val;
    }
    return NIL;
}

cell_base_t* cons(cell_base_t* cell, env_t env)
{
    list_t* lst = LIST();
    lst->pair.car = Eval(CAR(cell), env);
    lst->pair.cdr = Eval(CDR(cell), env);
    return (cell_base_t*)lst;
}
  

cell_base_t* car( cell_base_t* cell, env_t env)
{
    cell = Eval(cell, env);
    return CAR(cell);
}

cell_base_t* cdr( cell_base_t* cell, env_t env)
{
    cell = Eval(cell, env);
    return CDR(cell);
}

// define a lisp function
cell_base_t* lambda(cell_base_t* cell, env_t env)
{
  UNUSED(env);
    {
	fn_t* val = FUNL();
	if(cell->t == LIST)
	{
	    list_t* list = (list_t*)cell;
	    val->params = CAR(list);
	    val->body = CDR(list);
	    return &val->_base;
	}
    }
    return NIL;
}

cell_base_t* less(cell_base_t* cell, env_t env)
{
    cell_base_t* val1 = Eval(CAR(cell), env);
    cell_base_t* val2 = Eval(CADR(cell), env);

    int v1 = val1->t == SYM || val1->t == STRING ? atoi(((cell_t*)val1)->sym) : ((cell_t*)val1)->val;
    int v2 = val2->t == SYM || val2->t == STRING ? atoi(((cell_t*)val2)->sym) : ((cell_t*)val2)->val;
    
    return v1 < v2 ? T : F;
}

cell_base_t* not(cell_base_t* cell, env_t env)
{
    cell_base_t* v = Eval(cell, env);

    return (v == F || v == NIL) ? T : F;
}

cell_base_t* lisp_if(cell_base_t* cell, env_t env)
{    
    cell_base_t* test = Eval(CAR(cell), env);
    if( test && test != NIL && test != F && ((cell_t*)test)->val != 0)
    {
	return Eval(CADR(cell), env);
    }
    return Eval(CADDR(cell), env);
}

cell_base_t* lisp_read_file_text(cell_base_t* cell, env_t env)
{
    cell_base_t* nameval = Eval(cell, env);
    char* namestr = ((cell_t*)nameval)->sym;

    char* contentstr = read_file_text(namestr);

    return CELL(STRING, contentstr);
}

cell_base_t* lisp_while(cell_base_t* cell, env_t env)
{
    cell_base_t* test = Eval(CAR(cell), env);
    cell_base_t* ret = NIL;
    
    while( NOT_NIL(test) && test != F && ((cell_t*)test)->val != 0)
    {
        ret = Eval(CDR(cell), env);
	test = Eval(CAR(cell), env);
    }
    return ret;
}

void print_cell(cell_base_t* cell)
{
    static int depth = 0;

    for(int i = 0; i < depth; ++i) printf(" ");

    if(!NOT_NIL(cell))
    {
	printf("NIL\n");
	return;
    }
    
    switch(cell->t)
    {
    case SYM:
	printf("S:%s\n", ((cell_t*)cell)->sym);
	break;
    case LIST:
	printf("L:\n");
	++depth;
	print_cell(CAR(cell));
	print_cell(CDR(cell));
	--depth;
	break;
    case VAL:
	printf("V:%d\n", ((cell_t*)cell)->val);
	break;
    case BOOL:
	printf("B:\n");
	break;
    case NUM:
	printf("N:\n");
	break;
    case FUNL:
	printf("F:\n");
	break;
    case FUNC:
	printf("F:%p\n", ((cell_t*)cell)->func);
	break;
    case QUOT:
	printf("Q:\n");
	++depth;
	print_cell((cell_base_t*)((cell_t*)cell)->val);
	--depth;
	break;
    case STRING:
	printf("S:\"%s\"\n", ((cell_t*)cell)->sym);
	break;
    }
}

cell_base_t* print_cell_lisp(cell_base_t* cell, env_t env)
{
    if(cell->t == SYM)
	cell = GET(((cell_t*)cell)->sym);
    print_cell(cell);
    return NIL;
}

// main entry point
void lisp(const char* expr)
{
  env_t env = ENV();
    SET("print-cell", CELL(FUNC, print_cell_lisp));
    SET("println", CELL(FUNC, println));
    SET("+", CELL( FUNC, plus));
    SET("cons", CELL( FUNC, cons));
    SET("car", CELL( FUNC, car));
    SET("cdr", CELL( FUNC, cdr));
    SET("-", CELL( FUNC, sub));
    SET("<", CELL( FUNC, less));
    SET("not", CELL( FUNC, not));
    SET("if", CELL( FUNC, lisp_if));
    SET("set!", CELL( FUNC, set));
    SET("lambda", CELL( FUNC, lambda));
    SET("read-file-text", CELL( FUNC, lisp_read_file_text));
    SET("eval", CELL( FUNC, Eval));
    SET("while", CELL( FUNC, lisp_while));

    NIL = CELL(VAL, 0 );
    T = CELL(VAL,1);
    F = NIL;

    SET("t", T);
    SET("f", F);
    SET("nil", NIL);

    ast_t ast = Parse(expr);
    cell_base_t* cell = (cell_base_t*)ast;
    SET("ast", ast);

    print_cell((cell_base_t*)ast);

    while( NOT_NIL(cell) )
    {
	Eval( cell, env );
	cell = CDR(cell);
    }
    
    RELEASE( ast );

    for(int i = 0; i < sb_count( env->vals ); ++i)
    {
	RELEASE(env->vals[i]);
    }
    sb_free(env->keys); sb_free(env->vals);
    free(env);
}
