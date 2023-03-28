#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>

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
// display - output parameters followed by a new line                         //
// set - set a variable to something                                          //
// + - perform arithmetric summation between all parameters                   //
// lambda - define a function                                                 //
// ' - don't evaluate the following cell                                      //
//----------------------------------------------------------------------------//
// EXAMPLE                                                                    //
// (set 'testValue 1)                                                         //
// (set 'testFn lambda()                                                      //
//      (display (set 'testValue (+ testValue 1))))                           //
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
#define FREE(v) Free(&(v))
#define FREE_ENV(v) FreeEnv(&(v))
// Increase reference count - hold reference to a struct
#define RETAIN(_val) { ((ref_t*)_val)->_refCount += 1; }
// Decrease reference count - Free memory if no references left
#define RELEASE(_val) { if((((ref_t*)_val)->_refCount-= 1) <= 0) { FREE(_val); }}
#define RELEASE_ENV(_val) { if((((ref_t*)_val)->_refCount-= 1) <= 0) { FREE_ENV(_val); }}
#define __SET(XX,val,e) {cell_t* v = val; sb_push(e->keys, XX); sb_push(e->vals, v); RETAIN(v);}
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
#define LIST() CELL(LIST,0)
// Create a new Lisp function
#define FUNL() CELL(FUNL,0)
// Quote the following cell
#define QUOTE() CELL(QUOT,0)
// Push a value onto a list
#define PUSH_BACK(list, val)						\
  {									\
      cell_t* l = list;							\
      while(NOT_NIL(CAR(l)))						\
      {									\
	  if(IS_NIL(CDR(l))) l->pair.cdr = LIST();			\
	  l = CDR(l);							\
      }									\
      l->pair.car = val;						\
  }

#define NOT_NIL(x) ((x != NULL) && ((x) != NIL))
#define IS_NIL(x) (!NOT_NIL(x))

#define CAR(x)   ((NOT_NIL(x) && (x)->t == LIST) ? ((x)->pair.car) : NIL)
#define CDR(x)   ((NOT_NIL(x) && (x)->t == LIST) ? ((x)->pair.cdr) : NIL)
#define CADR(x ) (CAR(CDR(x)))
#define CADDR(x) (CAR(CDR(CDR(x))))
#define CDDR(x)  (CDR(CDR(x)))

//----------------------------------------------------------------------------//
// Types                                                                      //
//----------------------------------------------------------------------------//
// Symbols in this lisp are just a standard C null terminated string
typedef char *sym_t;

#if _WIN64 || __x86_64__
typedef long long number_t;
#define ATON(x) atoll(x)
#else
typedef int number_t;
#define ATON(x) atoi(x)
#endif

// Simple reference counting setup, use REFCOUNT to use
typedef struct
{
    signed int _refCount;
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

    MAX_TYPE,
} cell_type;

// C function pointer
struct _env_t;
struct cell_t;
typedef struct _cell_t* (*func)(struct _cell_t*, struct _env_t* env);

typedef struct _pair_t
{
    struct _cell_t* car;
    struct _cell_t* cdr;
} pair_t;

// base type for all cells - handles type lookup
typedef struct _cell_t
{
    REFCOUNT;
    cell_type t;
    union
    {
	sym_t sym;
	int val;
	func func;
	struct _cell_t* inner;
	pair_t pair;
    };
    struct _env_t* env; // do not like having this here
} cell_t, cell_t, *ast_t;


void Free(cell_t** cell);
void FreeEnv(struct _env_t** env);

// Environment/scope, a simple hashmap lookup, with ability to have children
typedef struct _env_t
{
    REFCOUNT;
    int*           keys;
    cell_t**  vals;
    struct _env_t* _parent;
} *env_t;

//----------------------------------------------------------------------------//
// Static types                                                               //
//----------------------------------------------------------------------------//
cell_t* NIL;
cell_t* T;
cell_t* F;
cell_t* COMMENT;

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
sym_t read_file_text(const sym_t fname)
{
  FILE* fp = fopen(fname, "r");
  fseek(fp, 0L, SEEK_END);
  long size = ftell(fp);
  fseek(fp, 0L, SEEK_SET);
  sym_t buffer=malloc(size + 1);
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

int ___i = 0;
int ___t = 0;
int ___r = 0;
// Create a new cell
cell_t* _cell(cell_type t, void* val)
{
    ++___i;
    ++___t;
    cell_t* cell = malloc(sizeof(cell_t));
    memset(cell, 0, sizeof(cell_t));
    cell->t = t;
    
    if(t == LIST || t == FUNL)
    {
	cell->pair.car = NOT_NIL(val) ? val : NIL;
	cell->pair.cdr = NIL;
    }
    else if(t == QUOT)
    {
      cell->inner = NOT_NIL(val) ? val : NIL;
    }
    else if(t == VAL)
    {
	cell->val = (number_t)val;
    }
    else if(t == STRING || t == SYM)
    {
	int len = strlen((char*)val)+1;
	cell->sym = malloc(len);
	memcpy(cell->sym, val, len);
    }
    else
    {
	cell->sym = val;
    }
    return cell;
}

// Retrieve a hashed, named cell from the current scope
cell_t* Get(env_t env, int hash)
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
const char* ParseList(cell_t* list, const char* expr);
const char* ParseToken(cell_t** cell, const char* expr)
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
	*cell = QUOTE();
	return ParseToken(&(*cell)->inner, pTokEnd);
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
	free(sym);
	if (lastChar == '"') pTokEnd++;
	return pTokEnd;
    }
    case '(':
	*cell = LIST();
	return ParseList(*cell, pTokEnd);
    case ')':
	*cell = NIL;
	return pTokEnd;
    case ';':
      // scan to the end of the line
      while(*pTokEnd != '\n' && *pTokEnd !=0) ++pTokEnd;
      *cell = COMMENT;
      return pTokEnd;
    case '#':
    {
	cell_t* datum = NIL;
	pTokEnd = ParseToken(&datum, pTokEnd);
	RELEASE(datum); // this one has been commented out
	*cell = COMMENT;
	return pTokEnd;
    }
    }

    if( (*pTokStart >= '0' && *pTokStart <= '9') || *pTokStart == '-' )
    {
      // TODO: Proper number parsing - allow different bases etc
	while((*pTokEnd >= '0' && *pTokEnd <= '9') || *pTokEnd == '.')
	    ++pTokEnd;
	int len = pTokEnd - pTokStart;
	char* sym = malloc(len + 1);
	memcpy(sym, pTokStart, len);
	sym[len] = 0;
	*cell = CELL(VAL, ATON(sym));
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
    free(sym);
    return pTokEnd;
}

const char* ParseList(cell_t* list, const char* expr)
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
	cell_t* cell = NIL;
	pTokEnd = ParseToken(&cell, pTokStart);

	if(cell != COMMENT)
	{
	    RETAIN(cell);
	    PUSH_BACK(list, cell);
	}

	
	// next token
	pTokStart = pTokEnd;
	if( *pTokEnd == '\0' || *pTokStart == '\0' || cell == NIL ) break;
    }
    return pTokEnd;
}

// Parse an expression into a syntax tree.
ast_t Parse(const char* expr)
{
    ast_t ast = LIST();
    ParseList(ast, expr);
    return ast;
}

// Evaluate a list and return the result
cell_t* Eval(cell_t* cell, env_t env)
{
    // if the value is quoted, don't evaluate it
    if(cell->t == QUOT)
    {
      return cell->inner;
    }

    if(cell->t == LIST)
    {
	cell_t* fn = Eval(CAR(cell), env);
	switch(fn->t)
	{
	case FUNC:
	{
	    cell = fn->func(CDR(cell), env);
	    return cell;
	}
	case FUNL:
	{
	    cell_t* params = CDR(cell);

	    if(params->t == LIST)
	    {
	        cell_t* name = fn->pair.car;
		cell_t* val = params;

		env_t scope = ENV();
		RETAIN(scope);
		scope->_parent = fn->env;;
		while(NOT_NIL(name) && NOT_NIL(CAR(name)) && NOT_NIL(val) && NOT_NIL(CAR(val)))
		{
		    __SET(hash(((cell_t*)CAR(name))->sym),
			  Eval(CAR(val), env),
			  scope);
		    name = CDR(name);
		    val = CDR(val);
		}

		cell_t* res = NIL;
		fn = fn->pair.cdr;
		while(NOT_NIL(CAR(fn)))
		{
		    res = Eval(CAR(fn), scope);
		    fn = CDR(fn);
		}
		
		RELEASE_ENV(scope);
		return res;
	    }
	}
	break;
	default:
	    break;
	}
	return fn;
    }

    // If we find a symbol, look it up and replace with the relevant value
    if(cell->t == SYM)
    {
	return GET(((cell_t*)cell)->sym);
    }
    return cell;
}

cell_t* lisp_parse(cell_t* cell, env_t env)
{
    cell = Eval(cell, env);
    RETAIN(cell);
    ast_t ast = LIST();
    if(cell->t == STRING)
    {
	ParseList(ast, cell->sym);
    }
    RELEASE(cell);
    return ast;
}

// Cleanup memory from an unused cell
void Free(cell_t** cell)
{
    if(*cell == NIL || *cell == T || *cell == F || (*cell)->t > MAX_TYPE) return; // don't free these
    switch((*cell)->t)
    {
    case STRING:
    case SYM:
	if((*cell)->sym)
	{
	    free((*cell)->sym);
	    (*cell)->sym = NULL;
	}
	break;
    case LIST:
    {
	if(NOT_NIL((*cell)->pair.car)) RELEASE((*cell)->pair.car);
	if(NOT_NIL((*cell)->pair.cdr)) RELEASE((*cell)->pair.cdr);
	break;
    }
    break;
    case QUOT:
	RELEASE((*cell)->inner);
	break;
    case FUNL:
	RELEASE_ENV((*cell)->env);
      break;
    case VAL:
    case BOOL:
    case NUM:
    case FUNC:
    case MAX_TYPE:
      break; // these don't have any additional memory allocated
    }
    ___i--;
    ++___r;

    (*cell)->t = MAX_TYPE;
    free(*cell);
    *cell = NIL;
}

void FreeEnv(env_t* env)
{
    for(int i = 0; i < sb_count( (*env)->vals ); ++i)
    {
	RELEASE((*env)->vals[i]);
    }
    sb_free((*env)->keys); sb_free((*env)->vals);
    free(*env);
    *env = NULL;
}

// Replace a value in memory if the name already exists
cell_t* Replace(env_t env, int hash, cell_t* val)
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
cell_t* str(cell_t* cell, env_t env)
{
    cell_t* val = Eval(cell, env);
    char* buff = NULL;
    switch(val->t)
    {
    case SYM:
	return CELL(STRING, val->sym);
    case STRING:
	return val;
    case VAL:
    {
	buff = malloc(128);
	sprintf(buff, "%d", val->val);
	cell_t* res = CELL(STRING, buff);
	free(buff);
	return res;
    }
    case LIST:
    {
	buff = sb_add(buff, 2);
	sprintf(buff, "(");
	int p = 1;
	while(NOT_NIL(val) && NOT_NIL(CAR(val)))
	{
	    cell_t* strcar = str(CAR(val), env);
	    int l = strlen(strcar->sym);
	    char* pbuff = sb_add(buff, l+1);
	    sprintf(pbuff-1, "%s ", strcar->sym);
	    p += l+1;
//	    RELEASE(strcar);
	    val = CDR(val);
	}

	if (NOT_NIL(val))
	{
	    if(val->t != LIST || (NOT_NIL(CAR(val)) && NOT_NIL(CDR(val))))
	    {
	      cell_t* strcar = str(val, env);
	      int l = strlen(strcar->sym);
	      char* pbuff = sb_add(buff, l+3);
	      sprintf(pbuff-1, ". %s ", strcar->sym);
	      p += l+3;
	    }
	}

	char* pbuff = sb_add(buff, p+2);
	pbuff[-2] = ')';
	pbuff[-1] = '\0';

	char* strbuf = malloc(p+2);
	memcpy(strbuf, buff, p+2);
	cell_t* res = CELL(STRING, strbuf);
	sb_free(buff);
	free(strbuf);
	return res;
    }
    case BOOL:
    case NUM:
    case FUNL:
    case FUNC:
    case QUOT:
    case MAX_TYPE:
	break;
    };
    return NIL;
}

// Print list with newline
cell_t* display(cell_t* cell, env_t env)
{
    cell_t* res = str(cell, env);
    printf("%s", res->sym);
    return NIL;
}

// arithemetic summation of all parameters
cell_t* plus(cell_t* cell, env_t env)
{
    number_t val = 0;
    while( NOT_NIL(cell) && NOT_NIL(CAR(cell)) )
    {
	cell_t* res = Eval(CAR(cell), env);
	if(res->t == SYM || res->t == STRING)
	{
	    val += ATON(res->sym);
	}
	else if( res->t == VAL)
	{
	    val += res->val;
	}
	cell = CDR(cell);
    }
    return CELL( VAL, val );
}

cell_t* sub(cell_t* cell, env_t env)
{
    number_t val = 0;
    int first = 1;
    while( NOT_NIL(cell) && NOT_NIL(CAR(cell)) )
    {
	cell_t* res = Eval(CAR(cell), env);
	if(res->t == SYM || res->t == STRING)
	{
	    val-= ATON(res->sym);
	}
	else if( res->t == VAL)
	{
	    val -= res->val;
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
cell_t* set(cell_t* cell, env_t env)
{
    cell_t* name = Eval(CAR(cell), env);
    if( NOT_NIL(name) && name->t == SYM)
    {
	cell_t* val = Eval(CADR(cell), env);

	int namehash = hash(name->sym);
	while(env != NULL)
	{
	    for(int i = 0; i < sb_count(env->keys); ++i)
	    {
		if(env->keys[i] == namehash)
		{
		    RETAIN(val);
		    RELEASE(env->vals[i]);
		    env->vals[i] = val;
		    return NIL;
		}
		   
	    }
	    env = env->_parent;
	}
    }
    return NIL;
}

cell_t* define(cell_t* cell, env_t env)
{
    cell_t* name = Eval(CAR(cell), env);
    if( NOT_NIL(name) && name->t == SYM)
    {
	cell_t* val = Eval(CADR(cell), env);
	RETAIN(val);
	REPLACE(name->sym, val);
    }
    return NIL;
}

cell_t* concat(cell_t* cell, env_t env)
{
    char* val = NULL;
    val = sb_add(val,1);
    val[0] = 0;
    while( NOT_NIL(cell) && NOT_NIL(CAR(cell)) )
    {
	cell_t* res = str(CAR(cell), env);
	int len = strlen(res->sym)+1;
	char* pVal = sb_add(val, len);
	if(pVal == NULL) break;
	// using sprintf to repeatedly concatenate
	// because it means I don't have to worry about null terminators
	sprintf(val, "%s%s", val, res->sym);
	cell = CDR(cell);
    }
    cell = CELL(STRING, val);
    sb_free(val);
    return cell;   
}

cell_t* cons(cell_t* cell, env_t env)
{
    cell_t* lst = LIST();
    lst->pair.car = Eval(CAR(cell), env);
    lst->pair.cdr = Eval(CDR(cell), env);
    return (cell_t*)lst;
}
  

cell_t* car( cell_t* cell, env_t env)
{
    cell = Eval(cell, env);
    return CAR(cell);
}

cell_t* cdr( cell_t* cell, env_t env)
{
    cell = Eval(cell, env);
    cell = CDR(cell);
    if(cell->t == LIST &&
       IS_NIL(CAR(cell)) &&
       IS_NIL(CDR(cell)))
      // If we're at the end of a list, this is
      // provided by (NIL . NIL) but we don't want
      // CDR to return this, so instead just return NIL
      return NIL;
    return cell;
}

// define a lisp function
cell_t* lambda(cell_t* cell, env_t env)
{
    {
	cell_t* val = FUNL();
	if(cell->t == LIST)
	{
	    cell_t* list = cell;
	    val->pair.car = CAR(list);
	    val->pair.cdr = CDR(list);
	    RETAIN(env);
	    val->env = env;
	    return val;
	}
    }
    return NIL;
}

cell_t* less(cell_t* cell, env_t env)
{
    cell_t* val1 = Eval(CAR(cell), env);
    cell_t* val2 = Eval(CADR(cell), env);

    int v1 = val1->t == SYM || val1->t == STRING ? atoi(((cell_t*)val1)->sym) : ((cell_t*)val1)->val;
    int v2 = val2->t == SYM || val2->t == STRING ? atoi(((cell_t*)val2)->sym) : ((cell_t*)val2)->val;
    
    return v1 < v2 ? T : F;
}

cell_t* not(cell_t* cell, env_t env)
{
    cell_t* v = Eval(cell, env);

    return (v == F || v == NIL) ? T : F;
}

cell_t* lisp_if(cell_t* cell, env_t env)
{    
    cell_t* test = Eval(CAR(cell), env);
    if( test && test != NIL && test != F && ((cell_t*)test)->val != 0)
    {
	return Eval(CADR(cell), env);
    }
    return Eval(CADDR(cell), env);
}

cell_t* lisp_read_file_text(cell_t* cell, env_t env)
{
    cell_t* nameval = Eval(cell, env);
    char* namestr = ((cell_t*)nameval)->sym;

    char* contentstr = read_file_text(namestr);

    // do a bit of juggling here - we want the cell to own the string
    // but we don't want to have to copy it, so instead, initialise with
    // empty string, then re-free it and swap our content in
    cell_t* res = CELL(STRING, "");
    free(res->sym);
    res->sym = contentstr;
    return res;
}

cell_t* lisp_while(cell_t* cell, env_t env)
{
    cell_t* test = Eval(CAR(cell), env);
    cell_t* ret = NIL;
    
    while( NOT_NIL(test) && test != F && ((cell_t*)test)->val != 0)
    {
        ret = Eval(CDR(cell), env);
	test = Eval(CAR(cell), env);
    }
    return ret;
}

void print_cell(cell_t* cell)
{
    static int depth = 0;

    for(int i = 0; i < depth; ++i) printf(" ");

    if(IS_NIL(cell))
    {
	printf("NIL\n");
	return;
    }
    
    switch(cell->t)
    {
    case SYM:
	printf("S:%s\n", cell->sym);
	break;
    case LIST:
	printf("L:\n");
	++depth;
	print_cell(CAR(cell));
	print_cell(CDR(cell));
	--depth;
	break;
    case VAL:
	printf("V:%d\n", cell->val);
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
	printf("F:%p\n", cell->func);
	break;
    case QUOT:
	printf("Q:\n");
	++depth;
	print_cell(cell->inner);
	--depth;
	break;
    case STRING:
	printf("S:\"%s\"\n", cell->sym);
	break;
    default:
      printf("UNKNOWN\n");
      break;
    }
}

cell_t* lisp_print_cell(cell_t* cell, env_t env)
{
    cell = Eval(cell, env);
    print_cell(cell);
    return NIL;
}

cell_t* lisp_eval(cell_t* cell, env_t env)
{
    // the first eval is to prepare the param
    cell_t* list = Eval(cell, env);
    cell = list;
    RETAIN(list);
    PUSH_BACK(GET("__e"), list)
    // then actually eval what requested
    cell_t* res = NIL;
    while( NOT_NIL(cell) )
    {
	res = Eval( cell, env );
	cell = CDR(cell);
    }
    return res;
}

cell_t* global(cell_t* cell, env_t env)
{
    cell_t* res = NIL;
    while( NOT_NIL(cell) )
    {
	res = Eval( cell, env );
	cell = CDR(cell);
    }

    env_t g = env;
    while(g->_parent != NULL) g = g->_parent;

    for(int i = 0; i < sb_count(env->keys); ++i)
    {
	Replace(g, env->keys[i], env->vals[i]);
	// This env has been brought into the global scope so the entries in here should be dropped - this is
	// in case something in a child scope accesses them as well as something in the global scope - if they
	// get overwritten in the global scope, the change won't be detected by the child scope
	env->keys[i] = 0;
	env->vals[i] = NIL;
    }
    
    return res;
}

cell_t* cond(cell_t* cell, env_t env)
{
    while(NOT_NIL(cell))
    {
	cell_t* test = Eval(CAR(CAR(cell)), env);
	if(test == T)
	{
	    return Eval(CADR(CAR(cell)), env);
	}
	cell = CDR(cell);
    }
    return NIL;
}

cell_t* let(cell_t* cell, env_t env)
{
    env_t scope = ENV();
    scope->_parent = env;
    RETAIN(scope);
    cell_t* values = CAR(cell);
    while(NOT_NIL(values))
    {
	cell_t* name = CAR(CAR(values));
	cell_t* value = Eval(CDR(CAR(values)), env);
//	RETAIN(value);
	if(NOT_NIL(name))
	{
	    __SET(hash(name->sym), value, scope);
	}
	
	values = CDR(values);
    }
    cell_t* res = NIL;
    cell = CDR(cell);
    while(NOT_NIL(cell))
    {
	res = Eval(cell, scope);
	cell = CDR(cell);
    }
    RELEASE_ENV(scope);

    return res;
}

cell_t* string_equals(cell_t* cell, env_t env)
{
  cell_t* test = Eval(CAR(cell), env);
  if(test->t != SYM && test->t != STRING)
    return NIL;
  cell = CDR(cell);
  while(NOT_NIL(cell))
  {
    if(CAR(cell) == NIL) break;
    
    cell_t* val = Eval(CAR(cell), env);
    if(val->t != SYM && val->t != STRING)
      return NIL;

    if(val == NIL) return NIL;

    if(strcmp(val->sym, test->sym) != 0) return NIL;

    cell = CDR(cell);
  }
  return T;
}

cell_t* substr(cell_t* cell, env_t env)
{
  cell_t* string = str(CAR(cell), env);
  cell_t* start = Eval(CADR(cell), env);
  cell_t* end = Eval(CADDR(cell), env);

  if(string->t != SYM && string->t != STRING) return NIL;
  if(start->t != VAL) return NIL;
  if(end->t != VAL) return NIL;

  int cend = strlen(string->sym);
  if(end != NIL)
  {
    if(end->val < 0) cend += end->val;
    else cend = end->val;
  }
  int size = cend - start->val;
  char* dest = malloc(size+1);

  strncpy(dest, string->sym + start->val, size);
  dest[size] = 0;
  
  cell = CELL(STRING, dest);
  free(dest);
  return cell;
}

#define DEFINE_PREDICATE(name, type)					\
  cell_t* name##_predicate(cell_t *cell, env_t env)		\
  {									\
    cell = Eval(cell, env);						\
    return cell->t == type ? T : F;					\
  }
#define DECLARE_PREDICATE(name)			\
  SET(#name"?", CELL(FUNC, name##_predicate));
#define DECLARE_FUNC(sym, name)			\
  SET(sym, CELL(FUNC, name))

DEFINE_PREDICATE(list, LIST);
DEFINE_PREDICATE(symbol, SYM);
DEFINE_PREDICATE(string, STRING);
DEFINE_PREDICATE(value, VAL);


cell_t* is_nil(cell_t* cell, env_t env)
{
    return NOT_NIL(Eval(cell, env)) ? F : T;
}

cell_t* file_exists_p(cell_t* cell, env_t env)
{
    cell = str(cell, env);
    struct stat st;
    int s = stat(cell->sym, &st);
    return s == 0 ? T : F;
}

cell_t* newline(cell_t* cell, env_t env)
{
  UNUSED(cell);
  UNUSED(env);
  printf("\n");
  return NIL;
}

// main entry point
int main(int argc, char** argv)
{
  env_t env = ENV();
    DECLARE_FUNC("print-cell", lisp_print_cell);
    DECLARE_FUNC("display", display);
    DECLARE_FUNC("newline", newline);
    DECLARE_FUNC("+", plus);
    DECLARE_FUNC("cons", cons);
    DECLARE_FUNC("car", car);
    DECLARE_FUNC("cdr", cdr);
    DECLARE_FUNC("-", sub);
    DECLARE_FUNC("<", less);
    DECLARE_FUNC("not", not);
    DECLARE_FUNC("if", lisp_if);
    DECLARE_FUNC("define", define);
    DECLARE_FUNC("set!", set);
    DECLARE_FUNC("lambda", lambda);
    DECLARE_FUNC("read-file-text", lisp_read_file_text);
    DECLARE_FUNC("eval", lisp_eval);
    DECLARE_FUNC("while", lisp_while);
    DECLARE_FUNC("parse", lisp_parse);
    DECLARE_FUNC("concat", concat);
    DECLARE_FUNC("cond", cond);
    DECLARE_FUNC("let", let);
    DECLARE_FUNC("string=?", string_equals);
    DECLARE_FUNC("substr", substr);
    DECLARE_FUNC("nil?", is_nil);
    DECLARE_FUNC("file-exists?", file_exists_p);
    DECLARE_FUNC("global", global);
    DECLARE_PREDICATE(list);
    DECLARE_PREDICATE(symbol);
    DECLARE_PREDICATE(string);
    DECLARE_PREDICATE(value);

    cell_t* args = LIST();
    for(int i = 0; i < argc; ++i)
    {
	cell_t* arg = CELL(STRING, argv[i]);
	PUSH_BACK(args, arg);
    }
    SET("args", args);
    SET("__e", LIST())
    
    
    NIL = CELL(VAL, 0 );
    T = CELL(VAL,1);
    F = NIL;
    COMMENT = CELL(SYM, ";;; COMMENT ;;;");
    
    SET("t", T);
    SET("f", F);
    SET("nil", NIL);

    ast_t ast = Parse("(eval (parse (read-file-text \"stage0.yl\")))");
    cell_t* cell = (cell_t*)ast;

    // set the ast to be available in lisp in case we want it
    SET("ast", ast);

    while( NOT_NIL(cell) )
    {
	Eval( cell, env );
	cell = CDR(cell);
    }
    
    Free(&ast);
    Free(&COMMENT);
    FreeEnv(&env);

    printf("Number of leaked cells:%d/%d (%luB)\n", ___i, ___t, ___i*sizeof(cell_t));
}
