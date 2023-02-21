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
// Allow current struct to be reference counted
#define REFCOUNT ref_t _ref;
// Release memory
#define FREE(v) Free((cell_base_t*)v)
// Increase reference count - hold reference to a struct
#define RETAIN(_val) { ((ref_t*)_val)->_refCount += 1; }
// Decrease reference count - Free memory if no references left
#define RELEASE(_val) { if((((ref_t*)_val)->_refCount-= 1) <= 0) { FREE(_val); }}
#define __SET(XX,val,e) {sb_push(e->keys, XX); sb_push(e->vals, val); RETAIN(val);}
// Set a value to a given name-hash (prefer SET)
#define _SET(XX,val)  __SET(XX,val,env)
// Set a value to a given name
#define SET(name,val) _SET(hash(name), val) 
// Get a named value
#define GET(name) Get(env, hash(name))
// Replace a named value with a new one
#define REPLACE(name, val) Replace(env, hash(name), val);
// Create a new cell of a given type with specific value
#define CELL(t, v) _cell(t, (void*)v)
// Create a new list
#define LIST() (list_t*)CELL(LIST,0)
// Create a new Lisp function
#define FUNL() (fn_t*)CELL(FUNL,0)
// Quote the following cell
#define QUOTE() (list_t*)CELL(QUOT,0)
// Push a value onto a list
#define PUSH_BACK(list, val)						\
    {									\
	if( list->car == NULL ) { list->car = val; }			\
	else if( list->cdr == NULL )					\
	{								\
	    list->cdr = val; list->car->next = val;			\
	}								\
	else								\
	{								\
	    cell_base_t* pCell = list->cdr;				\
	    while( pCell->next != NULL ) pCell = pCell->next;		\
	    pCell->next = val;						\
	}								\
    }
// Pop a value off the list
#define POP Pop

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
    SYM, // string value
    LIST,// list
    VAL, // integer value
    BOOL,// boolean value
    NUM, // float value
    FUNL,// Lisp fn
    FUNC,// C fn
    QUOT,// '
} cell_type;

// base type for all cells - handles list traversal
// and type lookup
typedef struct _cell_base_t
{
    REFCOUNT;
    struct _cell_base_t* next;
    cell_type t;
} cell_base_t;

// Basic list type, contains references to the head, and rest of the list
typedef struct _list_t
{
    cell_base_t _base;
    cell_base_t* car;
    cell_base_t* cdr;
} list_t, *ast_t;

// Lisp function, params used for renaming parameters, body contains executablt
typedef struct _fn_t
{
    cell_base_t _base;
    list_t*      params;
    list_t*      body;
} fn_t;

// C function pointer
struct _env_t;
typedef cell_base_t* (*func)(cell_base_t*, cell_base_t*, struct _env_t* env);

// Basic cell type. Contains support for string, integer and C function types
typedef struct _cell_t
{
    cell_base_t _base;
    union { sym_t sym; int val; func func; };
} cell_t;

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

//----------------------------------------------------------------------------//
// Code starts here!                                                          //
//----------------------------------------------------------------------------//

// Create a new cell
cell_base_t* _cell(cell_type t, void* val)
{
    cell_base_t* cell;
    if(t == LIST)
    {
	cell = malloc(sizeof(list_t));
    }
    else if(t == FUNL)
    {
	cell = malloc(sizeof(fn_t));
    }
    else
    {
	cell = malloc(sizeof(cell_t));
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
    return NULL;
}

// Pop a value off the list
cell_base_t* Pop(list_t* list)
{
    cell_base_t* c = list->car;
    list->car = c ? c->next : NULL;
    list->cdr = list->car ? list->car->next : NULL;
    return c;
}

// Parse an expression into a list, returns a pointer to the end
// of the expression
const char* ParseList(list_t* list, const char* expr)
{
    const char* pTokStart = expr;
    const char* pTokEnd = expr + 1;
    while(1)
    {
	switch(*pTokStart)
	{
	    // early out - the end
	case '\0':
	    return pTokStart;
	    // quote the following cell/list
	case '\'':
	{
	    cell_base_t* cell = CELL(QUOT, 0);
	    PUSH_BACK(list,cell);
	    pTokEnd = ParseList((list_t*)cell, pTokEnd);
	}
	break;
	// start a new list
	case '(':
	{
	    cell_base_t* cell = CELL(LIST, 0);
	    PUSH_BACK(list,cell);
	    pTokEnd = ParseList((list_t*)cell, pTokEnd);
	}
	break;
	// end the current list
	case ')':
	    return pTokEnd;
	    // do nothing, unless the previous cell was quoted
	case ' ':
	    if(list->_base.t == QUOT)
	    {
		return pTokEnd; // if we're quoting, just break on the next space
	    }
	    break;
	    // everything else, we convert to a symbol
	default:
	{
	    while(*pTokEnd != '\0' && *pTokEnd !=  ' ' &&
		  *pTokEnd != '\n' && *pTokEnd != '\t' &&
		  *pTokEnd != '('  && *pTokEnd !=  ')' )
		++pTokEnd;

	    int len = pTokEnd - pTokStart;
	    char* sym = malloc(len + 1);
	    memcpy(sym, pTokStart, len);
	    sym[len] = 0;
	    PUSH_BACK(list, CELL(SYM, sym));
	}
	break;
	}

	// next token
	pTokStart = pTokEnd;
	pTokEnd = pTokStart+1;
    }
    return pTokStart;
}

// Parse an expression into a syntax tree.
ast_t Parse(const char* expr)
{
    ast_t ast = LIST();
    ParseList(ast, expr);
    return ast;
}

// Evaluate a list and return the result
cell_base_t* Eval(cell_base_t* car, cell_base_t* cdr, env_t env)
{
    // if the value is quoted, don't evaluate it
    if(car->t == QUOT)
    {
	return ((list_t*)car)->car;
    }

    // evaluate until we have no lists left
    while(car->t == LIST)
    {
	cell_base_t* res = Eval(((list_t*)car)->car, ((list_t*)car)->cdr, env);
	car = res;
    }

    // If we find a symbol, look it up and replace with the relevant value
    if(car->t == SYM)
    {
	cell_base_t* val = GET(((cell_t*)car)->sym);
	if( val != NULL )
	{
	    // Call the C function directly
	    if( val->t == FUNC )
	    {
		return ((cell_t*)val)->func(cdr, cdr->next, env);
	    }
	    // Evaluate a Lisp function
	    else if( val->t == FUNL )
	    {
		env_t scope = malloc(8);
		memset(scope, 0, 8);
		scope->_parent = env;
		cell_base_t* name = ((fn_t*)val)->params->car;
		cell_base_t* var = cdr ? cdr : NIL;
		while(name && var)
		{
		    __SET(hash(((cell_t*)name)->sym), Eval(var, var->next, env), scope);
		    name = name->next;
		    var = var->next;
		}
		val = Eval(((cell_base_t*)((fn_t*)val)->body), ((fn_t*)val)->body->car->next, scope);
		
		free(scope);
	    }
	    return val;
	}
    }
    return car;
}

// Cleanup memory from an unused cell
void Free(cell_base_t* cell)
{
    switch(cell->t)
    {
    case SYM:
	free(((cell_t*)cell)->sym);
	break;
    case LIST:
    case QUOT:
    {
	cell_base_t* car = ((list_t*)cell)->car;
	while(car)
	{
	    cell_base_t* next = car->next;
	    RELEASE( car );
	    car = next;
	}
    }
    break;
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
// Print list with newline
cell_base_t* println(cell_base_t* car, cell_base_t* cdr, env_t env)
{
    while( car )
    {
	cell_base_t* res = Eval(car, cdr, env);
	if(res->t == SYM) printf("%s\n", ((cell_t*)res)->sym);
	if(res->t == VAL) printf("%d\n", ((cell_t*)res)->val);
	car = car->next;
    }
    return NIL;
}

// arithemetic summation of all parameters
cell_base_t* plus(cell_base_t* car, cell_base_t* cdr, env_t env)
{
    int val = 0;
    cell_base_t* cell = car;
    while( cell )
    {
	cell_base_t* res = Eval(cell, cell->next, env);
	if(res->t == SYM)
	{
	    val += atoi(((cell_t*)res)->sym);
	}
	else if( res->t == VAL)
	{
	    val += ((cell_t*)res)->val;
	}
	cell = cell->next;
    }
    return CELL( VAL, val );
}

cell_base_t* sub(cell_base_t* car, cell_base_t* cdr, env_t env)
{
    int val = 0;
    int first = 1;
    cell_base_t* cell = car;
    while( cell )
    {
	cell_base_t* res = Eval(cell, cell->next, env);
	if(res->t == SYM)
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
	cell = cell->next;
    }
    return CELL( VAL, val );
}

// set named variable
cell_base_t* set(cell_base_t* car, cell_base_t* cdr, env_t env)
{
    cell_base_t* name = Eval(car, cdr, env);
    if( name != NULL && name->t == SYM)
    {
	cell_base_t* val = Eval(cdr, cdr->next, env);
	REPLACE(((cell_t*)name)->sym, val);
	return val;
    }
    return NIL;
}

cell_base_t* car( cell_base_t* car, cell_base_t* cdr, env_t env)
{
    return Eval(car, cdr, env);
}

cell_base_t* cdr( cell_base_t* car, cell_base_t* cdr, env_t env)
{
    return cdr;
}
// define a lisp function
cell_base_t* lambda(cell_base_t* car, cell_base_t* cdr, env_t env)
{
    {
	fn_t* val = FUNL();
	if(car->t == LIST)
	{
	    val->params = (list_t*)car;
	    val->body = (list_t*)cdr;
	    return &val->_base;
	}
    }
    return NIL;
}

cell_base_t* less(cell_base_t* car, cell_base_t* cdr, env_t env)
{
    cell_base_t* val1 = Eval(car, cdr, env);
    cell_base_t* val2 = Eval(cdr, cdr->next, env);

    int v1 = val1->t == SYM ? atoi(((cell_t*)val1)->sym) : ((cell_t*)val1)->val;
    int v2 = val2->t == SYM ? atoi(((cell_t*)val2)->sym) : ((cell_t*)val2)->val;
    
    return v1 < v2 ? T : F;
}

cell_base_t* lisp_if(cell_base_t* car, cell_base_t* cdr, env_t env)
{
    cell_base_t* test = Eval(car, cdr, env);
    if( test && test != NIL && test != F && ((cell_t*)test)->val != 0)
    {
	return Eval(cdr, cdr->next, env);
    }
    return Eval( cdr->next, cdr->next->next, env);
}

// main entry point
void lisp(const char* expr)
{
    env_t env = malloc(8);
    SET("println", CELL(FUNC, println));
    SET("+", CELL( FUNC, plus));
    SET("car", CELL( FUNC, car));
    SET("cdr", CELL( FUNC, cdr));
    SET("-", CELL( FUNC, sub));
    SET("<", CELL( FUNC, less));
    SET("if", CELL( FUNC, lisp_if));
    SET("set", CELL( FUNC, set));
    SET("lambda", CELL( FUNC, lambda));

    NIL = CELL(VAL, 0 );
    T = CELL(VAL,1);
    F = NIL;

    SET("t", T);
    SET("f", F);
    SET("nil", NIL);
    
    ast_t ast = Parse(expr);
    cell_base_t* cell = ast->car;
    while( cell )
    {
	Eval( cell, cell->next, env );
	cell = cell->next;
    }
    
    RELEASE( ast );

    for(int i = 0; i < sb_count( env->vals ); ++i)
    {
	RELEASE(env->vals[i]);
    }
    sb_free(env->keys); sb_free(env->vals);
    free(env);
}
