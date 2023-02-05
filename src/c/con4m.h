#ifndef __CON4M_H__
#define __CON4M_H__

#include <stdint.h>

typedef void * C4State;
typedef void * C4Spec;
typedef void * NimDict;
typedef void * Box;

extern void     NimMain();
extern void     c4mStrDelete(char *);
extern char    *c4mOneShot(char *, char *);
extern C4State  c4mFirstRun(char *, char *, _Bool, C4Spec);
extern char    *c4mStack(C4State, char *, char *, C4Spec); // If err, decref.
extern int64_t  c4mSetAttrInt(C4State, char *, int64_t);
extern int64_t  c4mGetAttrInt(C4State, char *, int64_t *);
extern int64_t  c4mSetAttrStr(C4State, char *, char *);
extern char    *c4mGetAttrStr(C4State, char *, int64_t *);
extern int64_t  c4mSetAttrFloat(C4State, char *, float);
extern float    c4mGetAttrFloat(C4State, char *, int64_t *);
extern int64_t  c4mSetAttr(C4State, char *, Box); // Does NOT change ref counts.
extern Box      c4mGetAttr(C4State, char *); // Does incref the box it returns.
extern void     c4mClose(C4State);
extern int64_t  c4mUnpackInt(Box); // Decrefs the input
extern float    c4mUnpackFloat(Box); // Decrefs the input
extern char    *c4mUnpackString(Box); // Decrefs input; result needs decref
extern int64_t  c4mmUnpackArray(Box, Box *);  // Decrefs the 1st arg.
extern Box      c4mPackArray(Box *, int64_t); // Result needs to be decref'd.
extern NimDict  c4mUnpackDict(Box); // Input is decref'd, output incref'd
extern NimDict  c4mDictNew(); // Result needs to be decref'd.
extern void     c4mDictDelete(NimDict);
extern Box      c4mDictLookup(NimDict, Box); // Result needs to be decref'd.
extern void     c4mDictSet(NimDict, Box, Box);
extern C4Spec   c4mLoadSpec(char *, char *, int64_t *);
extern char *   c4mGetSpecErr(C4Spec); // Will decref the Spec object.
extern void     c4mSpecDelete(C4Spec);

#endif

