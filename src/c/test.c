#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "con4m.h"

char *configTest = "test section {\n  attr = \"hello, world!\"\nf = 12\n}";

int main(int argc, char *argv[], char *envp[]) {
  char *err;
  NimMain();

  char *res = c4mOneShot(configTest, "whatevs.c4m");
  printf("%s\n", res);
  c4mStrDelete(res);
  void *res2 = c4mFirstRun(configTest, "whatevs.c4m", 1, NULL, &err);
  if (!res2) {
      printf("%s", err);
      exit(0);
  }
  printf("res2 @%p\n", res2);
  assert(!c4mSetAttrInt(res2, "f", 14));
  AttrErr i = 0;
  printf("This should be 14: %ld\n", c4mGetAttrInt(res2, "f", &i));
  c4mSetAttrStr(res2, "foo", "bar");
  printf("foo = %s\n", c4mGetAttrStr(res2, "foo", &i));

  return 0;
}
