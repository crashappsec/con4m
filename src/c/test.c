#include <stdio.h>
#include <assert.h>
#include "con4m.h"

char *configTest = "test section {\n  attr = \"hello, world!\"\nf = 12\n}";

int main(int argc, char *argv[], char *envp[]) {
  NimMain();

  char *res = c4mOneShot(configTest, "whatevs.c4m");
  printf("%s\n", res);
  c4mStrDelete(res);
  void *res2 = c4mFirstRun(configTest, "whatevs.c4m", 1, NULL);
  printf("res2 @%p\n", res2);
  assert(!c4mSetAttrInt(res2, "f", 14));
  int64_t i = 0;
  printf("This should be 14: %ld\n", c4mGetAttrInt(res2, "f", &i));
  c4mSetAttrStr(res2, "foo", "bar");
  printf("foo = %s\n", c4mGetAttrStr(res2, "foo", &i));

  return 0;
}
