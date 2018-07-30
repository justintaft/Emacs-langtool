# This is a test

Normal text should be checked for grammar errors errors.

```
// langtool will not highlight grammar errors in markdown code by default
// it's configurable by adding text type-faces to
// langtool-ignore-grammar-errors-in-type-faces

#include <stdio.h>
int main(int argc, char **argv) {
	printf("Hello world.");
	return 0;
}
```

Inline code `won't be checked for grammar errors errors` either.


And sensible default grammar rule checks are used.
