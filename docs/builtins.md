# Con4m builtin calls as of v0.2.1

[Go back to the home page.](https://github.com/crashappsec/con4m)

| Call | Con4m Type | Description |
| --- | --- | --- |
| Conversion Operations |  |  |
| bool | (int) → bool | Convert an integer to a true/false value. |
| bool | (float) → bool | Convert a float to a true/false value. |
| bool | (string) → bool | Convert a string to a true/false value. |
| bool | ([@T]) → bool | Convert a list of any type to a true/false value. |
| bool | ({@T : @V}) → bool | Convert a dictionary of any type to a true/false value. |
| float | (int) → float | Convert an integer to a floating point representation. |
| int | (float) → int | Convert a float to an integer by truncating the decimal part. |
| string | (bool) → string | Convert a boolean value to the string “true” or “false”. |
| string | (int) → string | convert an integer into a string. |
| string | (float) → string | Convert a float into a string. |
| String and data structure manipulation |  |  |
| contains | (string, string) → bool | Returns true if the first argument contains second argument anywhere. |
| find | (string, string) → int | If the first string contains the second as a substring, returns the starting byte index of the first match, starting from 0.  If the  substring is not found, this returns -1. |
| format | (string) → string | Makes substitutions within a string, based on variables that are in scope. For the input string, anything inside braces {} will be treated as a specifier. You can access attributes that are out of scope by fully dotting from the top-level name.  Note that all tags are currently part of the dotted name. You can use both attributes and variables in a specifier. Strings, bools, ints and floats are acceptable for specifiers, but lists and dictionaries are not. Note that there is currently no way to specify things like padding and alignment in a format specifier. If you want to insert an actual { or } character that shouldn't be part of a specifier, quote them by doubling them up (e.g., {{ to get a single left brace)|
| len | (string) → int | Returns the number of bytes in a string |
| len | ([@T]) → int | Returns the number of items in a string. |
| len | ({@T : @V}) → int | Returns the number of items in a dictionary. |
| slice | (string, int) → string | Returns a new string that is a substring starting at the provided byte index, through the end of the string.  As with languages like Python, negative values work, indexing from the back of the string. |
| slice | (string, int, int) → string | Returns a new string that is a substring starting at the first provided byte index, through the second provided index (not inclusive). As with languages like Python, negative values work, indexing from the back of the string. |
| split | (string, string) → [ string ] | Take the first string, and break it into a list containing all of the pieces that are separated by the second string, putting the results in a list. If the  |
| strip | (string) → string | Returns a second string, with any trailing or leading whitespace removed. |
| System |  |  |
| abort | (string) | Stops the program immediately. |
| env | () → { string : string } | Returns a dictionary containing all environment variables and their contents. |
| env | (string) → string | Returns the value of a single environment variable.  If not set, the empty string is returned.  Note that it’s possible for environment variables to be set to an empty string, in which case use envContains() |
| envContains | (string) → bool | Returns true if the provided argument is a set environment variable, false otherwise. |
| run | (string) → string | Runs a specified command, as if on the command line, returning the output (This is the same to the system() call in many languages). |