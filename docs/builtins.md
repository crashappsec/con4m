# Con4m builtin calls as of v0.3.4

[Go back to the home page.](https://github.com/crashappsec/con4m)

| Call | Con4m Type | Description |
| --- | --- | --- |
| Type Conversion Operations |  |  |
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
| String manipulation |  |  |
| contains | (string, string) → bool | Returns true if the first argument contains second argument anywhere. |
| find | (string, string) → int | If the first string contains the second as a substring, returns the starting byte index of the first match, starting from 0.  If the  substring is not found, this returns -1. |
| format | (string) → string | Makes substitutions within a string, based on variables that are in scope. For the input string, anything inside braces {} will be treated as a specifier. You can access attributes that are out of scope by fully dotting from the top-level name.  Note that all tags are currently part of the dotted name. You can use both attributes and variables in a specifier. Strings, bools, ints and floats are acceptable for specifiers, but lists and dictionaries are not. Note that there is currently no way to specify things like padding and alignment in a format specifier. If you want to insert an actual { or } character that shouldn't be part of a specifier, quote them by doubling them up (e.g., {{ to get a single left brace)|
| len | (string) → int | Returns the number of bytes in a string |
| slice | (string, int) → string | Returns a new string that is a substring starting at the provided byte index, through the end of the string.  As with languages like Python, negative values work, indexing from the back of the string. |
| slice | (string, int, int) → string | Returns a new string that is a substring starting at the first provided byte index, through the second provided index (not inclusive). As with languages like Python, negative values work, indexing from the back of the string. |
| split | (string, string) → [ string ] | Take the first string, and break it into a list containing all of the pieces that are separated by the second string, putting the results in a list.  |
| strip | (string) → string | Returns a second string, with any trailing or leading whitespace removed. |
| pad | (string, int) → string | Puts the specified number of spaces in front of each line of a string. |
| Container Basics (i.e., lists and dicts) |  |  |
| len | ([@T]) → int | Returns the number of items in a string. |
| len | ({@T : @V}) → int | Returns the number of items in a dictionary. |
| keys | ({@K : @V}) → [@K] | Returns a list of the keys in a dictionary. |
| values | ({@K : @V}) → [@K] | Returns a list of the values in a dictionary. |
| items | ({@K : @V}) → [(@K, @V)] | Returns a list of the key / value pairs in a dictionary. |
| File System |  |  |
| listDir | () → [string] | Returns a list of files in the current working directory. |
| listDir | (string) → [string] | Returns a list of files in the specified directory. If the directory is invalid, no error is given; the results will be the same as if the directory were empty. |
| readFile | (string) → string | Returns the contents of the file.  On error, this will return the empty string. |
| writeFile | (string, string) → bool | Writes, to the file name given in the first argument, the value of the string given in the second argument.  Returns true if successful, false otherwise. |
| copyFile | (string, string) → bool | Copies the contents of the file specified by the first argument to the file specified by the second, creating the new file if necessary, overwriting it otherwise.  Returns true if successful, false otherwise. |
| moveFile | (string, string) → bool | Moves the  file specified by the first argument to the location specified by the second, overwriting any file, if present.  Returns true if successful, false otherwise. |
| rmFile | (string) → bool | Removes the specified file, if allowed.  Returns true if successful. |
| joinPath | (string, string) → string | Combines two pieces of a path in a way where you don't have to worry about extra slashes. |
| resolvePath | (string) → string | Turns a possibly relative path into an absolute path. This also expands home directories. |
| splitPath | (string) -> (string, string) | Separates out the final path component from the rest of the path, i.e., typically used to split out the file name from the remainder of the path. |
| cwd | () → string | Returns the current working directory of the process. |
| chdir | (string) → bool | Changes the current working directory of the process.  Returns true if successful. |
| mkdir | (string) → bool | Creates a directory, and returns true on succes. |
| isDir | (string) → bool | Returns true if the given file name exists at the time of the call, and is a directory. |
| isFile | (string) → bool | Returns true if the given file name exists at the time of the call, and is a regular file. |
| isLink | (string) → bool | Returns true if the given file name exists at the time of the call, and is a link. |
| chmod | (string, int) → bool | Attempt to set the file permissions; returns true if successful. |
| filelen | (string) → int | Returns the number of bytes in the specified file, or -1 if there is an error (e.g., no file, or not readable) |
| System |  |  |
| echo | (string*) | Output a list of strings... to stderr, NOT stdout. A newline is added at the end, but no spaces are added between arguments. |
| abort | (string) | Stops the program immediately. |
| env | () → { string : string } | Returns a dictionary containing all environment variables and their contents. |
| env | (string) → string | Returns the value of a single environment variable.  If not set, the empty string is returned.  Note that it’s possible for environment variables to be set to an empty string, in which case use envContains() |
| envExists | (string) → bool | Returns true if the provided argument is a set environment variable, false otherwise. |
| setEnv | (string, string) → bool | Sets the environment variable specified in the first argument to the value specified in the second.  Returns true if successful. |
| getpid | () → int | Returns the current process ID |
| quote | (string) → string | Takes a string, and quotes it in a way that's acceptable for passing to shells. |
| run | (string) → string | Runs a specified command, as if on the command line, returning the output (This is the same to the system() call in many languages). |
| system | (string) → (string, int) | Like system, runs a specified command as if on the command line, but returns a tuple consisting of the output and the exit code. Only available on Posix systems. |
| getuid | () → int | Returns the current uid of the process. |
| geteuid | () → int | Returns the current euid of the process. Note, however, that, even if the process has euid 0, those permissions will be dropped before any call that runs a command or might modify the file system. |
