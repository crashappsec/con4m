# Writing config files with Con4m

Con4m is meant to be familiar to anyone who has ever messed with a configuration file. But, it also makes it easy to make configuration decisions at the time the program runs (and at some point, dynamically if desired).

You can set global configuration values, and add named sections that can be tagged, and have attributes.  For instance:

```bash
git_user: "john@crashoverride.com"
repository con4m {
  URI: "https://www.github.com/crashappsec/con4m.git"
  key_file: "~/.ssh/con4mKey"
}
repository sami {
  URI: "https://www.github.com/crashappsec/sami.git"
  key_file: "~/.ssh/samiKey"
}
```

In Con4m, attribute names are standard unicode identifiers.  The right hand side of an attribute is *not* free-form. Strings need to be put in quotes (and they do accept unicode escape sequences, etc).

The section name (here, just repository), is followed by zero or more tags. But this might depend on the context— most uses of Con4m will enforce a config file schema.

# Builtin function calls

One feature of Con4m is that can make function calls, for instance to run other shell commands, or output text:

```bash
text := run("ls -l")
echo(text)
```

This will output (on my machine):

```bash
total 712
drwx------@   3 viega  staff      96 Oct 29 20:02 Applications
drwx------+   8 viega  staff     256 Dec  5 18:48 Desktop
drwx------+  21 viega  staff     672 Nov  7 14:56 Documents
drwx------@ 537 viega  staff   17184 Dec  6 20:55 Downloads
...
```

Con4m provides several built-in calls, and will be adding more ([see the current list here](builtins.md)). Individual applications can add or remove them easily, though, so it’s good to check the documentation for any particular application.

You can also write and call your own functions in the config file, which is discussed below.

One  particularly useful call is `env()`.  It gives you the ability to override configuration decisions encoded in the file, without changing the file. But, the program doesn’t have to add support for environment variables you have to hunt around to find… you get to create the environment variables you need, and name them what you like, thanks to the `if` statement:

# If statement

```bash
default_compiler_flags: "-O2"
if env("CON4M_DEBUG") {
   default_compiler_flags: "-DDEBUG -O0" # replaces the other assignment
} 
elif env("CON4M_ADD_FPIC"): 
  # Note that format({}) implements Python-like formatters with the
  # local environment.  Selective access to runtime formatting is a
  # roadmap item.
  default_compiler_flags: format("{default_compiler_flags} -fPIC")
}
```

The Con4m if statement supports arbitrary elif branches, and a final else branch. Obviously, different branches may end up not setting the same attributes. Thats okay… the application gets to decide if a configuration is valid or not, and usually many options will have default values, or be truly optional.

It’s important to know that loop conditions need to evaluate to either `true` or `false`. You can use `or` and `and` (`||` and `&&` also work) as well, but again, each side must evaluate to true or false. Any other data type can be converted to true or false with a [builtin conversion call](builtins.md). This requirement might be annoying to people who aren’t used to it, but results in more robust code in our experience.

# Attributes vs. Variables

You might have noticed, in the first example, we set config file keys with a colon.  And, we could also have used an equals sign.  People just slapping together simple config files shouldn’t have to work too hard on syntax to get it right, and both are familiar, so both should work.

However, as mentioned above, one of the advantages of Con4m from an application writer’s point of view is that they can write a simple schema that will still sanity check the application, making sure that the right attributes are present, and no more.  They need to be able to make sure that the config file writer does not pollute the namespace with junk!

But, if you’re doing any sort of transformation to set attributes, you will probably want helper variables that do NOT get presented as part of the program’s configuration attributes. For that, we use the so-called *"tusk"* operator, used in languages like go.  We did actually see this above:

```bash
text := run("ls -l")
```

This command will create a variable called `text` that’s available in the execution environment, but is not used by the program you’re configuring.  And, when we add runtime callbacks into the language (so that you can configure based on dynamic runtime information), the default will be that the variable state will still be available.

# For loops

Con4m currently doesn’t allow indefinite loops, but `for` loops work, and so do `break` and `continue`:

```bash
path := env("PATH")
desired := "~"
found = false
parts := split(path, ":")
for i from 0 to len(parts) {
  if parts[i] == "" {
     continue
  }
  elif parts[i] == desired {
    break
  }
  echo ("Not what I'm looking for: ", parts[i])
}
if found {
  echo("Found it!")
}
```

Some things to note here:

1. If we hadn’t set the variable `found` before the start of the loop, this code could throw a runtime error; the language is not doing the static analysis to detect such conditions, so these would show up when evaluating the configuration file.
2. We currently only support `for i from n to m` syntax.  The numbers can go up or down, but we don’t support steps, or iterating explicitly over arrays or dictionaries.  This will probably change.
3. The user is not allowed to modify the value of the index variable themselves. It must be an identifier (not a complex expression), and it can only be incremented by the system, helping to prevent infinite loops.
4. The desire to ensure termination is why there currently isn’t a `while` loop.  We will probably add it at some point, but in conjunction with a ceiling on overall config file execution time.
5. The `end` value in the range is evaluated BEFORE the loop body runs, not on every iteration.

# Data types and typing

Con4m has lists and dictionaries, with syntax like you’d expect:

```bash
my_path = ["/home/viega/bin", "/usr/sbin", "/usr/bin", "."]
user_priorities = { "john@crashoverride.com" : 5,
                    "mark@crashoverride.com" : 1,
                    "theo@crashoverride.com" : 10
                   }
```

You can access items with square brackets:

```bash
for i from 0 to len(my_path) {
  echo(my_path[i])
}

echo user_priorities["john@crashoverride.com"]
```

However, there is not currently a syntax for setting individual fields in dicts or lists, as you can not yet — we will add this soon via builtin calls, and eventually will improve assignment so that you can have array and member access on the left hand side of the assignment.

Note that list items must all have the same type.  Similarly, dictionary keys must always have the same type, as much dictionary values.  Additionally, dictionary keys are currently limited to only strings and integers.

Con4m does significant static type checking.  Once an attribute or variable is assigned a type, any inconsistencies will result in a compile-time (config load time) error.  All types are currently inferred from the context -- you do not have to declare types.

Of course, you can set individual attributes to any type you want. They just cannot change.  And, most uses of Con4m will specify a schema, meaning that after your config finishes loading, it will be generally checked against the specified types, and any incompatibility will again produce an error (though the application could choose to ignore it).

Con4m also has tuple types.  For instance, the built in `system()` function acts like `run()`, except that it returns a tuple, the first value containing the output of the command, the second tuple containing the exit value:

```bash
output, exitval = system("mycommand -x")

if exitval != 0 {
  echo("That didn't work :(")
} else {
  echo(output)
}
```

# Functions and callbacks

Con4m does support user-defined functions.  You can create and call a function just for your internal use, using the `func` keyword, or for the program to call back into at runtime with the `callback` keyword.

All the code in your config file NOT in a callback will run when the config file loads. Callbacks run after, whenever the program decides it needs to call it.  To that end, you can only define callbacks with name and parameters that are actually used by the application.

Functions, though, you can call yourself. For instance:

```
func fact(f) {
  result := 1
  for i from 1 to f {
    result := result * i
  }
}

for i from 1 to 10 {
  x := fact(i)
  echo (format("fact({i}) = {x}"))
}
```

Some things to note about functions:
- You CANNOT use global variables inside functions or callbacks.  They will not be available.  Pass them in as parameters.  This is because we reset global variable state if a config file gets reloaded, or if a second config file gets 'stacked' on top of this one. Particularly for stacked configuration files, it would be too much of a surprise for globals to persist (whereas attributes would be expected to persist).
- Currently, you can only access GLOBAL attributes from within functions, because we have not yet implemented the dot operator for member access. This will change.
- You CAN call `func`s from your callbacks; they will run at the time of the callback.
- You never declare types for function parameters. The system is capable of figuring out all required type information statically.
- You do not need to worry about calling a function in a place in the file before you define it. If needed, the compiler will make a second pass.
- Currently, users can NOT declare varargs functions. We may choose to add this, but do not right now.
- We do not support default arguments.
- Recursive functions are available by default, but they can be turned off at the time the program using con4m is compiled.
- If you use an enum (which defines immutable constants), they generally live in the scope of VARIABLES.  They go away if a new config file gets loaded on top of yours, so they should not be used if called from callbacks.  However, we do explicitly make them available for local functions.  If a callback tries to use one when a new config has been layered in, you could end up with significant issues. I am thinking about removing enums, or else giving them their own permanant status without making them an attribute.  Haven't decided which is best yet, so they're currently living in this in-between state.

# Learn more!

- Browse the current list of [builtin function calls](builtins.md).
- View the [syntax reference](langref.md).
- [Check out the developer API](nim-api.md).
- [Go back to the home page.](https://github.com/crashappsec/con4m)