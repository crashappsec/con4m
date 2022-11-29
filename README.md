# Con4m: Configuration Language for Mortals

For the SAMI project, Iwanted the flexibility of being able to expose
some scriptability, and be do some basic string interpolations.
However, HCL is overkill for most things, and overcomplicated. I
didn't want to have to implement all that (or switch to golang).

 
Con4m, like HCL, is in the UCL/NGINX family of config file formats,
allowing some lightweight programming (unlike UCL), but nowhere near
as crazy as HCL.

You can easily add your own builtin functions to expose to the user.
And, if a user needs more scriptability than you give them, you can
call out to other programs, e.g., with the builtin `run()` function.

You don't actually have to use the provided built-in functions.  But
you shouldn't recreate the wheel either. For instance, the existing
`run()` function DOES make sure to drop privileges before running.

Also, everything outside of strings gets evaluated at "compile" time
(i.e., when the configuration file is loaded), but the string
interpolations of the form `{variable}` or `{variable:fmt}` get
evaluated at runtime.  `{{` escapes the format specifier.

Note well, if a user assigns local variables, the value of any
variable the user defined will be replaced at compile time, not when
queried by the application.

Sometime soon, I'm planning on adding JSON interoperability, and hot
configuration loading (i.e., that works in multi-threaded
environments, swapping in the new config atomically).