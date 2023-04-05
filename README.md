# Con4m: Configuration for Mortals

I got tired of building mini-DSLs on top of YAML, especially since YAML has many syntactic quirks that make it bad for such things.

To the typical user, Con4m looks like a normal config file, somewhere in the NginX family. But! power users get a statically typed go-like language that seamlessly integrates, for any power user needs, but is tailored for configuration use cases (for instance, having built-in data types for things like dates, times, durations and sizes). But, Con4m can be invisible when people don't need the extra power.

Con4m validates configuration files before loading them, even making sure the types and values all what YOU need them to be, if your provide a brief specification defining the schema you want for your config files.  That validation can include common constraints, (like the value must be from a fixed list, must be in a particular range).  There are also constraints for field dependencies, and the ability to write custom field checkers. You basically just write a spec for what you want to accept in your config file, in Con4m, naturally.

Con4m also allows you to 'stack' configuration files. For instance, the app can load an internal default configuration hardcoded into the program, then layer a system-level config over it, then layer a local config on top.

After the configuration file loads, you can call any user-defined functions provided, if your application might need feedback from the user after configuration loads.

You can also create your own builtin functions to make available to users who use the scripting capabilities.  Con4m currently offers about 100 builtins, which you can selectively disable if you so choose.

# Basic Example

Let’s imagine the user has provided a config file like this:

```python
use_color: false

host localhost {
      ip: "127.0.0.1"
      port: 8080
}
host workstation {
      port: 8080
      if env("CUSTOM_VAR") != "" {
         ip: env("CUSTOM_VAR")
      }
      else {
         ip: "10.12.1.10"
      }
}
```

In this example, the conditional runs when the config file is evaluated (if something needs to be evaluated dynamically, you can do that with a callback).

Con4m provides a number of builtin functions like env(), which makes it easy for you to check environment variables, but also for your users to customize environment variables to suit their needs.  You can easily provide your own built-in functions.

Let’s say the application writer has loaded this configuration file into the variable s. She may then write the following c42 spec:

```python
object host {
  field ip {
    type: "string"
    required: true
  }

  field port {
    type: "int"
    required: true
  }

  field use_tls {
    type: "bool"
    default: true
  }
}
```
When you load a user configuration file via Con4m, if you also pass it the above spec, the following will happen (in roughly this order):
- The spec file loads and is validated.
- The user's configuration is read in, and checked for syntax and type safety.
- If the user skips attributes where you've provided a default, those values will be loaded from your spec before evaluation. If you didn't provide a value, but the field is required, then the user gets an appropriate error before the configuration is evaluated.
- The user's config is evaluated.
- The user's config file is checked against the constraints provided in the spec.  You can also provide additional validation constraints, like forcing strings to be from a particular set of values, or having integers be in a range. Whenever these constraints are violated, the user gets a descriptive error message.
You then get an easy API for querying and setting these values as your code runs. And, you can call back into the user's config via callback whenever needed.

# Getting Started

Currently, Con4m hasn't been officially released. We expect to provide a distro with the stand-alone compiler (for testing specs and config files), libcon4m and various language integrations (currently it's just C/C++ and Nim, but we will do at least Python and Go before 1.0).

Right now, you have to build from source, which requires Nim (https://nim-lang.org/), a systems language that's fast and has strong memory safety by default, yet somehow feels like Python.

If you have Nim installed, you can easily install the current version with nimble:

```bash
nimble install https://github.com/crashappsec/con4m
```

Then, you can run the `con4m` compiler, link to libcon4m, or, if you're a Nim user, simply `import con4m`

# More Information

- Getting started.
- [Learn about the Con4m configuration file syntax](docs/writing.md). It’s familiar, fast, easy, while being just powerful enough. And it always terminates.  This is currently a bit out of date... we'll do a doc run before 1.0.
- [Con4m API docs for Nim.](docs/nim-api.md)
- Browse the current list of [builtin function calls](docs/builtins.md).  This is similarly a bit out of date.
- Learn about planned features in [the Github backlog](https://github.com/crashappsec/con4m/issues).

# About

Con4m is open source under the Apache 2.0 license.

Con4m was written by John Viega (john@crashoverride.com).

Pull requests are welcome! If you want to make this work in another language, please do reach out to be first.
