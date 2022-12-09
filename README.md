# Con4m: Configuration for Mortals

Con4m implements an Apache-like configuration file format, with the following benefits:

- The configuration file writer is able to write code in their configuration, giving a lot of flexibility, somewhat like HCL (but far less insane; Con4m is much simpler).
- The programmer can load a config file and validate it, with a minimum of effort.
- It’s designed to allow stacked configuration files.  For instance, the app can load its own, then layer a system-level config over it, then layer a local config on top.

As Con4m develops, we expect it to do things like: handle command-line flags and arguments, allow callbacks into the config file after the config has loaded, and so on.

Currently, Con4m is built for Nim, but we intend to support other language environments.

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

Con4m provides a number of builtin functions like `env()`, and makes it easy to add your own.

Let’s say the application writer has loaded this configuration file into the variable `s`. She may then write the following spec (in Nim; other languages will eventually use Con4m to do the same thing).

```python
var config = con4m(MyApp, s):
  attr("max_timeout", int, required = false) # required = true is the default.
  attr("use_color", bool, true) # defaultVal = works too
  section(host, allowedSubSections = @["*"]): 
      attr("ip", string, required = true)
      attr("port", string, required = true)
      attr("use_tls", bool, defaultAttr = true)
```

This code will:

1. Parse the configuration file, making sure it’s syntactically correct, and that types are consistent.
2. “Run” the configuration file, evaluating all the code.
3. Check that the config file provides everything the spec requires, and nothing more (though you can allow user-defined attributes).
4. Fill in any default values declared in the spec, when the config omits them. After the above code block, the user can then do:

```python
echo "use_color = ", config.use_color # prints false
for host in config.host:
  echo host, host.ip, ":", host.port, "(use_tls = ", host.use_tls
# The above prints, assuming no environment variable set:
127.0.0.1:8080, use_tls = true
10.12.1.10:8080, use_tls = true
```

# Getting Started

Conform is [written in Nim](https://nim-lang.org/), and currently requires it.  Nim is fast like C and Rust, but with a memory model closer to Rust’s, so nice and safe. It easily compiles to a single, statically (like Go).  Plus, it mostly looks like Python. 

If you have Nim installed, you can easily install the current version with nimble:

```bash
nimble install con4m
```

Then, from your Nim application, you only need to: `import con4m`

# More Information

- Getting started.
- [Learn about the Con4m configuration file syntax](docs/writing.md). It’s familiar, fast, easy, while being just powerful enough. And it always terminates.
- [Con4m API docs for Nim.](docs/nim-api.md)
- Browse the current list of [builtin function calls](docs/builtins.md).
- Learn about planned features in [the Github backlog](https://github.com/crashappsec/con4m/issues).

# About

Con4m is open source under the Apache 2.0 license.

Con4m was written by John Viega (john@crashoverride.com).

Pull requests are welcome! If you want to make this work in another language, please do reach out to be first.