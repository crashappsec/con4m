# Con4m Nim API

## Coming soon

For now, there’s auto-generated documentation linked below. I’m not a big fan of the current automatic documentation capabilities for Nim.  Even though you only have to do:

```bash
import con4m
```

To access everything, it generates pages for each module inside the package, even though you can’t really see those modules by default.  What I really want is the exported API you get from importing  the module should be united on one page.  Maybe I’ll build some automation for that at some point.

Until then, I do plan on writing a more in-depth developer intro, but for now, the autogen docs for the main interface, which should meet most needs, is pretty thorough.  See the [documentation for `con4m()`](con4m/codegen.html).

## Notes not in the auto-gen docs
- Con4m assumes that one thread only will compile the state, and that no other threads will attempt to use the config until it's loaded.
- Con4m currently does NOT support the same callback being called from multiple threads in parallel.  If this might happen, you should put a lock around each callback.  We may support this in the future; some of the work is done toward it.

Or, the jumble of auto-gened docs are [available starting here](con4m.html).

## More Info

[Back to the home page.](https://github.com/crashappsec/con4m)