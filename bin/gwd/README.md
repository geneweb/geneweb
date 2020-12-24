# gwd - The GeneWeb daemon

## Plugins

### Disclaimer

Plugin system allow you to run **ANY** piece of code as a handler
for any requests.

It means that you could run **harmful** code if you do not control the source
of compiled.

i.e you should not run plugins using the `-unsafe_*` options unless
you are developping your own plugin.

*Reliable* plugins are the ones accepted by `-plugin` and `-plugins` option.

*Reliable* means that the compiled code and assets you load are the one used
in official distribution. It does not make the code safe, but you
know what is actually running on your machine by reading the source code.

### How to load a plugin in gwd

If you want to control what plugins `gwd` loads, and control the order,
use the `-plugin path/to/foo` option, and it will load
`path/to/foo/plugin_foo.cmxs` file.

A simpler solution is to use `-plugins path/to/plugins/` and let
`gwd` load all available plugins in the directory, using `META` files
in order to load the plugins in the right order.

### How to write a plugin for gwd

It is expected that you follow a simple architecture when writing a
plugin for `gwd`.

```
foo/
    META
    assets/
    dune
    plugin_foo.cmxs
```

- `META`: describe your plugin metadata such as name, and dependencies.
- `assets/`: every static assets needed by you plugin (css, js, images, etc...)
- `plugin_foo.cmxs`: the which will load handlers.

The `dune` file must define the `plugin` `alias`.

```
(executable
  (name plugin_foo)
  (modes (native plugin))
)

(alias (name plugin) (deps plugin_foo.cmxs))
```

#### Allowing gwd to run your plugin

Anything in GeneWeb distribution will be registered in whitelist, and
gwd will check file integrity before loading the plugin.

You can still execute an untrusted plugin with `-unsafe_plugin`
and `-unsafe_plugins` options.

#### META file

```
version: version of your plugin
maintainers: comma-seperated list of plugin maintainers
depends: comma-seperated list of other plugins needed
```

### Stability

Plugin system is new and still under heavy test and development.
API should not be considered stable yet.
