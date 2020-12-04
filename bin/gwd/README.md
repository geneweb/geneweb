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
use the `-plugin path/to/plugin_foo.cmxs` option.

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
    plugin_foo.cmxs
```

- `META`: describe your plugin metadata such as name, and dependencies.
- `assets/`: every static assets needed by you plugin (css, js, images, etc...)
- `plugin_foo.cmxs`: the which will load handlers.

#### Allowing gwd to run your plugin

Also, you will need to update `bin/gwd/dune` in order to add your plugin
to plugins whitelist.

```diff
(rule
  (target gwdPluginMD5.ml)
  (deps
    (:cmxs
      %{project_root}/plugins/export/plugin_export.cmxs
+     %{project_root}/plugins/foo/plugin_foo.cmxs
    )
    (:maker mk_gwdPluginMD5.ml)
  )
  (action (with-stdout-to %{target} (run ocaml %{maker} %{cmxs})))
)
```

You can still execute an untrusted plugin with `-unsafe_plugin` and `-unsafe_plugins`.

#### Editing Makefile in order to copy you plugin and assets in the distrib

Edit the `distrib` rule in `Makefile`. E.g.:

```
        mkdir $(DISTRIB_DIR)/gw/plugins
        mkdir $(DISTRIB_DIR)/gw/plugins/export
        cp $(BUILD_DIR)/plugins/export/plugin_export.cmxs $(DISTRIB_DIR)/gw/plugins/export/
+       mkdir $(DISTRIB_DIR)/gw/plugins/foo
+       cp $(BUILD_DIR)/plugins/foo/plugin_foo.cmxs $(DISTRIB_DIR)/gw/plugins/foo/
+       cp $(BUILD_DIR)/plugins/foo/META $(DISTRIB_DIR)/gw/plugins/foo/
+       cp -R $(BUILD_DIR)/plugins/foo/assets/ $(DISTRIB_DIR)/gw/plugins/foo/
```

#### META file

```
version: version of your plugin
maintainers: comma-seperated list of plugin maintainers
depends: comma-seperated list of other plugins needed
```
