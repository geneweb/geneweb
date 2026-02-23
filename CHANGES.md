# Unreleased
## Breaking changes
- Deprecate the multi-parents feature (#2726)
- Several arguments in a single line is no longer supported in the
  argument file `gwd.arg` for `gwd` executable (#2594).
- The `LC_TYPE` environment variable is no longer used to modify the
  default language of `gwd`. Use `LANG` for the same effect (#2594).
- The `-unsafe` and `-force` options of the plugin subsystem are noop.
  Use `--plugins u:...`, `--plugins f:...`, `--plugins uf:...` for the same
  effect (#2594).
