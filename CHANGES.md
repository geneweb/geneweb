# Unreleased

## Gwd
- Use `getaddrinfo` to retrieve addresses from the operating system (#2757)

## Breaking changes
- Deprecate the multi-parents feature (#2726)
- The `gwd.arg` is ignored and a warning is printed if there is such a
  file (#2594).
- The `LC_TYPE` environment variable is no longer used to modify the
  default language of `gwd`. Use `LANG` for the same effect (#2594).
- The `-unsafe` and `-force` options of the plugin subsystem are noop.
  Use `--plugins u:...`, `--plugins f:...`, `--plugins uf:...` for the same
  effect (#2594).
