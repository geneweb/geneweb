  $ gwd --connection-timeout 10
  gwd: --connection-timeout is available only on UNIX.
  [124]

  $ gwd --max-pending-requests 100
  gwd: --max-pending-requests is available only on UNIX.
  [124]

  $ gwd --max-clients 100
  gwd: deprecated option --max-clients: No effect. Use `--n-workers` and
                  `--max-pending-requests` instead.
  gwd: --max-clients is available only on UNIX.
  [124]

  $ gwd --n-workers 5
  gwd: --n-workers is available only on UNIX.
  [124]

  $ gwd --daemon
  gwd: --daemon is available only on UNIX.
  [124]
