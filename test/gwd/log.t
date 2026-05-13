The following tests ensures that `gwd` never prints special characters into 
log files that are not tty.

Cmdliner prints special characters to the output even when the file descriptor 
is not a TTY. It appears this feature cannot be controlled via the Cmdliner API,
except through the NO_COLOR environment variable. If this test fails, try 
setting NO_COLOR before investigating further.
$ export NO_COLOR=1

  $ gwd --check 2>&1 | tr -d '\r' | cat -v
  Geneweb 7.1.0-beta2-281-g271d78f
  Listen to http://gloin:2317/base
  Type CTRL+C to stop the service
  
  source: geneweb/geneweb
  branch: move-rpc-service3
  commit: 271d78f72
  gwd: "gwd"
  working_dir: "$TESTCASE_ROOT"
  gw_prefix: "/home/tiky/git/geneweb/move-rpc-service3/_build/install/default/share/geneweb/hd"
  etc_prefix: "/home/tiky/git/geneweb/move-rpc-service3/_build/install/default/share/geneweb/hd/etc"
  images_prefix: "/home/tiky/git/geneweb/move-rpc-service3/_build/install/default/share/geneweb/hd/images"
  images_dir: ""
  assets: "gw",
          "/home/tiky/git/geneweb/move-rpc-service3/_build/install/default/share/geneweb/hd",
          "/home/tiky/git/geneweb/move-rpc-service3/_build/install/default/share/geneweb/hd/etc"
  
  DEBUG GWD  Starting the HTTP server...
  DEBUG GWD  End of check mode.

  $ gwd --check --log '<stdout>' | tr -d '\r' | cat -v
  Geneweb 7.1.0-beta2-281-g271d78f
  Listen to http://gloin:2317/base
  Type CTRL+C to stop the service
  
  source: geneweb/geneweb
  branch: move-rpc-service3
  commit: 271d78f72
  gwd: "gwd"
  working_dir: "$TESTCASE_ROOT"
  gw_prefix: "/home/tiky/git/geneweb/move-rpc-service3/_build/install/default/share/geneweb/hd"
  etc_prefix: "/home/tiky/git/geneweb/move-rpc-service3/_build/install/default/share/geneweb/hd/etc"
  images_prefix: "/home/tiky/git/geneweb/move-rpc-service3/_build/install/default/share/geneweb/hd/images"
  images_dir: ""
  assets: "gw",
          "/home/tiky/git/geneweb/move-rpc-service3/_build/install/default/share/geneweb/hd",
          "/home/tiky/git/geneweb/move-rpc-service3/_build/install/default/share/geneweb/hd/etc"
  
  DEBUG GWD  Starting the HTTP server...
  DEBUG GWD  End of check mode.

  $ gwd --check --log '<stderr>' 2>&1 | tr -d '\r' | cat -v
  Geneweb 7.1.0-beta2-281-g271d78f
  Listen to http://gloin:2317/base
  Type CTRL+C to stop the service
  
  source: geneweb/geneweb
  branch: move-rpc-service3
  commit: 271d78f72
  gwd: "gwd"
  working_dir: "$TESTCASE_ROOT"
  gw_prefix: "/home/tiky/git/geneweb/move-rpc-service3/_build/install/default/share/geneweb/hd"
  etc_prefix: "/home/tiky/git/geneweb/move-rpc-service3/_build/install/default/share/geneweb/hd/etc"
  images_prefix: "/home/tiky/git/geneweb/move-rpc-service3/_build/install/default/share/geneweb/hd/images"
  images_dir: ""
  assets: "gw",
          "/home/tiky/git/geneweb/move-rpc-service3/_build/install/default/share/geneweb/hd",
          "/home/tiky/git/geneweb/move-rpc-service3/_build/install/default/share/geneweb/hd/etc"
  
  DEBUG GWD  Starting the HTTP server...
  DEBUG GWD  End of check mode.

  $ gwd --check --log foo && tr -d '\r' < foo | cat -v
  Geneweb 7.1.0-beta2-281-g271d78f
  Listen to http://gloin:2317/base
  Type CTRL+C to stop the service
  
  source: geneweb/geneweb
  branch: move-rpc-service3
  commit: 271d78f72
  gwd: "gwd"
  working_dir: "$TESTCASE_ROOT"
  gw_prefix: "/home/tiky/git/geneweb/move-rpc-service3/_build/install/default/share/geneweb/hd"
  etc_prefix: "/home/tiky/git/geneweb/move-rpc-service3/_build/install/default/share/geneweb/hd/etc"
  images_prefix: "/home/tiky/git/geneweb/move-rpc-service3/_build/install/default/share/geneweb/hd/images"
  images_dir: ""
  assets: "gw",
          "/home/tiky/git/geneweb/move-rpc-service3/_build/install/default/share/geneweb/hd",
          "/home/tiky/git/geneweb/move-rpc-service3/_build/install/default/share/geneweb/hd/etc"
  
  DEBUG GWD  Starting the HTTP server...
  DEBUG GWD  End of check mode.
