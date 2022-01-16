# test

Starts multiple non-regression tests. Tested modules are `MUtil`, `Util`
(`Test_utils`), the choosen `Sosa` implementation (`Test_sosa`), `Place`
(`Test_place`) and `NotesLink` (`Test_wiki`).
Can be executed with `make test` or `_build/default/test/test.exe`.

```
usage: default/test/test.exe options*
  -conf fn                        Read configuration file.
  -cache-filename str             Cache file to store previous results. (default: /home/ovenstent/gits/geneweb-project/geneweb/_build/oUnit-$(suite_name).cache)
  -chooser {failfirst|simple}
                                  Select the method to choose tests to run. (default: simple)
  -ci {true|false}                Display logs for CI, like Travis and AppVeyor, in the console with colors. (default: false)
  -display {true|false}           Output logs on screen. (default: true)
  -health-check-interval f        Seconds between checking health of workers. (default: 1.)
  -log-encoding str               Encoding of the log. (default: utf-8)
  -no-cache-filename              Reset value of cache_filename.
  -no-output-file                 Reset value of output_file.
  -no-output-html-dir             Reset value of output_html_dir.
  -no-output-junit-file           Reset value of output_junit_file.
  -no-testdata-dir                Reset value of testdata_dir.
  -output-file str                Output verbose log in the given file. (default: /home/ovenstent/gits/geneweb-project/geneweb/_build/oUnit-$(suite_name)-$(shard_id).log)
  -output-html-dir str            Output directory of the HTML files. (default: none)
  -output-junit-file str          Output file for JUnit. (default: none)
  -processes-grace-period f       Delay to wait for a process to stop. (default: 5.)
  -processes-kill-period f        Delay to wait for a process to stop after killing it. (default: 5.)
  -results-style-1-X {true|false} Use OUnit 1.X results printer (will be deprecated in 2.1.0+). (default: false)
  -run-gc-full-major {true|false} Run a Gc.full_major in between tests. (default: true)
  -runner {processes|sequential}
                                  Select a the method to run tests. (default: processes)
  -shards i                       Number of shards to use as worker (threads or processes). (default: 4)
  -suite-name str                 The name of the test suite running. (default: Geneweb)
  -testdata-dir str               Location of the test data directory (absolute path). (default: none)
  -verbose {true|false}           Run test in verbose mode. (default: false)
  -only-test path                 Run only the selected tests.
  -list-test                      List tests
```