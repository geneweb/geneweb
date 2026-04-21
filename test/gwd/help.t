Ensures that we don't use unescaped dollars in man pages. See issue
https://github.com/geneweb/geneweb/issues/2771 for more details.

  $ gwd --help > /dev/null
