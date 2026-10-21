GeneWeb RPC autocompletion server
=================================

rpc_server serves the autocompletion of the welcome page and of the
individual and family forms over a WebSocket: the browser receives only
the entries matching what is typed, instead of whole .gz lists. It is
meant for large bases; on smaller ones the .gz files alone are enough.

1. Build and install

     make distrib-rpc

   installs gw/rpc_server next to gwd.

2. Build the indexes

   The server indexes the .gz files written by cache_files, one per
   field type (first names, surnames, places, sources...):

     gw/cache_files -bd bases -datalist -all <base>

   They are not updated automatically: rerun cache_files after data
   changes, then restart the server. A field type whose file is missing
   gets no suggestions: to serve only some types, replace -all by their
   options (cache_files -help).

3. Launch the server

     gw/rpc_server --idx bases/etc/<base>/cache -i <server IP>

   --idx DIR       directory scanned, recursively, for .gz files
   -i INTERFACE    interface to listen on; the default, localhost, only
                   accepts browsers running on the same computer
   -p PORT         port; default 8080, or 8443 with TLS
   --crt, --key    TLS certificate and key, required when the GeneWeb
                   pages are served over https
   rpc_server --help lists the other options.

4. Configure the base (.gwf)

     rpc_datalist=1
     rpc_server_url=<server IP>

   rpc_server_url is the address of the server as seen by the visitor's
   browser; the port defaults to 8080. The datalist_xxx options then only
   select the .gz files loaded when the server is unreachable. The "RPC
   autocompletion server" section of gw/a.gwf documents every form of
   rpc_server_url, and rpc_debug.

5. Check

   - rpc_debug=yes in the .gwf logs the activity in the browser console.
   - rpc/test/autocomplete.html, in the sources, connects to a server and
     lists the indexes it has loaded.
   - Suggestions appear from the second character typed.