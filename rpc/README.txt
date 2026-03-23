In the .gwf file, add

autocomplete_rpc=/search
rpc_datalist=1
rpc_port=8080
rpc_server=127.0.0.1
rpc_server_url=127.0.0.1

To build and install:
make distrib-rpc

To launch:
<path>/rpc_server --idx <path>/<basename>/cache -i localhost

Autocompletions will appear as soon as 3 characters have been typed.
The browser specific autocompletion pop-up (Firefox) may obscure GeneWeb's autocompletion window.
Clicking once in the input line should remove it.