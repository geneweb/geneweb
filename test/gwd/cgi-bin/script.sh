#!/usr/bin/env bash
CGI_DIR="./cgi-bin"
SECRET_SALT="000000"
GW_PREFIX="gw"

ln -s ../../hd "$GW_PREFIX"

cd "$CGI_DIR"
"../$GWD" \
  -debug \
  -cgi \
  -cgi_secret_salt "$SECRET_SALT" \
  -bd ../bases \
  -hd "../$GW_PREFIX"
