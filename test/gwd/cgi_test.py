#!/usr/bin/env python3
import http.server, threading, pycurl, certifi, os, io
import time, sys

def curl():
    buffer = io.BytesIO()
    c = pycurl.Curl()
    c.setopt(c.URL, 'http://127.0.0.1/cgi-bin/script.sh')
    c.setopt(c.PORT, 8000)
    c.setopt(c.WRITEDATA, buffer)
    c.setopt(c.TIMEOUT, 1)
    c.setopt(c.CAINFO, certifi.where())
    c.perform()
    c.close()
    return buffer.getvalue().decode('utf8')

if __name__ == '__main__':
    interface = ('', 8000)
    httpd = http.server.HTTPServer(interface, http.server.CGIHTTPRequestHandler)
    thread = threading.Thread(target=httpd.serve_forever)
    thread.start()
    print(curl())
    httpd.shutdown()
    thread.join()
