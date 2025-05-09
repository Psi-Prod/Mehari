#!/usr/bin/env python3

import os

server = os.getenv("SERVER_SOFTWARE")

print("20 text/gemtext\r")
print(f"Hello from a CGI script served by {server}!")
