# Examples

In order to make examples work, you need to generate an SSL certificate in root path of the repo:
```bash
openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -sha256 -days 365 -nodes --subj "/CN=localhost"
```

- [hello](hello.ml) — the simplest Mehari server responds to every request with the same message.
- [echo](echo.ml) — demonstrates how to deal with user input.
- [counter](counter.ml) — an example of utilisation of Mehari middleware.
- [log](log.ml) — writing messages to Mehari's log.
- [guestbook](guestbook.ml) — a more complex application.
- [rate_limt](rate_limit.ml) — rate limits a route.
- [client_cert](client_cert.ml) — demonstrates how to deal with client certificate.
- [stream](stream.ml) — setup a request handler to stream a response body instead of plain text.
- [cgi](cgi.ml) — demonstrates CGI support. Don't forget to make `cgi_script.py` executable.
- [vhost](vhost.ml) — demonstrates the virtual hosting support.
