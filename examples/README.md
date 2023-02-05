# Examples

- [hello](hello.ml) — the simplest Mehari server responds to every request with the same message.
- [echo](echo.ml) — demonstrates how to deal with user input.
- [counter](counter.ml) — an example of utilisation of Mehari middleware.
- [log](log.ml) — writing messages to Mehari's log.
- [guestbook](guestbook.ml) — a more complex application.
- [rate_limt](rate_limit.ml) — rate limits a route.
- [client_cert](client_cert.ml) — demonstrates how to deal with client certificate.
- [eio_backend](eio_backend.ml) — a minimalistic app which shows the rudiments of `Mehari_eio`.
- [stream](stream.ml) — take advantage of Eio's direct style to setup a request handler to stream a response body instead of plain text.
- [cgi](cgi.ml) — demonstrates CGI support. Don't forget to make `cgi_script.py` executable.
- [vhost](vhost.ml) — demonstrates the virtual hosting support.

## Unikernel

[unikernel/](unikernel/) is a simple unikernel which serves files located in [unikernel/gemtext/](unikernel/gemtext/).
Copy your private key in [unikernel/tls/server.key](unikernel/tls/server.key) and your certificate in [unikernel/tls/server.pem](unikernel/tls/server.pem) and run the following:

```bash
$ cd unikernel/
$ mirage configure -t unix # initial setup for UNIX backend
$ make depend # install dependencies
$ make # build the program
$ ./gemini-srv # run the program
```
