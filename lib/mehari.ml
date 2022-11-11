type request = Request.t
type response = Response.t
type handler = request -> response Lwt.t
type 'a status = 'a Response.status
type mime = Mime.t
type body = Response.body
type gemtext = Gemtext.t

let uri = Request.uri
let addr = Request.addr
let port = Request.port

include Response.Status

let text = Response.text
let gemtext = Response.gemtext
let lines = Response.lines
let page = Response.page

include Mime

let make_mime = Mime.make
let response = Response.response
let respond = Response.respond
let serve = Server.serve
let run = Server.run
