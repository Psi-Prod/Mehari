type request = Request.t
type response = Response.t
type handler = request -> response Lwt.t
type 'a status = 'a Response.status
type mime = Mime.t
type body = Response.body
type gemtext = Gemtext.t

let text = Response.text
let gemtext = Response.gemtext
let page = Response.page

include Mime

let make_mime = Mime.make
let response = Response.response
let respond = Response.respond

include Response.Status

let serve = Server.serve
let run = Server.run
