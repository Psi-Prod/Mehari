type request = Request.t
type response = Response.t
type handler = request -> response Lwt.t
type 'a status = 'a Response.status
type mime = Response.mime
type body = Response.body
type gemtext = Gemtext.t

let text = Response.text
let gemtext = Response.gemtext
let response = Response.response
let respond = Response.respond
let empty_mime = Response.empty_mime
let make_mime = Response.make_mime

include Response.Status

let serve = Server.serve
let run = Server.run
