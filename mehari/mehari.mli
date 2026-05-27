(** This module provides the core abstraction, it does not depend on any
    platform code, and does not interact with the environment. *)

(** {1 Types} *)

type request = Request.t
(** [request] is an alias for {!type:Request.t}. See {!section-request}. *)

type response = Response.t
(** [response] is an alias for {!type:Response.t}. See {!section-response}. *)

type handler = Handler.t
(** Handlers are asynchronous functions from {!type:request} to
    {!type:response}. *)

(** {1:gemtext Gemtext} *)

module Gemtext = Gemtext

(** {1:request Request} *)

module Request = Request

(** {1:response Response} *)

module Response = Response

(** {1:status Response status} *)

module Status = Response.Status

(** {1 Response body} *)

module Body = Response.Body

(** {1:mime Mime} *)

module Mime = Mime

(** {1 Handler} *)

module Handler = Handler

(** {1 Middleware} *)

module Middleware = Middleware

(** {1 Path} *)

module Path = Path

(** {1 Router} *)

module Router = Router

(** {1:rate_limit Rate limit} *)

module Rate_limit = Rate_limit

(** {1 Logger} *)

module Logger = Logger

(** {1 Server implementation} *)

module Server = Server

(**/**)

module Cgi = Cgi
module Protocol = Protocol

(**/**)
