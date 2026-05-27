(** Routing. *)

type route

val route :
  ?rate_limit:Rate_limit.t ->
  ?middlewares:Middleware.t list ->
  ('continuation, Handler.t) Path.t ->
  'continuation ->
  route
(** [route ~rate_limit ~middlewares path handler] forwards requests for [path]
    to [handler]. If rate limit is in effect, [handler] is not executed and a
    respond with {!type:Response.Status.t} {!val:Response.Status.slow_down} is
    sended. An empty list of middlewares and rate limiter are attached by
    default. *)

val router : route list -> Handler.t
(** Creates a router. If none of the routes match the {!type:Request.t}, the
    router returns {!val:Response.Status.not_found}. *)

(** {1:host Virtual hosting} *)

type domain_handler
(** Handler at domain name level. *)

val domain : ?all:Handler.t -> string -> Handler.t -> domain_handler
(** [domain ?all name handler] attaches [handler] to the domain name [name]. If
    [all] is set, it is used to handle domain names of form [*.name].

    @raise Invalid_argument if [name] is not a valid domain name. *)

val virtual_host : domain_handler list -> Handler.t
(** [virtual_host domain_handlers] creates a {!type:Handler.t} which allows
    virtual hosting using TLS Server Name Indication. Respond not found if the
    server Common Name is an IP address. *)
