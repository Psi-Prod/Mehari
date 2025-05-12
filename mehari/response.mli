(** Gemini server response. *)

(** {1 Body} *)

module Body : sig
  type t
  (** Gemini response body. *)

  (** {2:note-on-data-stream-response A note on data stream response}

      Mehari offers ways to keep client connections open forever and stream data
      in real time such as {!val:seq} and {!val:stream} functions when the
      [flush] parameter is specified. It is important to note that most Gemini
      clients do not support streaming and should be used with caution. That's
      why this parameter is set to [false] by default in all the functions that
      Mehari expose. *)

  val string : string -> t
  (** Creates a body from a string. *)

  val gemtext : Gemtext.t -> t
  (** Creates a body from a {!type:Gemtext.t} document. *)

  val lines : string list -> t
  (** Creates a body from given lines. Each line is written followed by a
      newline ([LF]) character. *)

  val page : title:string -> string -> t
  (** [page ~title content] creates a simple Gemtext body of form:
      {@gemtext[
        # title
        content
      ]} *)

  val seq : ?flush:bool -> string Seq.t -> t
  (** Creates a body from a string sequence. See
      {!section:"note-on-data-stream-response"} for a description of [flush]
      parameter. *)

  val stream : ?flush:bool -> ((string -> unit) -> unit) -> t
  (** [stream (fun consume -> ...)] creates a body from a data stream. Each call
      to [consume] write the given input on socket. Useful for stream data or
      file chunk in real time. See {!section:"note-on-data-stream-response"} for
      a description of [flush] parameter. *)
end

(** {1 Status} *)

module Status : sig
  (** A wrapper around Gemini status codes.
      @see < https://geminiprotocol.net/docs/protocol-specification.gmi >
        Section "Status codes" for a description of the meaning of each code. *)

  type 'a t
  (** Status of a Gemini response *)

  (** {2 Status} *)

  val input : string t
  (** Status 10: input expected. *)

  val sensitive_input : string t
  (** Status 11: sensitive input expected. *)

  val success : Body.t -> Mime.t t
  (** Status 20: success. *)

  val redirect_temp : string t
  (** Status 30: temporary redirection. *)

  val redirect_perm : string t
  (** Status 31: permanent redirection. *)

  val temporary_failure : string t
  (** Status 40: temporary failure. *)

  val server_unavailable : string t
  (** Status 41: server unavailable. *)

  val cgi_error : string t
  (** Status 42: CGI error. *)

  val proxy_error : string t
  (** Status 43: proxy error. *)

  val slow_down : string t
  (** Status 44: slow down. *)

  val perm_failure : string t
  (** Status 50: permanent failure. *)

  val not_found : string t
  (** Status 51: not found. *)

  val gone : string t
  (** Status 52: gone. *)

  val proxy_request_refused : string t
  (** Status 53: proxy request refused. *)

  val bad_request : string t
  (** Status 59: bad request. *)

  val client_cert_req : string t
  (** Status 60: client certificate required. *)

  val cert_not_authorised : string t
  (** Status 61: client certificate not authorized. *)

  val cert_not_valid : string t
  (** Status 62: client certificate not valid. *)

  (** {2 Utils} *)

  val code_of_status : _ t -> int
  (** [code_of_status s] is status code associated with status [s]. *)
end

type t
(** Gemini server response. *)

(** {1 Creation} *)

val respond : 'a Status.t -> 'a -> t
(** Creates a new response with given {!type:Status.t}.

    @raise Invalid_argument if [meta] is more than 1024 bytes.
    @raise Invalid_argument if [meta] starts with [U+FEFF] byte order mark. *)

val body : Body.t -> Mime.t -> t
(** Creates a successful response with given body and use given mime as MIME
    type. *)

val text : string -> t
(** Creates a successful response with given text and use [text/plain] as MIME
    type. *)

val gemtext : ?charset:string -> ?lang:string list -> Gemtext.t -> t
(** Creates a successful response with given gemtext and use [text/gemini] as
    MIME type. *)

(** {1 Access} *)

val status : t -> int

(**/**)

module Private : sig
  type view = Immediate of string | Chunks of stream
  and stream = { body : (string -> unit) -> unit; flush : bool }

  val view_of_resp : t -> view

  val raw : int -> string -> string -> t
  (** [raw code meta body] creates a new raw response. Does not perform any
      check on validity i.e. length of header or beginning with a byte order
      mark [U+FEFF].

      This function is mainly intended for CGI implementation. *)

  val unsafe_raw : string -> t
  (** [unsafe_raw resp] creates a new raw response. Does not perform any check
      on validity i.e. length of header or beginning with a byte order mark
      [U+FEFF].

      This function is mainly intended for CGI implementation. *)
end

(**/**)
