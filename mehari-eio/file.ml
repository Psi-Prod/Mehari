let not_found = Mehari.(response not_found "")

let response_document ?(mime = Mehari.app_octet_stream) path =
  try
    let chunk_size = 16384 in
    let body =
      Mehari.stream (fun consume ->
          Eio.Path.with_open_in path (fun flow ->
              let buf = Eio.Buf_read.of_flow flow ~max_size:max_int in
              let n = ref 0 in
              let rec loop () =
                let chunk =
                  Eio.Buf_read.take_while
                    (fun _ ->
                      incr n;
                      !n <> chunk_size)
                    buf
                in
                if String.length chunk = chunk_size - 1 then (
                  consume chunk;
                  n := 0;
                  loop ())
                else consume chunk
              in
              loop ()))
    in
    Mehari.response_body body mime
  with Eio.Io _ -> not_found

include
  Mehari.Private.Static.Make
    (struct
      module IO = Common.Direct

      type path = [ `Dir ] Eio.Path.t

      let kind path =
        match (Eio.Path.stat ~follow:true path).kind with
        | (`Regular_file | `Directory) as f -> f
        | _ -> `Other

      let exists _ = true
      let read = Eio.Path.read_dir
      let concat = Eio.Path.( / )
      let response_document = response_document
      let pp_io_err = Eio.Exn.pp
    end)
    (Common.Addr)
