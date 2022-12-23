let response_document mime path =
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
              if String.length chunk = chunk_size then (
                consume chunk;
                n := 0;
                loop ())
              else consume chunk
            in
            loop ()))
  in
  Mehari.response_body body mime
