let run callback = Lwt_main.run @@ Server.start_server callback
