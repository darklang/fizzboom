open Httpaf

let fizzbuzz = Lib.Execution_engine.fizzbuzz

let fizzboom = Lib.Execution_engine.fizzboom

let (let*) = Lwt.bind
let (and*) = Lwt.both

module Server = struct
  let get reqd =
    match Reqd.request reqd  with
    | { Request.meth = `POST; headers; _ } ->
      let response =
        let content_type =
          match Headers.get headers "content-type" with
          | None   -> "application/octet-stream"
          | Some x -> x
        in
        Response.create ~headers:(Headers.of_list ["content-type", content_type; "connection", "close"]) `OK
      in
      let request_body  = Reqd.request_body reqd in
      let response_body = Reqd.respond_with_streaming reqd response in
      let rec on_read buffer ~off ~len =
        Lwt.async @@ fun () -> Lwt.Infix.(
          Lwt_unix.sleep(1.0) >>= fun () ->
          Body.schedule_bigstring response_body buffer ~off ~len;
          Body.flush response_body (fun () ->
            Body.schedule_read request_body ~on_eof ~on_read);
          Lwt.return ()
        );
      and on_eof () =
        Body.close_writer response_body
      in
      Body.schedule_read (Reqd.request_body reqd) ~on_eof ~on_read
    | _ ->
      let headers = Headers.of_list [ "connection", "close" ] in
      Reqd.respond_with_string reqd (Response.create ~headers `Method_not_allowed) ""



    (* let {Request.target; _} = Reqd.request reqd in *)
    (* let on_eof = body.close_write responrd_ody *)
    (* let handle program  = *)
    (*   let* text = Lib.Execution_engine.run_json program in *)
    (*   let headers = *)
    (*     Headers.of_list [("content-length", Int.to_string (String.length text))] *)
    (*   in *)
    (*   Reqd.Body.schedule_read reqd ~on_read(Response.create ~headers `OK) text; *)
    (* in *)
    (* let request_body = Reqd.request_body reqd in *)
    (* Body.close_reader request_body ; *)
    (* match target with *)
    (* | "/fizzbuzz" -> *)
    (*     handle fizzbuzz *)
    (* | "/fizzboom" -> *)
    (*     handle fizzboom *)
    (* | _ -> *)
    (*     (Reqd.respond_with_string *)
    (*       reqd *)
    (*       (Response.create `Not_found) *)
    (*       "Route not found"; Lwt.return_unit) *)


  let error_handler ?request:_ error start_response : unit =
    let response_body = start_response Headers.empty in
    ( match error with
    | `Exn exn ->
        Body.write_string response_body (Base.Exn.to_string exn) ;
        Body.write_string response_body "\n"
    | #Status.standard as error ->
        Body.write_string response_body (Status.default_reason_phrase error) ) ;
    Body.close_writer response_body
end

let request_handler (_ : Unix.sockaddr) = Server.get

let error_handler (_ : Unix.sockaddr) = Server.error_handler

let main port =
  let open Lwt.Infix in
  let listen_address =
    let open Unix in
    ADDR_INET (inet_addr_loopback, port)
  in
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket
        listen_address
        (Httpaf_lwt_unix.Server.create_connection_handler
           ~request_handler
           ~error_handler)
      >|= fun _server -> Stdio.printf "Listening on port %i.\n" port) ;
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever


let () =
  let _options = [
   ("fd_passing", `fd_passing)
  ; ("fdatasync", `fdatasync)
  ; ("get_affinity", `get_affinity)
  ; ("get_cpu", `get_cpu)
  ; ("get_credentials", `get_credentials)
  ; ("libev", `libev)
  ; ("madvise", `madvise)
  ; ("mincore", `mincore)
  ; ("recv_msg", `recv_msg)
  ; ("send_msg", `send_msg)
  ; ("set_affinity", `set_affinity)
  ; ("wait4", `wait4)
  ] in
  (* Base.List.iter options ~f:(fun (str, opt) -> print_endline ("option " ^ str ^ ": " ^ (string_of_bool(Lwt_sys.have opt)) )); *)
  "port"
  |> Stdio.In_channel.read_all
  |> Base.String.strip
  |> Base.Int.of_string
  |> main
