open Httpaf

let fizzbuzz = Lib.Execution_engine.fizzbuzz

let fizzboom = Lib.Execution_engine.fizzboom

let (let*) = Lwt.bind
let (and*) = Lwt.both

module Server = struct
  let get _ reqd =
    Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
            let handle program =
              let* text = Lib.Execution_engine.run_json program in
              let headers = Headers.of_list [("content-length", Int.to_string (String.length text))] in
              Reqd.respond_with_string reqd (Response.create ~headers `OK) text;
              Lwt.return_unit
            in
            let {Request.meth; target; _} = Httpaf.Reqd.request reqd in
            (match meth, target with
            | `GET, "/fizzbuzz" -> handle fizzbuzz
            | `GET, "/fizzboom" -> handle fizzboom
            | _ ->
              (* let headers = Headers.of_list [ "connection", "close" ] in *)
              (* Reqd.respond_with_string reqd (Response.create ~headers `Method_not_allowed) ""); *)
              Lwt.return_unit))
          (fun exn ->
            Httpaf.Reqd.report_exn reqd exn;
            Lwt.return_unit))

  let error_handler _ ?request:_ error start_response : unit =
    let response_body = start_response Headers.empty in
    ( match error with
    | `Exn exn ->
        Body.write_string response_body (Base.Exn.to_string exn) ;
        Body.write_string response_body "\n"
    | #Status.standard as error ->
        Body.write_string response_body (Status.default_reason_phrase error) ) ;
    Body.close_writer response_body
end

let request_handler = Server.get

let error_handler  = Server.error_handler

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
  let options = [
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
  Base.List.iter options ~f:(fun (str, opt) -> print_endline ("option " ^ str ^ ": " ^ (string_of_bool(Lwt_sys.have opt)) ));
  "port"
  |> Stdio.In_channel.read_all
  |> Base.String.strip
  |> Base.Int.of_string
  |> main
