open Lwt.Infix
open Base

open Httpaf
open Httpaf_lwt_unix

module Server = struct
  let echo_get reqd =
    match Reqd.request reqd  with
    | { Request.meth = `GET; headers; _ } ->
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
        Body.write_bigstring response_body buffer ~off ~len;
        Body.schedule_read request_body ~on_eof ~on_read;
      and on_eof () =
        Body.close_writer response_body
      in
      Body.schedule_read (Reqd.request_body reqd) ~on_eof ~on_read
    | _ ->
      let headers = Headers.of_list [ "connection", "close" ] in
      Reqd.respond_with_string reqd (Response.create ~headers `Method_not_allowed) ""

  let text = "hello world"
  let text = Bigstringaf.of_string ~off:0 ~len:(String.length text) text

  let benchmark =
    let headers = Headers.of_list ["content-length", Int.to_string (Bigstringaf.length text)] in
    let handler reqd =
      let { Request.target; _ } = Reqd.request reqd in
      let request_body          = Reqd.request_body reqd in
      Body.close_reader request_body;
      match target with
      | "/" -> Reqd.respond_with_bigstring reqd (Response.create ~headers `OK) text;
      | _   -> Reqd.respond_with_string    reqd (Response.create `Not_found) "Route not found"
    in
    handler


  let error_handler ?request:_ error start_response =
    let response_body = start_response Headers.empty in
    begin match error with
    | `Exn exn ->
      Body.write_string response_body (Exn.to_string exn);
      Body.write_string response_body "\n";
    | #Status.standard as error ->
      Body.write_string response_body (Status.default_reason_phrase error)
    end;
    Body.close_writer response_body

end



let request_handler (_ : Unix.sockaddr) = Server.echo_get
let error_handler (_ : Unix.sockaddr) = Server.error_handler

let main port =
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt.async (fun () ->
    Lwt_io.establish_server_with_client_socket
      listen_address
      (Httpaf_lwt_unix.Server.create_connection_handler ~request_handler ~error_handler)
    >|= fun _server ->
      Stdio.printf "Listening on port %i.\n" port);
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
;;

let () =
  main 8081