open Lwt.Infix
open Base
open Httpaf
open Httpaf_lwt_unix

let program = Lib.Execution_engine.program

module Server = struct
  let get =
    let text = Lib.Execution_engine.run_json program in
    let headers =
      Headers.of_list [("content-length", Int.to_string (String.length text))]
    in
    let handler reqd =
      let {Request.target; _} = Reqd.request reqd in
      let request_body = Reqd.request_body reqd in
      Body.close_reader request_body ;
      match target with
      | "/" ->
          Reqd.respond_with_string reqd (Response.create ~headers `OK) text
      | _ ->
          Reqd.respond_with_string
            reqd
            (Response.create `Not_found)
            "Route not found"
    in
    handler


  let error_handler ?request:_ error start_response =
    let response_body = start_response Headers.empty in
    ( match error with
    | `Exn exn ->
        Body.write_string response_body (Exn.to_string exn) ;
        Body.write_string response_body "\n"
    | #Status.standard as error ->
        Body.write_string response_body (Status.default_reason_phrase error) ) ;
    Body.close_writer response_body
end

let request_handler (_ : Unix.sockaddr) = Server.get

let error_handler (_ : Unix.sockaddr) = Server.error_handler

let main port =
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
  "port" |> Stdio.In_channel.read_all |> String.strip |> Int.of_string |> main
