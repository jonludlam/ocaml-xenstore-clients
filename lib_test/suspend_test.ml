(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Lwt
open Xs_protocol
module Client = Xs_client.Client(Xs_transport_unix_client)
open Client

let ( |> ) a b = b a



let main () =
	Xs_transport_unix_client.debug_delay := Some 2.0;
	lwt client = make () in
    with_xs client
		(fun xs ->
			let threads = List.map (fun x -> 
				lwt result = read xs x in
				Printf.printf "Got: %s\n%!" result;
                return result) ["domid"; "vm"; "name"; "memory/target"; "domid"] in
			let suspend_thread = suspend client in
			lwt () = Lwt_unix.sleep 1.0 in
			let new_threads = List.map (fun x -> 
				lwt result = read xs x in
				Printf.printf "Post suspend request: Got: %s\n%!" result;
                return result) ["domid"; "vm"; "name"; "memory/target" ] in
			lwt () = suspend_thread in
            lwt () = resume client in
            Lwt_list.iter_p (fun x -> x >> return ()) (threads @ new_threads))

let _ =
  Lwt_main.run (main ())
