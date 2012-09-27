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

(** A byte-level transport over the xenstore Unix domain socket *)

open Lwt

let xenstored_socket = ref "/var/run/xenstored/socket"
let debug_delay = ref None

(* Individual connections *)
type t = Lwt_unix.file_descr * Lwt_unix.sockaddr
let create () =
  let sockaddr = Lwt_unix.ADDR_UNIX(!xenstored_socket) in
  let fd = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
  lwt () = Lwt_unix.connect fd sockaddr in
  return (fd, sockaddr)
let destroy (fd, _) = Lwt_unix.close fd
let read (fd, _) x y z = 
	lwt result = Lwt_unix.read fd x y z in
    lwt () = match !debug_delay with 
		| Some x -> Lwt_unix.sleep x
		| None -> return ()
    in
    return result
let write (fd, _) bufs ofs len =
	lwt n = Lwt_unix.write fd bufs ofs len in
	if n <> len then begin
		fail End_of_file
	end else return ()
