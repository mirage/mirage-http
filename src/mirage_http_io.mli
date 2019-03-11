(*
 * Copyright (c) 2012-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * %%NAME%% %%VERSION%%
 *)

(** The [IO] module defines the blocking interface for reading
    and writing to Cohttp streams *)
module type S = sig

  (** ['a t] represents a blocking monad state *)
  type +'a t

  (** [a >>= b] will pass the result of [a] to the
      [b] function.  This is a monadic [bind]. *)
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  (** [return a] will construct a constant IO value. *)
  val return : 'a -> 'a t

  (** [ic] represents an input channel *)
  type ic

  (** [oc] represents an output channel *)
  type oc

  (** [conn] represents the underlying network flow *)
  type conn

  (** [read_line ic] will read a single line terminated
      by CR or CRLF from the input channel [ic].  It returns
      {!None} if EOF or other error condition is reached. *)
  val read_line : ic -> string option t

  (** [read ic len] will block until a maximum of [len] characters
      are read from the input channel [ic].  It returns an
      empty string if EOF or some other error condition occurs
      on the input channel, and can also return fewer than [len]
      characters if input buffering is not sufficient to satisfy the
      request. *)
  val read : ic -> int -> string t

  (** [write oc s] will block until the complete [s] string is
      written to the output channel [oc]. *)
  val write : oc -> string -> unit t

  (** [flush oc] will return when all previously buffered content
      from calling {!write} have been written to the output channel
      [oc]. *)
  val flush : oc -> unit t
end

(** Cohttp IO implementation using Mirage channels. *)

module Make (Channel: Mirage_channel_lwt.S) : S
  with type 'a t = 'a Lwt.t
   and type ic = Channel.t
   and type oc = Channel.t
   and type conn = Channel.flow

