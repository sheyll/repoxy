;;; rpx-client.el --- High-Level Client for a Repoxy Erlang Node

;; Copyright (C) 2012 Sven Heyll

;; Author: Sven Heyll <sven.heyll@gmail.com>

;; Repoxy is free software: you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; Repoxy is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; Repoxy.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; These functions start a Repoxy erlang node and connect to it using the
;; proprietary TCP text protocol. Each message consists of a length field and a
;; single s-expression or raw text. The s-expression are a super-stupid
;; representation of erlang terms: Each erlang tuple is converted to a list,
;; each atom is converted to a symbol with the same name and a colon in front:
;; ":" A string is simply the string with double quotes. Lists are lists, and
;; 2-tuples are represented as cons-cells. A message header looks like this:
;; "Length: [0-9]+\nContent-Type: text|s-expression\n\n". After the header
;; follows the message body.
;;
;; A client will start the server, at a certain base directory as found by the
;; functions in 'rpx-project-finder.el'.
;;
;; The server startup and TPC port negotiation happens in 'rpx-server.el'
;;
;; The TCP client code that provides an asynchronous message queue for commands
;; to send to a server is provided by 'rpx-tcp.el'
;;
;; And finally message en-/decoding is done in 'rpx-msg.el'

;;; Code:

(require 'rpxu)
(require 'rpx-project-finder)

(provide 'rpx-client)

;; Local variables:
;; byte-compile-dynamic: t
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; rpx-client.el ends here
