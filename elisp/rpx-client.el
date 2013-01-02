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

;; These functions connect to a Repoxy Erlang node using TCP for message
;; exchange.
;;
;; The server startup and TPC port negotiation happens in 'rpx-server.el'
;;
;; And finally message en-/decoding is done in 'rpx-msg.el'

;;; Code:

(require 'rpxu)
(provide 'rpx-client)

;;; Functions

;; Local variables:
;; byte-compile-dynamic: t
;; byte-compile-warnings: (not cl-functions)
;; lexical-binding: t
;; End:

;;; rpx-client.el ends here
