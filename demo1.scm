;; demo1.scm - yaml demo
;; call this guile-libyaml

;; Copyright (C) 2020 Matthew R. Wette
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>

(add-to-load-path (getcwd))

(use-modules ((system foreign) #:prefix ffi:))
(use-modules (bytestructures guile))
(use-modules (system ffi-help-rt))
(use-modules (yaml) (yaml libyaml))

(use-modules (ice-9 format))
(define (ff fmt . args) (apply format #t fmt args))
(define (sf fmt . args) (apply simple-format #t fmt args))
(use-modules (ice-9 pretty-print))
(define pp pretty-print)

(let* ((filename "demo1.yml")
       (parser (make-yaml_parser_t))
       (&parser (pointer-to parser))
       (document (make-yaml_document_t))
       (&document (pointer-to document))
       (file (fopen filename "r")))

  (yaml_parser_initialize &parser)
  (yaml_parser_set_input_file &parser file)
  (yaml_parser_load &parser &document)
  
  (let* ((start (fh-object-ref document 'nodes 'start))
	 (stack (bytestructure yaml_node_t*-desc start))
	 (root (fh-object-val (yaml_document_get_root_node &document))))
    (pp (cnvt-tree root stack)))
  
  (yaml_document_delete &document)
  (yaml_parser_delete &parser)
  (fclose file))

;; --- last line ---
