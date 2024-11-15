;; yaml.scm - yaml

;; Copyright (C) 2020-2021,2024 Matthew Wette
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

;;; Code:

(define-module (yaml)
  #:export (read-yaml-file cnvt-tree)
  #:use-module (yaml libyaml)
  #:use-module (nyacc foreign cdata)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (system foreign-library)
  #:version (2 0 0))

(use-modules (ice-9 format))
(define (ff fmt . args) (apply format #t fmt args))
(define (sf fmt . args) (apply simple-format #t fmt args))
(use-modules (ice-9 pretty-print))
(define pp pretty-print)

(use-modules (ice-9 exceptions))
(define (file-not-found filename)
  (raise-exception
   (make-exception-with-message
    (string-append
     "read-yaml-file: can't access file: " filename))))

(define fopen
  (let ((fopen (foreign-library-function
                #f "fopen" #:return-type '* #:arg-types '(* *))))
    (lambda (path mode)
      (let ((path (ffi:string->pointer path))
            (mode (ffi:string->pointer mode)))
        (fopen path mode)))))

(define fclose
  (foreign-library-function
   #f "fclose" #:return-type ffi:int #:arg-types '(*)))

(define int*-type (cpointer (cbase 'int)))

;; works w/ bytestructures
;; tag-property created via (make-object-property) used to track tags
(define* (cnvt-tree root stack #:optional tag-property)

  ;; not implemented yet
  (define (add-node-tag node tag) node)
  (define add-node-tagx
    (let ((dict (make-hash-table 31)))
      (lambda (node tag)
	(if (and tag-property
		 (not (equal? tag "tag:yaml.org,2002:str")))
	    (set! (tag-property node) (hash-ref dict tag)))
	node)))

  (define (cnvt-scalar-node node)
    (let* ((style (wrap-yaml_scalar_style_t
                   (cdata*-ref node 'data 'scalar 'style)))
	   (raw (cdata*-ref node 'data 'scalar 'value))
	   (val (ffi:pointer->string raw)))
      val))

  (define (cnvt-sequence-node node)
    (let* ((style (wrap-yaml_sequence_style_t
                   (cdata*-ref node 'data 'sequence 'style)))
	   (start-addr (ffi:pointer-address
                        (cdata*-ref node 'data 'sequence 'items 'start)))
	   (end-addr (ffi:pointer-address
                      (cdata*-ref node 'data 'sequence 'items 'end)))
	   (top-addr (ffi:pointer-address
                      (cdata*-ref node 'data 'sequence 'items 'top)))
	   (item-size (ctype-size yaml_node_item_t)))
      (let loop ((sequence '()) (addr (- top-addr item-size)))
	(if (>= addr start-addr)
	    (let* ((item (make-cdata int*-type addr))
		   (indx (1- (cdata*-ref item))) ; ???
		   (nd (cdata-sel stack indx)))
	      (loop (cons (cnvt-node (cdata& nd)) sequence) (- addr item-size)))
	    (list->vector sequence)))))

  (define (cnvt-mapping-node node)
    (let* ((style (cdata*-ref node 'data 'mapping 'style))
	   (start-addr (ffi:pointer-address
                        (cdata*-ref node 'data 'mapping 'pairs 'start)))
	   (end-addr (ffi:pointer-address
                      (cdata*-ref node 'data 'mapping 'pairs 'end)))
	   (top-addr (ffi:pointer-address
                      (cdata*-ref node 'data 'mapping 'pairs 'top)))
	   (pair-size (ctype-size yaml_node_pair_t)))
      (let loop ((mapping '()) (addr (- top-addr pair-size)))
	(if (>= addr start-addr)
	    (let* ((pair (make-cdata yaml_node_pair_t* addr))
		   (key-ix (1- (cdata-ref pair '* 'key)))
		   (key-nd (cdata& (cdata-sel stack key-ix)))
		   (val-ix (1- (cdata-ref pair '* 'value)))
		   (val-nd (cdata& (cdata-sel stack val-ix))))
	      (loop (acons (cnvt-node key-nd) (cnvt-node val-nd) mapping)
		    (- addr pair-size)))
	    mapping))))

  (define (cnvt-node node)
    (let ((type (cdata*-ref node 'type)))
      (add-node-tag
       (case type
	 ((YAML_SCALAR_NODE) (cnvt-scalar-node node))
	 ((YAML_SEQUENCE_NODE) (cnvt-sequence-node node))
	 ((YAML_MAPPING_NODE) (cnvt-mapping-node node))
	 (else (error "missed type" type)))
       (ffi:pointer->string (cdata*-ref node 'tag)))))

  (cnvt-node root))

(define (read-yaml-file filename)
  (let* ((parser (make-cdata yaml_parser_t))
	 (&parser (cdata&-ref parser))
	 (document (make-cdata yaml_document_t))
	 (&document (cdata&-ref document))
	 (file (if (access? filename R_OK)
		   (fopen filename "r")
		   (file-not-found filename))))

    (yaml_parser_initialize &parser)
    (yaml_parser_set_input_file &parser file)
    (yaml_parser_load &parser &document)
    
    (let* ((start (cdata-ref document 'nodes 'start))
	   (stack (make-cdata yaml_node_t* start))
	   (root (cdata-sel (yaml_document_get_root_node &document)))
	   (yaml (cnvt-tree root stack)))
      (yaml_document_delete &document)
      (yaml_parser_delete &parser)
      (fclose file)
      yaml)))

;; --- last line ---
