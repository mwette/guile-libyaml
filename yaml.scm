;; yaml.scm - yaml

;; Copyright (C) 2020-2021,2025 Matthew Wette
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
  #:use-module (yaml cdata)
  #:use-module ((system foreign) #:prefix ffi:))

(define ptr-addr ffi:pointer-address)
(define-once cdata&-sel
  (lambda (data . tags)
    (let* ((data (apply cdata-sel data tags))
           (bptr (ffi:bytevector->pointer (cdata-bv data)))
           (addr (+ (ffi:pointer-address bptr) (cdata-ix data)))
           (type (name-ctype 'yaml_node_t* (cpointer (cdata-ct data)))))
      (make-cdata type addr))))

(define fopen
  (let ((~fopen (ffi:pointer->procedure
                 '* (dynamic-func "fopen" (dynamic-link)) (list '* '*))))
    (lambda (filename mode)
      (~fopen (ffi:string->pointer filename) (ffi:string->pointer mode)))))

(define fclose
  (let ((~fclose (ffi:pointer->procedure
                 ffi:int (dynamic-func "fclose" (dynamic-link)) (list '*))))
    (lambda (file)
      (~fclose file))))

(cond-expand
 (guile-3
  (use-modules (ice-9 exceptions))
  (define (file-not-found filename)
    (raise-exception
     (make-exception-with-message
      (string-append
       "read-yaml-file: can't access file: " filename)))))
 (else
  (define (file-not-found filename)
    (scm-error 'misc "read-yaml-file"
               "file not found: ~S" (list filename) #f))))

;; tag-property created via (make-object-property) used to track tags
(define* (cnvt-tree root stack #:optional tag-property)

  ;; not implemented yet
  (define (add-node-tag node tag) node)
  (define add-node-tagx
    (let ((dict (make-hash-table 31)))
      (lambda (node tag)
        (if (and tag-property
                 (not (equal? tag "tag:yaml.org,2002:str")))
            ;;(unless (hash-ref dict tag) (hash-set! dict tag tag))
            (set! (tag-property node) (hash-ref dict tag)))
        node)))

  (define (cnvt-scalar-node node)
    (let* ((style (cdata*-ref node 'data 'scalar 'style))
           (ptr-value (cdata*-ref node 'data 'scalar 'value))
           (value (ffi:pointer->string ptr-value)))
      value))

  (define (cnvt-sequence-node node)
    (let* ((style (cdata*-ref node 'data 'scalar 'style))
           (start (ptr-addr (cdata*-ref node 'data 'sequence 'items 'start)))
           (end (ptr-addr (cdata*-ref node 'data 'sequence 'items 'end)))
           (top (ptr-addr (cdata*-ref node 'data 'sequence 'items 'top)))
           (item-size (ctype-size yaml_node_item_t)))
      (let loop ((sequence '()) (addr (- top item-size)))
        (if (>= addr start)
            (let* ((item (make-cdata (cpointer 'int) addr))
                   (indx (1- (cdata-ref item '*)))
                   (nd (cdata&-sel stack indx)))
              (loop (cons (cnvt-node nd) sequence) (- addr item-size)))
            (list->vector sequence)))))

  (define (cnvt-mapping-node node)
    (let* ((style (cdata*-ref node 'data 'scalar 'style))
           (start (ptr-addr (cdata*-ref node 'data 'mapping 'pairs 'start)))
           (end (ptr-addr (cdata*-ref node 'data 'mapping 'pairs 'end)))
           (top (ptr-addr (cdata*-ref node 'data 'mapping 'pairs 'top)))
           (pair-size (ctype-size yaml_node_pair_t)))
      (let loop ((mapping '()) (addr (- top pair-size)))
        (if (>= addr start)
            (let* ((pair (make-cdata yaml_node_pair_t* addr))
                   (key-ix (1- (cdata*-ref pair 'key)))
                   (key-nd (cdata&-sel stack key-ix))
                   (val-ix (1- (cdata*-ref pair 'value)))
                   (val-nd (cdata&-sel stack val-ix)))
              (loop (acons (cnvt-node key-nd) (cnvt-node val-nd) mapping)
                    (- addr pair-size)))
            mapping))))

  (define (cnvt-node node)
    (let ((type (cdata-ref node '* 'type)))
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
         (&parser (cdata& parser))
         (document (make-cdata yaml_document_t))
         (&document (cdata& document))
         (file (if (access? filename R_OK)
                   (fopen filename "r")
                   (file-not-found filename))))

    (yaml_parser_initialize &parser)
    (yaml_parser_set_input_file &parser file)
    (yaml_parser_load &parser &document)
    
    (let* ((start (cdata-sel document 'nodes 'start))
           (stack start)
           (root (yaml_document_get_root_node &document))
           (yaml (cnvt-tree root stack)))
      (yaml_document_delete &document)
      (yaml_parser_delete &parser)
      (fclose file)
      yaml)))

;; --- last line ---
