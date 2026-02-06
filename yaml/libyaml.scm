;; generated with `guild compile-ffi yaml/libyaml.ffi'
;; using nyacc version 3.02.0

(define-module (yaml libyaml)
  #:use-module (system foreign-library)
  #:use-module ((system foreign) #:prefix ffi:))

(begin
  (use-modules (yaml cdata))
  (define arg->number cdata-arg->number)
  (define arg->pointer cdata-arg->pointer)
  (define (extern-ref obj) (cdata-sel obj '*))
  (define (extern-set! obj val) (cdata-set! obj '* val)))

(eval-when (expand load eval) (define backend 'cdata))

(define (rev-alist l) (map (lambda (p) (cons (cdr p) (car p))) l))

(define (foreign-pointer-search name)
  (define (flc l) (load-foreign-library (car l) #:search-path (list)))
  (let loop ((libs (list #f "libyaml")))
    (cond
      ((null? libs) (error "no library for ~s" name))
      ((false-if-exception (foreign-library-pointer (flc libs) name)))
      (else (loop (cdr libs))))))


;; const char *yaml_get_version_string(void);
(define-public yaml_get_version_string
  (let ((~proc (delay (ffi:pointer->procedure
                       '*
                       (foreign-pointer-search "yaml_get_version_string")
                       (list)))))
    (lambda () (let () ((force ~proc))))))

;; void yaml_get_version(int *major, int *minor, int *patch);
(define-public yaml_get_version
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:void
                       (foreign-pointer-search "yaml_get_version")
                       (list '* '* '*)))))
    (lambda (major minor patch)
      (let ((major (arg->pointer major))
            (minor (arg->pointer minor))
            (patch (arg->pointer patch)))
        ((force ~proc) major minor patch)))))

;; typedef unsigned char yaml_char_t;
(define-public yaml_char_t (name-ctype 'yaml_char_t (cbase 'unsigned-char)))

;; typedef struct yaml_version_directive_s {
;;   /** The major version number. */
;;   int major;
;;   /** The minor version number. */
;;   int minor;
;; } yaml_version_directive_t;
(define-public yaml_version_directive_t
  (name-ctype
   'yaml_version_directive_t
   (cstruct (list `(major ,(cbase 'int)) `(minor ,(cbase 'int))))))
(define-public yaml_version_directive_t*
  (name-ctype 'yaml_version_directive_t* (cpointer yaml_version_directive_t)))
(define-public struct-yaml_version_directive_s
  (name-ctype 'struct-yaml_version_directive_s yaml_version_directive_t))
(define-public struct-yaml_version_directive_s*
  (name-ctype 'struct-yaml_version_directive_s* yaml_version_directive_t*))

;; typedef struct yaml_tag_directive_s {
;;   /** The tag handle. */
;;   yaml_char_t *handle;
;;   /** The tag prefix. */
;;   yaml_char_t *prefix;
;; } yaml_tag_directive_t;
(define-public yaml_tag_directive_t
  (name-ctype
   'yaml_tag_directive_t
   (cstruct
    (list `(handle ,(cpointer (delay yaml_char_t)))
          `(prefix ,(cpointer (delay yaml_char_t)))))))
(define-public yaml_tag_directive_t*
  (name-ctype 'yaml_tag_directive_t* (cpointer yaml_tag_directive_t)))
(define-public struct-yaml_tag_directive_s
  (name-ctype 'struct-yaml_tag_directive_s yaml_tag_directive_t))
(define-public struct-yaml_tag_directive_s*
  (name-ctype 'struct-yaml_tag_directive_s* yaml_tag_directive_t*))

;; typedef enum yaml_encoding_e {
;;   YAML_ANY_ENCODING,
;;   YAML_UTF8_ENCODING,
;;   YAML_UTF16LE_ENCODING,
;;   YAML_UTF16BE_ENCODING,
;; } yaml_encoding_t;
(define yaml_encoding_t-alist
  '((YAML_ANY_ENCODING 0)
    (YAML_UTF8_ENCODING 1)
    (YAML_UTF16LE_ENCODING 2)
    (YAML_UTF16BE_ENCODING 3)))
(define-public yaml_encoding_t
  (name-ctype 'yaml_encoding_t (cenum yaml_encoding_t-alist)))
(define-public unwrap-yaml_encoding_t
  (lambda (arg) (or (assq-ref yaml_encoding_t-alist arg) arg)))
(define-public wrap-yaml_encoding_t
  (let ((ral (rev-alist yaml_encoding_t-alist)))
    (lambda (arg) (or (assq-ref ral arg) arg))))

;; typedef enum yaml_break_e {
;;   YAML_ANY_BREAK,
;;   YAML_CR_BREAK,
;;   YAML_LN_BREAK,
;;   YAML_CRLN_BREAK,
;; } yaml_break_t;
(define yaml_break_t-alist
  '((YAML_ANY_BREAK 0) (YAML_CR_BREAK 1) (YAML_LN_BREAK 2) (YAML_CRLN_BREAK 3)))
(define-public yaml_break_t
  (name-ctype 'yaml_break_t (cenum yaml_break_t-alist)))
(define-public unwrap-yaml_break_t
  (lambda (arg) (or (assq-ref yaml_break_t-alist arg) arg)))
(define-public wrap-yaml_break_t
  (let ((ral (rev-alist yaml_break_t-alist)))
    (lambda (arg) (or (assq-ref ral arg) arg))))

;; typedef enum yaml_error_type_e {
;;   YAML_NO_ERROR,
;;   YAML_MEMORY_ERROR,
;;   YAML_READER_ERROR,
;;   YAML_SCANNER_ERROR,
;;   YAML_PARSER_ERROR,
;;   YAML_COMPOSER_ERROR,
;;   YAML_WRITER_ERROR,
;;   YAML_EMITTER_ERROR,
;; } yaml_error_type_t;
(define yaml_error_type_t-alist
  '((YAML_NO_ERROR 0)
    (YAML_MEMORY_ERROR 1)
    (YAML_READER_ERROR 2)
    (YAML_SCANNER_ERROR 3)
    (YAML_PARSER_ERROR 4)
    (YAML_COMPOSER_ERROR 5)
    (YAML_WRITER_ERROR 6)
    (YAML_EMITTER_ERROR 7)))
(define-public yaml_error_type_t
  (name-ctype 'yaml_error_type_t (cenum yaml_error_type_t-alist)))
(define-public unwrap-yaml_error_type_t
  (lambda (arg) (or (assq-ref yaml_error_type_t-alist arg) arg)))
(define-public wrap-yaml_error_type_t
  (let ((ral (rev-alist yaml_error_type_t-alist)))
    (lambda (arg) (or (assq-ref ral arg) arg))))

;; typedef struct yaml_mark_s {
;;   /** The position index. */
;;   size_t index;
;;   /** The position line. */
;;   size_t line;
;;   /** The position column. */
;;   size_t column;
;; } yaml_mark_t;
(define-public yaml_mark_t
  (name-ctype
   'yaml_mark_t
   (cstruct
    (list `(index ,(cbase 'size_t))
          `(line ,(cbase 'size_t))
          `(column ,(cbase 'size_t))))))
(define-public yaml_mark_t* (name-ctype 'yaml_mark_t* (cpointer yaml_mark_t)))
(define-public struct-yaml_mark_s (name-ctype 'struct-yaml_mark_s yaml_mark_t))
(define-public struct-yaml_mark_s*
  (name-ctype 'struct-yaml_mark_s* yaml_mark_t*))

;; typedef enum yaml_scalar_style_e {
;;   YAML_ANY_SCALAR_STYLE,
;;   YAML_PLAIN_SCALAR_STYLE,
;;   YAML_SINGLE_QUOTED_SCALAR_STYLE,
;;   YAML_DOUBLE_QUOTED_SCALAR_STYLE,
;;   YAML_LITERAL_SCALAR_STYLE,
;;   YAML_FOLDED_SCALAR_STYLE,
;; } yaml_scalar_style_t;
(define yaml_scalar_style_t-alist
  '((YAML_ANY_SCALAR_STYLE 0)
    (YAML_PLAIN_SCALAR_STYLE 1)
    (YAML_SINGLE_QUOTED_SCALAR_STYLE 2)
    (YAML_DOUBLE_QUOTED_SCALAR_STYLE 3)
    (YAML_LITERAL_SCALAR_STYLE 4)
    (YAML_FOLDED_SCALAR_STYLE 5)))
(define-public yaml_scalar_style_t
  (name-ctype 'yaml_scalar_style_t (cenum yaml_scalar_style_t-alist)))
(define-public unwrap-yaml_scalar_style_t
  (lambda (arg) (or (assq-ref yaml_scalar_style_t-alist arg) arg)))
(define-public wrap-yaml_scalar_style_t
  (let ((ral (rev-alist yaml_scalar_style_t-alist)))
    (lambda (arg) (or (assq-ref ral arg) arg))))

;; typedef enum yaml_sequence_style_e {
;;   YAML_ANY_SEQUENCE_STYLE,
;;   YAML_BLOCK_SEQUENCE_STYLE,
;;   YAML_FLOW_SEQUENCE_STYLE,
;; } yaml_sequence_style_t;
(define yaml_sequence_style_t-alist
  '((YAML_ANY_SEQUENCE_STYLE 0)
    (YAML_BLOCK_SEQUENCE_STYLE 1)
    (YAML_FLOW_SEQUENCE_STYLE 2)))
(define-public yaml_sequence_style_t
  (name-ctype 'yaml_sequence_style_t (cenum yaml_sequence_style_t-alist)))
(define-public unwrap-yaml_sequence_style_t
  (lambda (arg) (or (assq-ref yaml_sequence_style_t-alist arg) arg)))
(define-public wrap-yaml_sequence_style_t
  (let ((ral (rev-alist yaml_sequence_style_t-alist)))
    (lambda (arg) (or (assq-ref ral arg) arg))))

;; typedef enum yaml_mapping_style_e {
;;   YAML_ANY_MAPPING_STYLE,
;;   YAML_BLOCK_MAPPING_STYLE,
;;   YAML_FLOW_MAPPING_STYLE,
;; } yaml_mapping_style_t;
(define yaml_mapping_style_t-alist
  '((YAML_ANY_MAPPING_STYLE 0)
    (YAML_BLOCK_MAPPING_STYLE 1)
    (YAML_FLOW_MAPPING_STYLE 2)))
(define-public yaml_mapping_style_t
  (name-ctype 'yaml_mapping_style_t (cenum yaml_mapping_style_t-alist)))
(define-public unwrap-yaml_mapping_style_t
  (lambda (arg) (or (assq-ref yaml_mapping_style_t-alist arg) arg)))
(define-public wrap-yaml_mapping_style_t
  (let ((ral (rev-alist yaml_mapping_style_t-alist)))
    (lambda (arg) (or (assq-ref ral arg) arg))))

;; typedef enum yaml_token_type_e {
;;   YAML_NO_TOKEN,
;;   YAML_STREAM_START_TOKEN,
;;   YAML_STREAM_END_TOKEN,
;;   YAML_VERSION_DIRECTIVE_TOKEN,
;;   YAML_TAG_DIRECTIVE_TOKEN,
;;   YAML_DOCUMENT_START_TOKEN,
;;   YAML_DOCUMENT_END_TOKEN,
;;   YAML_BLOCK_SEQUENCE_START_TOKEN,
;;   YAML_BLOCK_MAPPING_START_TOKEN,
;;   YAML_BLOCK_END_TOKEN,
;;   YAML_FLOW_SEQUENCE_START_TOKEN,
;;   YAML_FLOW_SEQUENCE_END_TOKEN,
;;   YAML_FLOW_MAPPING_START_TOKEN,
;;   YAML_FLOW_MAPPING_END_TOKEN,
;;   YAML_BLOCK_ENTRY_TOKEN,
;;   YAML_FLOW_ENTRY_TOKEN,
;;   YAML_KEY_TOKEN,
;;   YAML_VALUE_TOKEN,
;;   YAML_ALIAS_TOKEN,
;;   YAML_ANCHOR_TOKEN,
;;   YAML_TAG_TOKEN,
;;   YAML_SCALAR_TOKEN,
;; } yaml_token_type_t;
(define yaml_token_type_t-alist
  '((YAML_NO_TOKEN 0)
    (YAML_STREAM_START_TOKEN 1)
    (YAML_STREAM_END_TOKEN 2)
    (YAML_VERSION_DIRECTIVE_TOKEN 3)
    (YAML_TAG_DIRECTIVE_TOKEN 4)
    (YAML_DOCUMENT_START_TOKEN 5)
    (YAML_DOCUMENT_END_TOKEN 6)
    (YAML_BLOCK_SEQUENCE_START_TOKEN 7)
    (YAML_BLOCK_MAPPING_START_TOKEN 8)
    (YAML_BLOCK_END_TOKEN 9)
    (YAML_FLOW_SEQUENCE_START_TOKEN 10)
    (YAML_FLOW_SEQUENCE_END_TOKEN 11)
    (YAML_FLOW_MAPPING_START_TOKEN 12)
    (YAML_FLOW_MAPPING_END_TOKEN 13)
    (YAML_BLOCK_ENTRY_TOKEN 14)
    (YAML_FLOW_ENTRY_TOKEN 15)
    (YAML_KEY_TOKEN 16)
    (YAML_VALUE_TOKEN 17)
    (YAML_ALIAS_TOKEN 18)
    (YAML_ANCHOR_TOKEN 19)
    (YAML_TAG_TOKEN 20)
    (YAML_SCALAR_TOKEN 21)))
(define-public yaml_token_type_t
  (name-ctype 'yaml_token_type_t (cenum yaml_token_type_t-alist)))
(define-public unwrap-yaml_token_type_t
  (lambda (arg) (or (assq-ref yaml_token_type_t-alist arg) arg)))
(define-public wrap-yaml_token_type_t
  (let ((ral (rev-alist yaml_token_type_t-alist)))
    (lambda (arg) (or (assq-ref ral arg) arg))))

;; typedef struct yaml_token_s {
;;   /** The token type. */
;;   yaml_token_type_t type;
;;   /** The token data. */
;;   union {
;;     /** The stream start (for @c YAML_STREAM_START_TOKEN). */
;;     struct {
;;       /** The stream encoding. */
;;       yaml_encoding_t encoding;
;;     } stream_start;
;;     /** The alias (for @c YAML_ALIAS_TOKEN). */
;;     struct {
;;       /** The alias value. */
;;       yaml_char_t *value;
;;     } alias;
;;     /** The anchor (for @c YAML_ANCHOR_TOKEN). */
;;     struct {
;;       /** The anchor value. */
;;       yaml_char_t *value;
;;     } anchor;
;;     /** The tag (for @c YAML_TAG_TOKEN). */
;;     struct {
;;       /** The tag handle. */
;;       yaml_char_t *handle;
;;       /** The tag suffix. */
;;       yaml_char_t *suffix;
;;     } tag;
;;     /** The scalar value (for @c YAML_SCALAR_TOKEN). */
;;     struct {
;;       /** The scalar value. */
;;       yaml_char_t *value;
;;       /** The length of the scalar value. */
;;       size_t length;
;;       /** The scalar style. */
;;       yaml_scalar_style_t style;
;;     } scalar;
;;     /** The version directive (for @c YAML_VERSION_DIRECTIVE_TOKEN). */
;;     struct {
;;       /** The major version number. */
;;       int major;
;;       /** The minor version number. */
;;       int minor;
;;     } version_directive;
;;     /** The tag directive (for @c YAML_TAG_DIRECTIVE_TOKEN). */
;;     struct {
;;       /** The tag handle. */
;;       yaml_char_t *handle;
;;       /** The tag prefix. */
;;       yaml_char_t *prefix;
;;     } tag_directive;
;;   } data;
;;   /** The beginning of the token. */
;;   yaml_mark_t start_mark;
;;   /** The end of the token. */
;;   yaml_mark_t end_mark;
;; } yaml_token_t;
(define-public yaml_token_t
  (name-ctype
   'yaml_token_t
   (cstruct
    (list `(type ,yaml_token_type_t)
          `(data ,(cunion
                   (list `(stream_start
                           ,(cstruct (list `(encoding ,yaml_encoding_t))))
                         `(alias ,(cstruct
                                   (list `(value ,(cpointer
                                                   (delay yaml_char_t))))))
                         `(anchor
                           ,(cstruct
                             (list `(value ,(cpointer (delay yaml_char_t))))))
                         `(tag ,(cstruct
                                 (list `(handle
                                         ,(cpointer (delay yaml_char_t)))
                                       `(suffix
                                         ,(cpointer (delay yaml_char_t))))))
                         `(scalar
                           ,(cstruct
                             (list `(value ,(cpointer (delay yaml_char_t)))
                                   `(length ,(cbase 'size_t))
                                   `(style ,yaml_scalar_style_t))))
                         `(version_directive
                           ,(cstruct
                             (list `(major ,(cbase 'int))
                                   `(minor ,(cbase 'int)))))
                         `(tag_directive
                           ,(cstruct
                             (list `(handle ,(cpointer (delay yaml_char_t)))
                                   `(prefix ,(cpointer (delay yaml_char_t)))))))))
          `(start_mark ,yaml_mark_t)
          `(end_mark ,yaml_mark_t)))))
(define-public yaml_token_t*
  (name-ctype 'yaml_token_t* (cpointer yaml_token_t)))
(define-public struct-yaml_token_s
  (name-ctype 'struct-yaml_token_s yaml_token_t))
(define-public struct-yaml_token_s*
  (name-ctype 'struct-yaml_token_s* yaml_token_t*))

;; void yaml_token_delete(yaml_token_t *token);
(define-public yaml_token_delete
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:void
                       (foreign-pointer-search "yaml_token_delete")
                       (list '*)))))
    (lambda (token)
      (let ((token (arg->pointer token yaml_token_t*))) ((force ~proc) token)))))

;; typedef enum yaml_event_type_e {
;;   YAML_NO_EVENT,
;;   YAML_STREAM_START_EVENT,
;;   YAML_STREAM_END_EVENT,
;;   YAML_DOCUMENT_START_EVENT,
;;   YAML_DOCUMENT_END_EVENT,
;;   YAML_ALIAS_EVENT,
;;   YAML_SCALAR_EVENT,
;;   YAML_SEQUENCE_START_EVENT,
;;   YAML_SEQUENCE_END_EVENT,
;;   YAML_MAPPING_START_EVENT,
;;   YAML_MAPPING_END_EVENT,
;; } yaml_event_type_t;
(define yaml_event_type_t-alist
  '((YAML_NO_EVENT 0)
    (YAML_STREAM_START_EVENT 1)
    (YAML_STREAM_END_EVENT 2)
    (YAML_DOCUMENT_START_EVENT 3)
    (YAML_DOCUMENT_END_EVENT 4)
    (YAML_ALIAS_EVENT 5)
    (YAML_SCALAR_EVENT 6)
    (YAML_SEQUENCE_START_EVENT 7)
    (YAML_SEQUENCE_END_EVENT 8)
    (YAML_MAPPING_START_EVENT 9)
    (YAML_MAPPING_END_EVENT 10)))
(define-public yaml_event_type_t
  (name-ctype 'yaml_event_type_t (cenum yaml_event_type_t-alist)))
(define-public unwrap-yaml_event_type_t
  (lambda (arg) (or (assq-ref yaml_event_type_t-alist arg) arg)))
(define-public wrap-yaml_event_type_t
  (let ((ral (rev-alist yaml_event_type_t-alist)))
    (lambda (arg) (or (assq-ref ral arg) arg))))

;; typedef struct yaml_event_s {
;;   /** The event type. */
;;   yaml_event_type_t type;
;;   /** The event data. */
;;   union {
;;     /** The stream parameters (for @c YAML_STREAM_START_EVENT). */
;;     struct {
;;       /** The document encoding. */
;;       yaml_encoding_t encoding;
;;     } stream_start;
;;     /** The document parameters (for @c YAML_DOCUMENT_START_EVENT). */
;;     struct {
;;       /** The version directive. */
;;       yaml_version_directive_t *version_directive;
;;       /** The list of tag directives. */
;;       struct {
;;         /** The beginning of the tag directives list. */
;;         yaml_tag_directive_t *start;
;;         /** The end of the tag directives list. */
;;         yaml_tag_directive_t *end;
;;       } tag_directives;
;;       /** Is the document indicator implicit? */
;;       int implicit;
;;     } document_start;
;;     /** The document end parameters (for @c YAML_DOCUMENT_END_EVENT). */
;;     struct {
;;       /** Is the document end indicator implicit? */
;;       int implicit;
;;     } document_end;
;;     /** The alias parameters (for @c YAML_ALIAS_EVENT). */
;;     struct {
;;       /** The anchor. */
;;       yaml_char_t *anchor;
;;     } alias;
;;     /** The scalar parameters (for @c YAML_SCALAR_EVENT). */
;;     struct {
;;       /** The anchor. */
;;       yaml_char_t *anchor;
;;       /** The tag. */
;;       yaml_char_t *tag;
;;       /** The scalar value. */
;;       yaml_char_t *value;
;;       /** The length of the scalar value. */
;;       size_t length;
;;       /** Is the tag optional for the plain style? */
;;       int plain_implicit;
;;       /** Is the tag optional for any non-plain style? */
;;       int quoted_implicit;
;;       /** The scalar style. */
;;       yaml_scalar_style_t style;
;;     } scalar;
;;     /** The sequence parameters (for @c YAML_SEQUENCE_START_EVENT). */
;;     struct {
;;       /** The anchor. */
;;       yaml_char_t *anchor;
;;       /** The tag. */
;;       yaml_char_t *tag;
;;       /** Is the tag optional? */
;;       int implicit;
;;       /** The sequence style. */
;;       yaml_sequence_style_t style;
;;     } sequence_start;
;;     /** The mapping parameters (for @c YAML_MAPPING_START_EVENT). */
;;     struct {
;;       /** The anchor. */
;;       yaml_char_t *anchor;
;;       /** The tag. */
;;       yaml_char_t *tag;
;;       /** Is the tag optional? */
;;       int implicit;
;;       /** The mapping style. */
;;       yaml_mapping_style_t style;
;;     } mapping_start;
;;   } data;
;;   /** The beginning of the event. */
;;   yaml_mark_t start_mark;
;;   /** The end of the event. */
;;   yaml_mark_t end_mark;
;; } yaml_event_t;
(define-public yaml_event_t
  (name-ctype
   'yaml_event_t
   (cstruct
    (list `(type ,yaml_event_type_t)
          `(data ,(cunion
                   (list `(stream_start
                           ,(cstruct (list `(encoding ,yaml_encoding_t))))
                         `(document_start
                           ,(cstruct
                             (list `(version_directive
                                     ,yaml_version_directive_t*)
                                   `(tag_directives
                                     ,(cstruct
                                       (list `(start ,yaml_tag_directive_t*)
                                             `(end ,yaml_tag_directive_t*))))
                                   `(implicit ,(cbase 'int)))))
                         `(document_end
                           ,(cstruct (list `(implicit ,(cbase 'int)))))
                         `(alias ,(cstruct
                                   (list `(anchor
                                           ,(cpointer (delay yaml_char_t))))))
                         `(scalar
                           ,(cstruct
                             (list `(anchor ,(cpointer (delay yaml_char_t)))
                                   `(tag ,(cpointer (delay yaml_char_t)))
                                   `(value ,(cpointer (delay yaml_char_t)))
                                   `(length ,(cbase 'size_t))
                                   `(plain_implicit ,(cbase 'int))
                                   `(quoted_implicit ,(cbase 'int))
                                   `(style ,yaml_scalar_style_t))))
                         `(sequence_start
                           ,(cstruct
                             (list `(anchor ,(cpointer (delay yaml_char_t)))
                                   `(tag ,(cpointer (delay yaml_char_t)))
                                   `(implicit ,(cbase 'int))
                                   `(style ,yaml_sequence_style_t))))
                         `(mapping_start
                           ,(cstruct
                             (list `(anchor ,(cpointer (delay yaml_char_t)))
                                   `(tag ,(cpointer (delay yaml_char_t)))
                                   `(implicit ,(cbase 'int))
                                   `(style ,yaml_mapping_style_t)))))))
          `(start_mark ,yaml_mark_t)
          `(end_mark ,yaml_mark_t)))))
(define-public yaml_event_t*
  (name-ctype 'yaml_event_t* (cpointer yaml_event_t)))
(define-public struct-yaml_event_s
  (name-ctype 'struct-yaml_event_s yaml_event_t))
(define-public struct-yaml_event_s*
  (name-ctype 'struct-yaml_event_s* yaml_event_t*))

;; int yaml_stream_start_event_initialize(yaml_event_t *event, yaml_encoding_t 
;;     encoding);
(define-public yaml_stream_start_event_initialize
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search
                        "yaml_stream_start_event_initialize")
                       (list '* ffi:int)))))
    (lambda (event encoding)
      (let ((event (arg->pointer event yaml_event_t*))
            (encoding (unwrap-enum encoding)))
        ((force ~proc) event encoding)))))

;; int yaml_stream_end_event_initialize(yaml_event_t *event);
(define-public yaml_stream_end_event_initialize
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search
                        "yaml_stream_end_event_initialize")
                       (list '*)))))
    (lambda (event)
      (let ((event (arg->pointer event yaml_event_t*))) ((force ~proc) event)))))

;; int yaml_document_start_event_initialize(yaml_event_t *event, 
;;     yaml_version_directive_t *version_directive, yaml_tag_directive_t *
;;     tag_directives_start, yaml_tag_directive_t *tag_directives_end, int 
;;     implicit);
(define-public yaml_document_start_event_initialize
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search
                        "yaml_document_start_event_initialize")
                       (list '* '* '* '* ffi:int)))))
    (lambda (event
             version_directive
             tag_directives_start
             tag_directives_end
             implicit)
      (let ((event (arg->pointer event yaml_event_t*))
            (version_directive
             (arg->pointer version_directive yaml_version_directive_t*))
            (tag_directives_start
             (arg->pointer tag_directives_start yaml_tag_directive_t*))
            (tag_directives_end
             (arg->pointer tag_directives_end yaml_tag_directive_t*))
            (implicit (arg->number implicit)))
        ((force ~proc)
         event
         version_directive
         tag_directives_start
         tag_directives_end
         implicit)))))

;; int yaml_document_end_event_initialize(yaml_event_t *event, int implicit);
(define-public yaml_document_end_event_initialize
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search
                        "yaml_document_end_event_initialize")
                       (list '* ffi:int)))))
    (lambda (event implicit)
      (let ((event (arg->pointer event yaml_event_t*))
            (implicit (arg->number implicit)))
        ((force ~proc) event implicit)))))

;; int yaml_alias_event_initialize(yaml_event_t *event, const yaml_char_t *
;;     anchor);
(define-public yaml_alias_event_initialize
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search "yaml_alias_event_initialize")
                       (list '* '*)))))
    (lambda (event anchor)
      (let ((event (arg->pointer event yaml_event_t*))
            (anchor (arg->pointer anchor (cpointer yaml_char_t))))
        ((force ~proc) event anchor)))))

;; int yaml_scalar_event_initialize(yaml_event_t *event, const yaml_char_t *
;;     anchor, const yaml_char_t *tag, const yaml_char_t *value, int length, 
;;     int plain_implicit, int quoted_implicit, yaml_scalar_style_t style);
(define-public yaml_scalar_event_initialize
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search "yaml_scalar_event_initialize")
                       (list '* '* '* '* ffi:int ffi:int ffi:int ffi:int)))))
    (lambda (event
             anchor
             tag
             value
             length
             plain_implicit
             quoted_implicit
             style)
      (let ((event (arg->pointer event yaml_event_t*))
            (anchor (arg->pointer anchor (cpointer yaml_char_t)))
            (tag (arg->pointer tag (cpointer yaml_char_t)))
            (value (arg->pointer value (cpointer yaml_char_t)))
            (length (arg->number length))
            (plain_implicit (arg->number plain_implicit))
            (quoted_implicit (arg->number quoted_implicit))
            (style (unwrap-enum style)))
        ((force ~proc)
         event
         anchor
         tag
         value
         length
         plain_implicit
         quoted_implicit
         style)))))

;; int yaml_sequence_start_event_initialize(yaml_event_t *event, const 
;;     yaml_char_t *anchor, const yaml_char_t *tag, int implicit, 
;;     yaml_sequence_style_t style);
(define-public yaml_sequence_start_event_initialize
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search
                        "yaml_sequence_start_event_initialize")
                       (list '* '* '* ffi:int ffi:int)))))
    (lambda (event anchor tag implicit style)
      (let ((event (arg->pointer event yaml_event_t*))
            (anchor (arg->pointer anchor (cpointer yaml_char_t)))
            (tag (arg->pointer tag (cpointer yaml_char_t)))
            (implicit (arg->number implicit))
            (style (unwrap-enum style)))
        ((force ~proc) event anchor tag implicit style)))))

;; int yaml_sequence_end_event_initialize(yaml_event_t *event);
(define-public yaml_sequence_end_event_initialize
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search
                        "yaml_sequence_end_event_initialize")
                       (list '*)))))
    (lambda (event)
      (let ((event (arg->pointer event yaml_event_t*))) ((force ~proc) event)))))

;; int yaml_mapping_start_event_initialize(yaml_event_t *event, const 
;;     yaml_char_t *anchor, const yaml_char_t *tag, int implicit, 
;;     yaml_mapping_style_t style);
(define-public yaml_mapping_start_event_initialize
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search
                        "yaml_mapping_start_event_initialize")
                       (list '* '* '* ffi:int ffi:int)))))
    (lambda (event anchor tag implicit style)
      (let ((event (arg->pointer event yaml_event_t*))
            (anchor (arg->pointer anchor (cpointer yaml_char_t)))
            (tag (arg->pointer tag (cpointer yaml_char_t)))
            (implicit (arg->number implicit))
            (style (unwrap-enum style)))
        ((force ~proc) event anchor tag implicit style)))))

;; int yaml_mapping_end_event_initialize(yaml_event_t *event);
(define-public yaml_mapping_end_event_initialize
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search
                        "yaml_mapping_end_event_initialize")
                       (list '*)))))
    (lambda (event)
      (let ((event (arg->pointer event yaml_event_t*))) ((force ~proc) event)))))

;; void yaml_event_delete(yaml_event_t *event);
(define-public yaml_event_delete
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:void
                       (foreign-pointer-search "yaml_event_delete")
                       (list '*)))))
    (lambda (event)
      (let ((event (arg->pointer event yaml_event_t*))) ((force ~proc) event)))))

;; typedef enum yaml_node_type_e {
;;   YAML_NO_NODE,
;;   YAML_SCALAR_NODE,
;;   YAML_SEQUENCE_NODE,
;;   YAML_MAPPING_NODE,
;; } yaml_node_type_t;
(define yaml_node_type_t-alist
  '((YAML_NO_NODE 0)
    (YAML_SCALAR_NODE 1)
    (YAML_SEQUENCE_NODE 2)
    (YAML_MAPPING_NODE 3)))
(define-public yaml_node_type_t
  (name-ctype 'yaml_node_type_t (cenum yaml_node_type_t-alist)))
(define-public unwrap-yaml_node_type_t
  (lambda (arg) (or (assq-ref yaml_node_type_t-alist arg) arg)))
(define-public wrap-yaml_node_type_t
  (let ((ral (rev-alist yaml_node_type_t-alist)))
    (lambda (arg) (or (assq-ref ral arg) arg))))

;; typedef struct yaml_node_s yaml_node_t;
(define-public yaml_node_t*
  (name-ctype 'yaml_node_t* (cpointer (delay yaml_node_t))))

;; typedef int yaml_node_item_t;
(define-public yaml_node_item_t (name-ctype 'yaml_node_item_t (cbase 'int)))

;; typedef struct yaml_node_pair_s {
;;   /** The key of the element. */
;;   int key;
;;   /** The value of the element. */
;;   int value;
;; } yaml_node_pair_t;
(define-public yaml_node_pair_t
  (name-ctype
   'yaml_node_pair_t
   (cstruct (list `(key ,(cbase 'int)) `(value ,(cbase 'int))))))
(define-public yaml_node_pair_t*
  (name-ctype 'yaml_node_pair_t* (cpointer yaml_node_pair_t)))
(define-public struct-yaml_node_pair_s
  (name-ctype 'struct-yaml_node_pair_s yaml_node_pair_t))
(define-public struct-yaml_node_pair_s*
  (name-ctype 'struct-yaml_node_pair_s* yaml_node_pair_t*))

;; struct yaml_node_s {
;;   /** The node type. */
;;   yaml_node_type_t type;
;;   /** The node tag. */
;;   yaml_char_t *tag;
;;   /** The node data. */
;;   union {
;;     /** The scalar parameters (for @c YAML_SCALAR_NODE). */
;;     struct {
;;       /** The scalar value. */
;;       yaml_char_t *value;
;;       /** The length of the scalar value. */
;;       size_t length;
;;       /** The scalar style. */
;;       yaml_scalar_style_t style;
;;     } scalar;
;;     /** The sequence parameters (for @c YAML_SEQUENCE_NODE). */
;;     struct {
;;       /** The stack of sequence items. */
;;       struct {
;;         /** The beginning of the stack. */
;;         yaml_node_item_t *start;
;;         /** The end of the stack. */
;;         yaml_node_item_t *end;
;;         /** The top of the stack. */
;;         yaml_node_item_t *top;
;;       } items;
;;       /** The sequence style. */
;;       yaml_sequence_style_t style;
;;     } sequence;
;;     /** The mapping parameters (for @c YAML_MAPPING_NODE). */
;;     struct {
;;       /** The stack of mapping pairs (key, value). */
;;       struct {
;;         /** The beginning of the stack. */
;;         yaml_node_pair_t *start;
;;         /** The end of the stack. */
;;         yaml_node_pair_t *end;
;;         /** The top of the stack. */
;;         yaml_node_pair_t *top;
;;       } pairs;
;;       /** The mapping style. */
;;       yaml_mapping_style_t style;
;;     } mapping;
;;   } data;
;;   /** The beginning of the node. */
;;   yaml_mark_t start_mark;
;;   /** The end of the node. */
;;   yaml_mark_t end_mark;
;; };
(define-public struct-yaml_node_s
  (name-ctype
   'struct-yaml_node_s
   (cstruct
    (list `(type ,yaml_node_type_t)
          `(tag ,(cpointer (delay yaml_char_t)))
          `(data ,(cunion
                   (list `(scalar
                           ,(cstruct
                             (list `(value ,(cpointer (delay yaml_char_t)))
                                   `(length ,(cbase 'size_t))
                                   `(style ,yaml_scalar_style_t))))
                         `(sequence
                           ,(cstruct
                             (list `(items ,(cstruct
                                             (list `(start ,(cpointer
                                                             (delay yaml_node_item_t)))
                                                   `(end ,(cpointer
                                                           (delay yaml_node_item_t)))
                                                   `(top ,(cpointer
                                                           (delay yaml_node_item_t))))))
                                   `(style ,yaml_sequence_style_t))))
                         `(mapping
                           ,(cstruct
                             (list `(pairs ,(cstruct
                                             (list `(start ,yaml_node_pair_t*)
                                                   `(end ,yaml_node_pair_t*)
                                                   `(top ,yaml_node_pair_t*))))
                                   `(style ,yaml_mapping_style_t)))))))
          `(start_mark ,yaml_mark_t)
          `(end_mark ,yaml_mark_t)))))
(define-public struct-yaml_node_s*
  (name-ctype 'struct-yaml_node_s* (cpointer struct-yaml_node_s)))
(define-public yaml_node_t (name-ctype 'yaml_node_t struct-yaml_node_s))

;; typedef struct yaml_document_s {
;;   /** The document nodes. */
;;   struct {
;;     /** The beginning of the stack. */
;;     yaml_node_t *start;
;;     /** The end of the stack. */
;;     yaml_node_t *end;
;;     /** The top of the stack. */
;;     yaml_node_t *top;
;;   } nodes;
;;   /** The version directive. */
;;   yaml_version_directive_t *version_directive;
;;   /** The list of tag directives. */
;;   struct {
;;     /** The beginning of the tag directives list. */
;;     yaml_tag_directive_t *start;
;;     /** The end of the tag directives list. */
;;     yaml_tag_directive_t *end;
;;   } tag_directives;
;;   /** Is the document start indicator implicit? */
;;   int start_implicit;
;;   /** Is the document end indicator implicit? */
;;   int end_implicit;
;;   /** The beginning of the document. */
;;   yaml_mark_t start_mark;
;;   /** The end of the document. */
;;   yaml_mark_t end_mark;
;; } yaml_document_t;
(define-public yaml_document_t
  (name-ctype
   'yaml_document_t
   (cstruct
    (list `(nodes ,(cstruct
                    (list `(start ,yaml_node_t*)
                          `(end ,yaml_node_t*)
                          `(top ,yaml_node_t*))))
          `(version_directive ,yaml_version_directive_t*)
          `(tag_directives
            ,(cstruct
              (list `(start ,yaml_tag_directive_t*)
                    `(end ,yaml_tag_directive_t*))))
          `(start_implicit ,(cbase 'int))
          `(end_implicit ,(cbase 'int))
          `(start_mark ,yaml_mark_t)
          `(end_mark ,yaml_mark_t)))))
(define-public yaml_document_t*
  (name-ctype 'yaml_document_t* (cpointer yaml_document_t)))
(define-public struct-yaml_document_s
  (name-ctype 'struct-yaml_document_s yaml_document_t))
(define-public struct-yaml_document_s*
  (name-ctype 'struct-yaml_document_s* yaml_document_t*))

;; int yaml_document_initialize(yaml_document_t *document, 
;;     yaml_version_directive_t *version_directive, yaml_tag_directive_t *
;;     tag_directives_start, yaml_tag_directive_t *tag_directives_end, int 
;;     start_implicit, int end_implicit);
(define-public yaml_document_initialize
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search "yaml_document_initialize")
                       (list '* '* '* '* ffi:int ffi:int)))))
    (lambda (document
             version_directive
             tag_directives_start
             tag_directives_end
             start_implicit
             end_implicit)
      (let ((document (arg->pointer document yaml_document_t*))
            (version_directive
             (arg->pointer version_directive yaml_version_directive_t*))
            (tag_directives_start
             (arg->pointer tag_directives_start yaml_tag_directive_t*))
            (tag_directives_end
             (arg->pointer tag_directives_end yaml_tag_directive_t*))
            (start_implicit (arg->number start_implicit))
            (end_implicit (arg->number end_implicit)))
        ((force ~proc)
         document
         version_directive
         tag_directives_start
         tag_directives_end
         start_implicit
         end_implicit)))))

;; void yaml_document_delete(yaml_document_t *document);
(define-public yaml_document_delete
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:void
                       (foreign-pointer-search "yaml_document_delete")
                       (list '*)))))
    (lambda (document)
      (let ((document (arg->pointer document yaml_document_t*)))
        ((force ~proc) document)))))

;; yaml_node_t *yaml_document_get_node(yaml_document_t *document, int index);
(define-public yaml_document_get_node
  (let ((~proc (delay (ffi:pointer->procedure
                       '*
                       (foreign-pointer-search "yaml_document_get_node")
                       (list '* ffi:int)))))
    (lambda (document index)
      (let ((document (arg->pointer document yaml_document_t*))
            (index (arg->number index)))
        ((lambda (~ret) (make-cdata yaml_node_t* ~ret))
         ((force ~proc) document index))))))

;; yaml_node_t *yaml_document_get_root_node(yaml_document_t *document);
(define-public yaml_document_get_root_node
  (let ((~proc (delay (ffi:pointer->procedure
                       '*
                       (foreign-pointer-search "yaml_document_get_root_node")
                       (list '*)))))
    (lambda (document)
      (let ((document (arg->pointer document yaml_document_t*)))
        ((lambda (~ret) (make-cdata yaml_node_t* ~ret))
         ((force ~proc) document))))))

;; int yaml_document_add_scalar(yaml_document_t *document, const yaml_char_t *
;;     tag, const yaml_char_t *value, int length, yaml_scalar_style_t style);
(define-public yaml_document_add_scalar
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search "yaml_document_add_scalar")
                       (list '* '* '* ffi:int ffi:int)))))
    (lambda (document tag value length style)
      (let ((document (arg->pointer document yaml_document_t*))
            (tag (arg->pointer tag (cpointer yaml_char_t)))
            (value (arg->pointer value (cpointer yaml_char_t)))
            (length (arg->number length))
            (style (unwrap-enum style)))
        ((force ~proc) document tag value length style)))))

;; int yaml_document_add_sequence(yaml_document_t *document, const yaml_char_t 
;;     *tag, yaml_sequence_style_t style);
(define-public yaml_document_add_sequence
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search "yaml_document_add_sequence")
                       (list '* '* ffi:int)))))
    (lambda (document tag style)
      (let ((document (arg->pointer document yaml_document_t*))
            (tag (arg->pointer tag (cpointer yaml_char_t)))
            (style (unwrap-enum style)))
        ((force ~proc) document tag style)))))

;; int yaml_document_add_mapping(yaml_document_t *document, const yaml_char_t *
;;     tag, yaml_mapping_style_t style);
(define-public yaml_document_add_mapping
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search "yaml_document_add_mapping")
                       (list '* '* ffi:int)))))
    (lambda (document tag style)
      (let ((document (arg->pointer document yaml_document_t*))
            (tag (arg->pointer tag (cpointer yaml_char_t)))
            (style (unwrap-enum style)))
        ((force ~proc) document tag style)))))

;; int yaml_document_append_sequence_item(yaml_document_t *document, int 
;;     sequence, int item);
(define-public yaml_document_append_sequence_item
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search
                        "yaml_document_append_sequence_item")
                       (list '* ffi:int ffi:int)))))
    (lambda (document sequence item)
      (let ((document (arg->pointer document yaml_document_t*))
            (sequence (arg->number sequence))
            (item (arg->number item)))
        ((force ~proc) document sequence item)))))

;; int yaml_document_append_mapping_pair(yaml_document_t *document, int mapping
;;     , int key, int value);
(define-public yaml_document_append_mapping_pair
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search
                        "yaml_document_append_mapping_pair")
                       (list '* ffi:int ffi:int ffi:int)))))
    (lambda (document mapping key value)
      (let ((document (arg->pointer document yaml_document_t*))
            (mapping (arg->number mapping))
            (key (arg->number key))
            (value (arg->number value)))
        ((force ~proc) document mapping key value)))))

;; typedef int yaml_read_handler_t(void *data, unsigned char *buffer, size_t 
;;     size, size_t *size_read);
(define-public yaml_read_handler_t
  (name-ctype
   'yaml_read_handler_t
   (cfunction
    (lambda (~proc)
      (ffi:procedure->pointer
       ffi:int
       (lambda (data buffer size size_read)
         ((lambda (~ret) (arg->number ~ret))
          (~proc data buffer size size_read)))
       (list '* '* ffi:size_t '*)))
    (lambda (~fptr)
      (let ((~proc (ffi:pointer->procedure
                    ffi:int
                    ~fptr
                    (list '* '* ffi:size_t '*))))
        (lambda (data buffer size size_read)
          (let ((data (arg->pointer data))
                (buffer (arg->pointer buffer))
                (size (arg->number size))
                (size_read (arg->pointer size_read)))
            (~proc data buffer size size_read))))))))
(define-public yaml_read_handler_t*
  (name-ctype 'yaml_read_handler_t* (cpointer yaml_read_handler_t)))

;; typedef struct yaml_simple_key_s {
;;   /** Is a simple key possible? */
;;   int possible;
;;   /** Is a simple key required? */
;;   int required;
;;   /** The number of the token. */
;;   size_t token_number;
;;   /** The position mark. */
;;   yaml_mark_t mark;
;; } yaml_simple_key_t;
(define-public yaml_simple_key_t
  (name-ctype
   'yaml_simple_key_t
   (cstruct
    (list `(possible ,(cbase 'int))
          `(required ,(cbase 'int))
          `(token_number ,(cbase 'size_t))
          `(mark ,yaml_mark_t)))))
(define-public yaml_simple_key_t*
  (name-ctype 'yaml_simple_key_t* (cpointer yaml_simple_key_t)))
(define-public struct-yaml_simple_key_s
  (name-ctype 'struct-yaml_simple_key_s yaml_simple_key_t))
(define-public struct-yaml_simple_key_s*
  (name-ctype 'struct-yaml_simple_key_s* yaml_simple_key_t*))

;; typedef enum yaml_parser_state_e {
;;   YAML_PARSE_STREAM_START_STATE,
;;   YAML_PARSE_IMPLICIT_DOCUMENT_START_STATE,
;;   YAML_PARSE_DOCUMENT_START_STATE,
;;   YAML_PARSE_DOCUMENT_CONTENT_STATE,
;;   YAML_PARSE_DOCUMENT_END_STATE,
;;   YAML_PARSE_BLOCK_NODE_STATE,
;;   YAML_PARSE_BLOCK_NODE_OR_INDENTLESS_SEQUENCE_STATE,
;;   YAML_PARSE_FLOW_NODE_STATE,
;;   YAML_PARSE_BLOCK_SEQUENCE_FIRST_ENTRY_STATE,
;;   YAML_PARSE_BLOCK_SEQUENCE_ENTRY_STATE,
;;   YAML_PARSE_INDENTLESS_SEQUENCE_ENTRY_STATE,
;;   YAML_PARSE_BLOCK_MAPPING_FIRST_KEY_STATE,
;;   YAML_PARSE_BLOCK_MAPPING_KEY_STATE,
;;   YAML_PARSE_BLOCK_MAPPING_VALUE_STATE,
;;   YAML_PARSE_FLOW_SEQUENCE_FIRST_ENTRY_STATE,
;;   YAML_PARSE_FLOW_SEQUENCE_ENTRY_STATE,
;;   YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_KEY_STATE,
;;   YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_VALUE_STATE,
;;   YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_END_STATE,
;;   YAML_PARSE_FLOW_MAPPING_FIRST_KEY_STATE,
;;   YAML_PARSE_FLOW_MAPPING_KEY_STATE,
;;   YAML_PARSE_FLOW_MAPPING_VALUE_STATE,
;;   YAML_PARSE_FLOW_MAPPING_EMPTY_VALUE_STATE,
;;   YAML_PARSE_END_STATE,
;; } yaml_parser_state_t;
(define yaml_parser_state_t-alist
  '((YAML_PARSE_STREAM_START_STATE 0)
    (YAML_PARSE_IMPLICIT_DOCUMENT_START_STATE 1)
    (YAML_PARSE_DOCUMENT_START_STATE 2)
    (YAML_PARSE_DOCUMENT_CONTENT_STATE 3)
    (YAML_PARSE_DOCUMENT_END_STATE 4)
    (YAML_PARSE_BLOCK_NODE_STATE 5)
    (YAML_PARSE_BLOCK_NODE_OR_INDENTLESS_SEQUENCE_STATE 6)
    (YAML_PARSE_FLOW_NODE_STATE 7)
    (YAML_PARSE_BLOCK_SEQUENCE_FIRST_ENTRY_STATE 8)
    (YAML_PARSE_BLOCK_SEQUENCE_ENTRY_STATE 9)
    (YAML_PARSE_INDENTLESS_SEQUENCE_ENTRY_STATE 10)
    (YAML_PARSE_BLOCK_MAPPING_FIRST_KEY_STATE 11)
    (YAML_PARSE_BLOCK_MAPPING_KEY_STATE 12)
    (YAML_PARSE_BLOCK_MAPPING_VALUE_STATE 13)
    (YAML_PARSE_FLOW_SEQUENCE_FIRST_ENTRY_STATE 14)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_STATE 15)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_KEY_STATE 16)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_VALUE_STATE 17)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_END_STATE 18)
    (YAML_PARSE_FLOW_MAPPING_FIRST_KEY_STATE 19)
    (YAML_PARSE_FLOW_MAPPING_KEY_STATE 20)
    (YAML_PARSE_FLOW_MAPPING_VALUE_STATE 21)
    (YAML_PARSE_FLOW_MAPPING_EMPTY_VALUE_STATE 22)
    (YAML_PARSE_END_STATE 23)))
(define-public yaml_parser_state_t
  (name-ctype 'yaml_parser_state_t (cenum yaml_parser_state_t-alist)))
(define-public unwrap-yaml_parser_state_t
  (lambda (arg) (or (assq-ref yaml_parser_state_t-alist arg) arg)))
(define-public wrap-yaml_parser_state_t
  (let ((ral (rev-alist yaml_parser_state_t-alist)))
    (lambda (arg) (or (assq-ref ral arg) arg))))

;; typedef struct yaml_alias_data_s {
;;   /** The anchor. */
;;   yaml_char_t *anchor;
;;   /** The node id. */
;;   int index;
;;   /** The anchor mark. */
;;   yaml_mark_t mark;
;; } yaml_alias_data_t;
(define-public yaml_alias_data_t
  (name-ctype
   'yaml_alias_data_t
   (cstruct
    (list `(anchor ,(cpointer (delay yaml_char_t)))
          `(index ,(cbase 'int))
          `(mark ,yaml_mark_t)))))
(define-public yaml_alias_data_t*
  (name-ctype 'yaml_alias_data_t* (cpointer yaml_alias_data_t)))
(define-public struct-yaml_alias_data_s
  (name-ctype 'struct-yaml_alias_data_s yaml_alias_data_t))
(define-public struct-yaml_alias_data_s*
  (name-ctype 'struct-yaml_alias_data_s* yaml_alias_data_t*))

;; typedef struct yaml_parser_s {
;;   /**
;;    * @name Error handling
;;    * @{
;;    */
;;   /** Error type. */
;;   yaml_error_type_t error;
;;   /** Error description. */
;;   const char *problem;
;;   /** The byte about which the problem occured. */
;;   size_t problem_offset;
;;   /** The problematic value (@c -1 is none). */
;;   int problem_value;
;;   /** The problem position. */
;;   yaml_mark_t problem_mark;
;;   /** The error context. */
;;   const char *context;
;;   /** The context position. */
;;   yaml_mark_t context_mark;
;;   /**
;;    * @}
;;    */
;;   /**
;;    * @name Reader stuff
;;    * @{
;;    */
;;   /** Read handler. */
;;   yaml_read_handler_t *read_handler;
;;   /** A pointer for passing to the read handler. */
;;   void *read_handler_data;
;;   /** Standard (string or file) input data. */
;;   union {
;;     /** String input data. */
;;     struct {
;;       /** The string start pointer. */
;;       const unsigned char *start;
;;       /** The string end pointer. */
;;       const unsigned char *end;
;;       /** The string current position. */
;;       const unsigned char *current;
;;     } string;
;;     /** File input data. */
;;     FILE *file;
;;   } input;
;;   /** EOF flag */
;;   int eof;
;;   /** The working buffer. */
;;   struct {
;;     /** The beginning of the buffer. */
;;     yaml_char_t *start;
;;     /** The end of the buffer. */
;;     yaml_char_t *end;
;;     /** The current position of the buffer. */
;;     yaml_char_t *pointer;
;;     /** The last filled position of the buffer. */
;;     yaml_char_t *last;
;;   } buffer;
;;   /* The number of unread characters in the buffer. */
;;   size_t unread;
;;   /** The raw buffer. */
;;   struct {
;;     /** The beginning of the buffer. */
;;     unsigned char *start;
;;     /** The end of the buffer. */
;;     unsigned char *end;
;;     /** The current position of the buffer. */
;;     unsigned char *pointer;
;;     /** The last filled position of the buffer. */
;;     unsigned char *last;
;;   } raw_buffer;
;;   /** The input encoding. */
;;   yaml_encoding_t encoding;
;;   /** The offset of the current position (in bytes). */
;;   size_t offset;
;;   /** The mark of the current position. */
;;   yaml_mark_t mark;
;;   /**
;;    * @}
;;    */
;;   /**
;;    * @name Scanner stuff
;;    * @{
;;    */
;;   /** Have we started to scan the input stream? */
;;   int stream_start_produced;
;;   /** Have we reached the end of the input stream? */
;;   int stream_end_produced;
;;   /** The number of unclosed '[' and '{' indicators. */
;;   int flow_level;
;;   /** The tokens queue. */
;;   struct {
;;     /** The beginning of the tokens queue. */
;;     yaml_token_t *start;
;;     /** The end of the tokens queue. */
;;     yaml_token_t *end;
;;     /** The head of the tokens queue. */
;;     yaml_token_t *head;
;;     /** The tail of the tokens queue. */
;;     yaml_token_t *tail;
;;   } tokens;
;;   /** The number of tokens fetched from the queue. */
;;   size_t tokens_parsed;
;;   /** Does the tokens queue contain a token ready for dequeueing. */
;;   int token_available;
;;   /** The indentation levels stack. */
;;   struct {
;;     /** The beginning of the stack. */
;;     int *start;
;;     /** The end of the stack. */
;;     int *end;
;;     /** The top of the stack. */
;;     int *top;
;;   } indents;
;;   /** The current indentation level. */
;;   int indent;
;;   /** May a simple key occur at the current position? */
;;   int simple_key_allowed;
;;   /** The stack of simple keys. */
;;   struct {
;;     /** The beginning of the stack. */
;;     yaml_simple_key_t *start;
;;     /** The end of the stack. */
;;     yaml_simple_key_t *end;
;;     /** The top of the stack. */
;;     yaml_simple_key_t *top;
;;   } simple_keys;
;;   /**
;;    * @}
;;    */
;;   /**
;;    * @name Parser stuff
;;    * @{
;;    */
;;   /** The parser states stack. */
;;   struct {
;;     /** The beginning of the stack. */
;;     yaml_parser_state_t *start;
;;     /** The end of the stack. */
;;     yaml_parser_state_t *end;
;;     /** The top of the stack. */
;;     yaml_parser_state_t *top;
;;   } states;
;;   /** The current parser state. */
;;   yaml_parser_state_t state;
;;   /** The stack of marks. */
;;   struct {
;;     /** The beginning of the stack. */
;;     yaml_mark_t *start;
;;     /** The end of the stack. */
;;     yaml_mark_t *end;
;;     /** The top of the stack. */
;;     yaml_mark_t *top;
;;   } marks;
;;   /** The list of TAG directives. */
;;   struct {
;;     /** The beginning of the list. */
;;     yaml_tag_directive_t *start;
;;     /** The end of the list. */
;;     yaml_tag_directive_t *end;
;;     /** The top of the list. */
;;     yaml_tag_directive_t *top;
;;   } tag_directives;
;;   /**
;;    * @}
;;    */
;;   /**
;;    * @name Dumper stuff
;;    * @{
;;    */
;;   /** The alias data. */
;;   struct {
;;     /** The beginning of the list. */
;;     yaml_alias_data_t *start;
;;     /** The end of the list. */
;;     yaml_alias_data_t *end;
;;     /** The top of the list. */
;;     yaml_alias_data_t *top;
;;   } aliases;
;;   /** The currently parsed document. */
;;   yaml_document_t *document;
;;   /**
;;    * @}
;;    */
;; } yaml_parser_t;
(define-public yaml_parser_t
  (name-ctype
   'yaml_parser_t
   (cstruct
    (list `(error ,yaml_error_type_t)
          `(problem ,(cpointer (cbase 'char)))
          `(problem_offset ,(cbase 'size_t))
          `(problem_value ,(cbase 'int))
          `(problem_mark ,yaml_mark_t)
          `(context ,(cpointer (cbase 'char)))
          `(context_mark ,yaml_mark_t)
          `(read_handler ,yaml_read_handler_t*)
          `(read_handler_data ,(cpointer (cbase 'void)))
          `(input ,(cunion
                    (list `(string
                            ,(cstruct
                              (list `(start ,(cpointer (cbase 'unsigned-char)))
                                    `(end ,(cpointer (cbase 'unsigned-char)))
                                    `(current
                                      ,(cpointer (cbase 'unsigned-char))))))
                          `(file ,(cpointer (cbase 'void))))))
          `(eof ,(cbase 'int))
          `(buffer
            ,(cstruct
              (list `(start ,(cpointer (delay yaml_char_t)))
                    `(end ,(cpointer (delay yaml_char_t)))
                    `(pointer ,(cpointer (delay yaml_char_t)))
                    `(last ,(cpointer (delay yaml_char_t))))))
          `(unread ,(cbase 'size_t))
          `(raw_buffer
            ,(cstruct
              (list `(start ,(cpointer (cbase 'unsigned-char)))
                    `(end ,(cpointer (cbase 'unsigned-char)))
                    `(pointer ,(cpointer (cbase 'unsigned-char)))
                    `(last ,(cpointer (cbase 'unsigned-char))))))
          `(encoding ,yaml_encoding_t)
          `(offset ,(cbase 'size_t))
          `(mark ,yaml_mark_t)
          `(stream_start_produced ,(cbase 'int))
          `(stream_end_produced ,(cbase 'int))
          `(flow_level ,(cbase 'int))
          `(tokens
            ,(cstruct
              (list `(start ,yaml_token_t*)
                    `(end ,yaml_token_t*)
                    `(head ,yaml_token_t*)
                    `(tail ,yaml_token_t*))))
          `(tokens_parsed ,(cbase 'size_t))
          `(token_available ,(cbase 'int))
          `(indents
            ,(cstruct
              (list `(start ,(cpointer (cbase 'int)))
                    `(end ,(cpointer (cbase 'int)))
                    `(top ,(cpointer (cbase 'int))))))
          `(indent ,(cbase 'int))
          `(simple_key_allowed ,(cbase 'int))
          `(simple_keys
            ,(cstruct
              (list `(start ,yaml_simple_key_t*)
                    `(end ,yaml_simple_key_t*)
                    `(top ,yaml_simple_key_t*))))
          `(states
            ,(cstruct
              (list `(start ,(cpointer (delay yaml_parser_state_t)))
                    `(end ,(cpointer (delay yaml_parser_state_t)))
                    `(top ,(cpointer (delay yaml_parser_state_t))))))
          `(state ,yaml_parser_state_t)
          `(marks ,(cstruct
                    (list `(start ,yaml_mark_t*)
                          `(end ,yaml_mark_t*)
                          `(top ,yaml_mark_t*))))
          `(tag_directives
            ,(cstruct
              (list `(start ,yaml_tag_directive_t*)
                    `(end ,yaml_tag_directive_t*)
                    `(top ,yaml_tag_directive_t*))))
          `(aliases
            ,(cstruct
              (list `(start ,yaml_alias_data_t*)
                    `(end ,yaml_alias_data_t*)
                    `(top ,yaml_alias_data_t*))))
          `(document ,yaml_document_t*)))))
(define-public yaml_parser_t*
  (name-ctype 'yaml_parser_t* (cpointer yaml_parser_t)))
(define-public struct-yaml_parser_s
  (name-ctype 'struct-yaml_parser_s yaml_parser_t))
(define-public struct-yaml_parser_s*
  (name-ctype 'struct-yaml_parser_s* yaml_parser_t*))

;; int yaml_parser_initialize(yaml_parser_t *parser);
(define-public yaml_parser_initialize
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search "yaml_parser_initialize")
                       (list '*)))))
    (lambda (parser)
      (let ((parser (arg->pointer parser yaml_parser_t*)))
        ((force ~proc) parser)))))

;; void yaml_parser_delete(yaml_parser_t *parser);
(define-public yaml_parser_delete
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:void
                       (foreign-pointer-search "yaml_parser_delete")
                       (list '*)))))
    (lambda (parser)
      (let ((parser (arg->pointer parser yaml_parser_t*)))
        ((force ~proc) parser)))))

;; void yaml_parser_set_input_string(yaml_parser_t *parser, const unsigned char
;;      *input, size_t size);
(define-public yaml_parser_set_input_string
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:void
                       (foreign-pointer-search "yaml_parser_set_input_string")
                       (list '* '* ffi:size_t)))))
    (lambda (parser input size)
      (let ((parser (arg->pointer parser yaml_parser_t*))
            (input (arg->pointer input))
            (size (arg->number size)))
        ((force ~proc) parser input size)))))

;; void yaml_parser_set_input_file(yaml_parser_t *parser, FILE *file);
(define-public yaml_parser_set_input_file
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:void
                       (foreign-pointer-search "yaml_parser_set_input_file")
                       (list '* '*)))))
    (lambda (parser file)
      (let ((parser (arg->pointer parser yaml_parser_t*))
            (file (arg->pointer file)))
        ((force ~proc) parser file)))))

;; void yaml_parser_set_input(yaml_parser_t *parser, yaml_read_handler_t *
;;     handler, void *data);
(define-public yaml_parser_set_input
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:void
                       (foreign-pointer-search "yaml_parser_set_input")
                       (list '* '* '*)))))
    (lambda (parser handler data)
      (let ((parser (arg->pointer parser yaml_parser_t*))
            (handler (arg->pointer handler yaml_read_handler_t*))
            (data (arg->pointer data)))
        ((force ~proc) parser handler data)))))

;; void yaml_parser_set_encoding(yaml_parser_t *parser, yaml_encoding_t 
;;     encoding);
(define-public yaml_parser_set_encoding
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:void
                       (foreign-pointer-search "yaml_parser_set_encoding")
                       (list '* ffi:int)))))
    (lambda (parser encoding)
      (let ((parser (arg->pointer parser yaml_parser_t*))
            (encoding (unwrap-enum encoding)))
        ((force ~proc) parser encoding)))))

;; int yaml_parser_scan(yaml_parser_t *parser, yaml_token_t *token);
(define-public yaml_parser_scan
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search "yaml_parser_scan")
                       (list '* '*)))))
    (lambda (parser token)
      (let ((parser (arg->pointer parser yaml_parser_t*))
            (token (arg->pointer token yaml_token_t*)))
        ((force ~proc) parser token)))))

;; int yaml_parser_parse(yaml_parser_t *parser, yaml_event_t *event);
(define-public yaml_parser_parse
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search "yaml_parser_parse")
                       (list '* '*)))))
    (lambda (parser event)
      (let ((parser (arg->pointer parser yaml_parser_t*))
            (event (arg->pointer event yaml_event_t*)))
        ((force ~proc) parser event)))))

;; int yaml_parser_load(yaml_parser_t *parser, yaml_document_t *document);
(define-public yaml_parser_load
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search "yaml_parser_load")
                       (list '* '*)))))
    (lambda (parser document)
      (let ((parser (arg->pointer parser yaml_parser_t*))
            (document (arg->pointer document yaml_document_t*)))
        ((force ~proc) parser document)))))

;; typedef int yaml_write_handler_t(void *data, unsigned char *buffer, size_t 
;;     size);
(define-public yaml_write_handler_t
  (name-ctype
   'yaml_write_handler_t
   (cfunction
    (lambda (~proc)
      (ffi:procedure->pointer
       ffi:int
       (lambda (data buffer size)
         ((lambda (~ret) (arg->number ~ret)) (~proc data buffer size)))
       (list '* '* ffi:size_t)))
    (lambda (~fptr)
      (let ((~proc (ffi:pointer->procedure
                    ffi:int
                    ~fptr
                    (list '* '* ffi:size_t))))
        (lambda (data buffer size)
          (let ((data (arg->pointer data))
                (buffer (arg->pointer buffer))
                (size (arg->number size)))
            (~proc data buffer size))))))))
(define-public yaml_write_handler_t*
  (name-ctype 'yaml_write_handler_t* (cpointer yaml_write_handler_t)))

;; typedef enum yaml_emitter_state_e {
;;   YAML_EMIT_STREAM_START_STATE,
;;   YAML_EMIT_FIRST_DOCUMENT_START_STATE,
;;   YAML_EMIT_DOCUMENT_START_STATE,
;;   YAML_EMIT_DOCUMENT_CONTENT_STATE,
;;   YAML_EMIT_DOCUMENT_END_STATE,
;;   YAML_EMIT_FLOW_SEQUENCE_FIRST_ITEM_STATE,
;;   YAML_EMIT_FLOW_SEQUENCE_ITEM_STATE,
;;   YAML_EMIT_FLOW_MAPPING_FIRST_KEY_STATE,
;;   YAML_EMIT_FLOW_MAPPING_KEY_STATE,
;;   YAML_EMIT_FLOW_MAPPING_SIMPLE_VALUE_STATE,
;;   YAML_EMIT_FLOW_MAPPING_VALUE_STATE,
;;   YAML_EMIT_BLOCK_SEQUENCE_FIRST_ITEM_STATE,
;;   YAML_EMIT_BLOCK_SEQUENCE_ITEM_STATE,
;;   YAML_EMIT_BLOCK_MAPPING_FIRST_KEY_STATE,
;;   YAML_EMIT_BLOCK_MAPPING_KEY_STATE,
;;   YAML_EMIT_BLOCK_MAPPING_SIMPLE_VALUE_STATE,
;;   YAML_EMIT_BLOCK_MAPPING_VALUE_STATE,
;;   YAML_EMIT_END_STATE,
;; } yaml_emitter_state_t;
(define yaml_emitter_state_t-alist
  '((YAML_EMIT_STREAM_START_STATE 0)
    (YAML_EMIT_FIRST_DOCUMENT_START_STATE 1)
    (YAML_EMIT_DOCUMENT_START_STATE 2)
    (YAML_EMIT_DOCUMENT_CONTENT_STATE 3)
    (YAML_EMIT_DOCUMENT_END_STATE 4)
    (YAML_EMIT_FLOW_SEQUENCE_FIRST_ITEM_STATE 5)
    (YAML_EMIT_FLOW_SEQUENCE_ITEM_STATE 6)
    (YAML_EMIT_FLOW_MAPPING_FIRST_KEY_STATE 7)
    (YAML_EMIT_FLOW_MAPPING_KEY_STATE 8)
    (YAML_EMIT_FLOW_MAPPING_SIMPLE_VALUE_STATE 9)
    (YAML_EMIT_FLOW_MAPPING_VALUE_STATE 10)
    (YAML_EMIT_BLOCK_SEQUENCE_FIRST_ITEM_STATE 11)
    (YAML_EMIT_BLOCK_SEQUENCE_ITEM_STATE 12)
    (YAML_EMIT_BLOCK_MAPPING_FIRST_KEY_STATE 13)
    (YAML_EMIT_BLOCK_MAPPING_KEY_STATE 14)
    (YAML_EMIT_BLOCK_MAPPING_SIMPLE_VALUE_STATE 15)
    (YAML_EMIT_BLOCK_MAPPING_VALUE_STATE 16)
    (YAML_EMIT_END_STATE 17)))
(define-public yaml_emitter_state_t
  (name-ctype 'yaml_emitter_state_t (cenum yaml_emitter_state_t-alist)))
(define-public unwrap-yaml_emitter_state_t
  (lambda (arg) (or (assq-ref yaml_emitter_state_t-alist arg) arg)))
(define-public wrap-yaml_emitter_state_t
  (let ((ral (rev-alist yaml_emitter_state_t-alist)))
    (lambda (arg) (or (assq-ref ral arg) arg))))

;; typedef struct yaml_anchors_s {
;;   /** The number of references. */
;;   int references;
;;   /** The anchor id. */
;;   int anchor;
;;   /** If the node has been emitted? */
;;   int serialized;
;; } yaml_anchors_t;
(define-public yaml_anchors_t
  (name-ctype
   'yaml_anchors_t
   (cstruct
    (list `(references ,(cbase 'int))
          `(anchor ,(cbase 'int))
          `(serialized ,(cbase 'int))))))
(define-public yaml_anchors_t*
  (name-ctype 'yaml_anchors_t* (cpointer yaml_anchors_t)))
(define-public struct-yaml_anchors_s
  (name-ctype 'struct-yaml_anchors_s yaml_anchors_t))
(define-public struct-yaml_anchors_s*
  (name-ctype 'struct-yaml_anchors_s* yaml_anchors_t*))

;; typedef struct yaml_emitter_s {
;;   /**
;;    * @name Error handling
;;    * @{
;;    */
;;   /** Error type. */
;;   yaml_error_type_t error;
;;   /** Error description. */
;;   const char *problem;
;;   /**
;;    * @}
;;    */
;;   /**
;;    * @name Writer stuff
;;    * @{
;;    */
;;   /** Write handler. */
;;   yaml_write_handler_t *write_handler;
;;   /** A pointer for passing to the write handler. */
;;   void *write_handler_data;
;;   /** Standard (string or file) output data. */
;;   union {
;;     /** String output data. */
;;     struct {
;;       /** The buffer pointer. */
;;       unsigned char *buffer;
;;       /** The buffer size. */
;;       size_t size;
;;       /** The number of written bytes. */
;;       size_t *size_written;
;;     } string;
;;     /** File output data. */
;;     FILE *file;
;;   } output;
;;   /** The working buffer. */
;;   struct {
;;     /** The beginning of the buffer. */
;;     yaml_char_t *start;
;;     /** The end of the buffer. */
;;     yaml_char_t *end;
;;     /** The current position of the buffer. */
;;     yaml_char_t *pointer;
;;     /** The last filled position of the buffer. */
;;     yaml_char_t *last;
;;   } buffer;
;;   /** The raw buffer. */
;;   struct {
;;     /** The beginning of the buffer. */
;;     unsigned char *start;
;;     /** The end of the buffer. */
;;     unsigned char *end;
;;     /** The current position of the buffer. */
;;     unsigned char *pointer;
;;     /** The last filled position of the buffer. */
;;     unsigned char *last;
;;   } raw_buffer;
;;   /** The stream encoding. */
;;   yaml_encoding_t encoding;
;;   /**
;;    * @}
;;    */
;;   /**
;;    * @name Emitter stuff
;;    * @{
;;    */
;;   /** If the output is in the canonical style? */
;;   int canonical;
;;   /** The number of indentation spaces. */
;;   int best_indent;
;;   /** The preferred width of the output lines. */
;;   int best_width;
;;   /** Allow unescaped non-ASCII characters? */
;;   int unicode;
;;   /** The preferred line break. */
;;   yaml_break_t line_break;
;;   /** The stack of states. */
;;   struct {
;;     /** The beginning of the stack. */
;;     yaml_emitter_state_t *start;
;;     /** The end of the stack. */
;;     yaml_emitter_state_t *end;
;;     /** The top of the stack. */
;;     yaml_emitter_state_t *top;
;;   } states;
;;   /** The current emitter state. */
;;   yaml_emitter_state_t state;
;;   /** The event queue. */
;;   struct {
;;     /** The beginning of the event queue. */
;;     yaml_event_t *start;
;;     /** The end of the event queue. */
;;     yaml_event_t *end;
;;     /** The head of the event queue. */
;;     yaml_event_t *head;
;;     /** The tail of the event queue. */
;;     yaml_event_t *tail;
;;   } events;
;;   /** The stack of indentation levels. */
;;   struct {
;;     /** The beginning of the stack. */
;;     int *start;
;;     /** The end of the stack. */
;;     int *end;
;;     /** The top of the stack. */
;;     int *top;
;;   } indents;
;;   /** The list of tag directives. */
;;   struct {
;;     /** The beginning of the list. */
;;     yaml_tag_directive_t *start;
;;     /** The end of the list. */
;;     yaml_tag_directive_t *end;
;;     /** The top of the list. */
;;     yaml_tag_directive_t *top;
;;   } tag_directives;
;;   /** The current indentation level. */
;;   int indent;
;;   /** The current flow level. */
;;   int flow_level;
;;   /** Is it the document root context? */
;;   int root_context;
;;   /** Is it a sequence context? */
;;   int sequence_context;
;;   /** Is it a mapping context? */
;;   int mapping_context;
;;   /** Is it a simple mapping key context? */
;;   int simple_key_context;
;;   /** The current line. */
;;   int line;
;;   /** The current column. */
;;   int column;
;;   /** If the last character was a whitespace? */
;;   int whitespace;
;;   /** If the last character was an indentation character (' ', '-', '?', ':')? */
;;       
;;   int indention;
;;   /** If an explicit document end is required? */
;;   int open_ended;
;;   /** Anchor analysis. */
;;   struct {
;;     /** The anchor value. */
;;     yaml_char_t *anchor;
;;     /** The anchor length. */
;;     size_t anchor_length;
;;     /** Is it an alias? */
;;     int alias;
;;   } anchor_data;
;;   /** Tag analysis. */
;;   struct {
;;     /** The tag handle. */
;;     yaml_char_t *handle;
;;     /** The tag handle length. */
;;     size_t handle_length;
;;     /** The tag suffix. */
;;     yaml_char_t *suffix;
;;     /** The tag suffix length. */
;;     size_t suffix_length;
;;   } tag_data;
;;   /** Scalar analysis. */
;;   struct {
;;     /** The scalar value. */
;;     yaml_char_t *value;
;;     /** The scalar length. */
;;     size_t length;
;;     /** Does the scalar contain line breaks? */
;;     int multiline;
;;     /** Can the scalar be expessed in the flow plain style? */
;;     int flow_plain_allowed;
;;     /** Can the scalar be expressed in the block plain style? */
;;     int block_plain_allowed;
;;     /** Can the scalar be expressed in the single quoted style? */
;;     int single_quoted_allowed;
;;     /** Can the scalar be expressed in the literal or folded styles? */
;;     int block_allowed;
;;     /** The output style. */
;;     yaml_scalar_style_t style;
;;   } scalar_data;
;;   /**
;;    * @}
;;    */
;;   /**
;;    * @name Dumper stuff
;;    * @{
;;    */
;;   /** If the stream was already opened? */
;;   int opened;
;;   /** If the stream was already closed? */
;;   int closed;
;;   /** The information associated with the document nodes. */
;;   yaml_anchors_t *anchors;
;;   /** The last assigned anchor id. */
;;   int last_anchor_id;
;;   /** The currently emitted document. */
;;   yaml_document_t *document;
;;   /**
;;    * @}
;;    */
;; } yaml_emitter_t;
(define-public yaml_emitter_t
  (name-ctype
   'yaml_emitter_t
   (cstruct
    (list `(error ,yaml_error_type_t)
          `(problem ,(cpointer (cbase 'char)))
          `(write_handler ,yaml_write_handler_t*)
          `(write_handler_data ,(cpointer (cbase 'void)))
          `(output
            ,(cunion
              (list `(string
                      ,(cstruct
                        (list `(buffer ,(cpointer (cbase 'unsigned-char)))
                              `(size ,(cbase 'size_t))
                              `(size_written ,(cpointer (cbase 'size_t))))))
                    `(file ,(cpointer (cbase 'void))))))
          `(buffer
            ,(cstruct
              (list `(start ,(cpointer (delay yaml_char_t)))
                    `(end ,(cpointer (delay yaml_char_t)))
                    `(pointer ,(cpointer (delay yaml_char_t)))
                    `(last ,(cpointer (delay yaml_char_t))))))
          `(raw_buffer
            ,(cstruct
              (list `(start ,(cpointer (cbase 'unsigned-char)))
                    `(end ,(cpointer (cbase 'unsigned-char)))
                    `(pointer ,(cpointer (cbase 'unsigned-char)))
                    `(last ,(cpointer (cbase 'unsigned-char))))))
          `(encoding ,yaml_encoding_t)
          `(canonical ,(cbase 'int))
          `(best_indent ,(cbase 'int))
          `(best_width ,(cbase 'int))
          `(unicode ,(cbase 'int))
          `(line_break ,yaml_break_t)
          `(states
            ,(cstruct
              (list `(start ,(cpointer (delay yaml_emitter_state_t)))
                    `(end ,(cpointer (delay yaml_emitter_state_t)))
                    `(top ,(cpointer (delay yaml_emitter_state_t))))))
          `(state ,yaml_emitter_state_t)
          `(events
            ,(cstruct
              (list `(start ,yaml_event_t*)
                    `(end ,yaml_event_t*)
                    `(head ,yaml_event_t*)
                    `(tail ,yaml_event_t*))))
          `(indents
            ,(cstruct
              (list `(start ,(cpointer (cbase 'int)))
                    `(end ,(cpointer (cbase 'int)))
                    `(top ,(cpointer (cbase 'int))))))
          `(tag_directives
            ,(cstruct
              (list `(start ,yaml_tag_directive_t*)
                    `(end ,yaml_tag_directive_t*)
                    `(top ,yaml_tag_directive_t*))))
          `(indent ,(cbase 'int))
          `(flow_level ,(cbase 'int))
          `(root_context ,(cbase 'int))
          `(sequence_context ,(cbase 'int))
          `(mapping_context ,(cbase 'int))
          `(simple_key_context ,(cbase 'int))
          `(line ,(cbase 'int))
          `(column ,(cbase 'int))
          `(whitespace ,(cbase 'int))
          `(indention ,(cbase 'int))
          `(open_ended ,(cbase 'int))
          `(anchor_data
            ,(cstruct
              (list `(anchor ,(cpointer (delay yaml_char_t)))
                    `(anchor_length ,(cbase 'size_t))
                    `(alias ,(cbase 'int)))))
          `(tag_data
            ,(cstruct
              (list `(handle ,(cpointer (delay yaml_char_t)))
                    `(handle_length ,(cbase 'size_t))
                    `(suffix ,(cpointer (delay yaml_char_t)))
                    `(suffix_length ,(cbase 'size_t)))))
          `(scalar_data
            ,(cstruct
              (list `(value ,(cpointer (delay yaml_char_t)))
                    `(length ,(cbase 'size_t))
                    `(multiline ,(cbase 'int))
                    `(flow_plain_allowed ,(cbase 'int))
                    `(block_plain_allowed ,(cbase 'int))
                    `(single_quoted_allowed ,(cbase 'int))
                    `(block_allowed ,(cbase 'int))
                    `(style ,yaml_scalar_style_t))))
          `(opened ,(cbase 'int))
          `(closed ,(cbase 'int))
          `(anchors ,yaml_anchors_t*)
          `(last_anchor_id ,(cbase 'int))
          `(document ,yaml_document_t*)))))
(define-public yaml_emitter_t*
  (name-ctype 'yaml_emitter_t* (cpointer yaml_emitter_t)))
(define-public struct-yaml_emitter_s
  (name-ctype 'struct-yaml_emitter_s yaml_emitter_t))
(define-public struct-yaml_emitter_s*
  (name-ctype 'struct-yaml_emitter_s* yaml_emitter_t*))

;; int yaml_emitter_initialize(yaml_emitter_t *emitter);
(define-public yaml_emitter_initialize
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search "yaml_emitter_initialize")
                       (list '*)))))
    (lambda (emitter)
      (let ((emitter (arg->pointer emitter yaml_emitter_t*)))
        ((force ~proc) emitter)))))

;; void yaml_emitter_delete(yaml_emitter_t *emitter);
(define-public yaml_emitter_delete
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:void
                       (foreign-pointer-search "yaml_emitter_delete")
                       (list '*)))))
    (lambda (emitter)
      (let ((emitter (arg->pointer emitter yaml_emitter_t*)))
        ((force ~proc) emitter)))))

;; void yaml_emitter_set_output_string(yaml_emitter_t *emitter, unsigned char *
;;     output, size_t size, size_t *size_written);
(define-public yaml_emitter_set_output_string
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:void
                       (foreign-pointer-search
                        "yaml_emitter_set_output_string")
                       (list '* '* ffi:size_t '*)))))
    (lambda (emitter output size size_written)
      (let ((emitter (arg->pointer emitter yaml_emitter_t*))
            (output (arg->pointer output))
            (size (arg->number size))
            (size_written (arg->pointer size_written)))
        ((force ~proc) emitter output size size_written)))))

;; void yaml_emitter_set_output_file(yaml_emitter_t *emitter, FILE *file);
(define-public yaml_emitter_set_output_file
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:void
                       (foreign-pointer-search "yaml_emitter_set_output_file")
                       (list '* '*)))))
    (lambda (emitter file)
      (let ((emitter (arg->pointer emitter yaml_emitter_t*))
            (file (arg->pointer file)))
        ((force ~proc) emitter file)))))

;; void yaml_emitter_set_output(yaml_emitter_t *emitter, yaml_write_handler_t *
;;     handler, void *data);
(define-public yaml_emitter_set_output
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:void
                       (foreign-pointer-search "yaml_emitter_set_output")
                       (list '* '* '*)))))
    (lambda (emitter handler data)
      (let ((emitter (arg->pointer emitter yaml_emitter_t*))
            (handler (arg->pointer handler yaml_write_handler_t*))
            (data (arg->pointer data)))
        ((force ~proc) emitter handler data)))))

;; void yaml_emitter_set_encoding(yaml_emitter_t *emitter, yaml_encoding_t 
;;     encoding);
(define-public yaml_emitter_set_encoding
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:void
                       (foreign-pointer-search "yaml_emitter_set_encoding")
                       (list '* ffi:int)))))
    (lambda (emitter encoding)
      (let ((emitter (arg->pointer emitter yaml_emitter_t*))
            (encoding (unwrap-enum encoding)))
        ((force ~proc) emitter encoding)))))

;; void yaml_emitter_set_canonical(yaml_emitter_t *emitter, int canonical);
(define-public yaml_emitter_set_canonical
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:void
                       (foreign-pointer-search "yaml_emitter_set_canonical")
                       (list '* ffi:int)))))
    (lambda (emitter canonical)
      (let ((emitter (arg->pointer emitter yaml_emitter_t*))
            (canonical (arg->number canonical)))
        ((force ~proc) emitter canonical)))))

;; void yaml_emitter_set_indent(yaml_emitter_t *emitter, int indent);
(define-public yaml_emitter_set_indent
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:void
                       (foreign-pointer-search "yaml_emitter_set_indent")
                       (list '* ffi:int)))))
    (lambda (emitter indent)
      (let ((emitter (arg->pointer emitter yaml_emitter_t*))
            (indent (arg->number indent)))
        ((force ~proc) emitter indent)))))

;; void yaml_emitter_set_width(yaml_emitter_t *emitter, int width);
(define-public yaml_emitter_set_width
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:void
                       (foreign-pointer-search "yaml_emitter_set_width")
                       (list '* ffi:int)))))
    (lambda (emitter width)
      (let ((emitter (arg->pointer emitter yaml_emitter_t*))
            (width (arg->number width)))
        ((force ~proc) emitter width)))))

;; void yaml_emitter_set_unicode(yaml_emitter_t *emitter, int unicode);
(define-public yaml_emitter_set_unicode
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:void
                       (foreign-pointer-search "yaml_emitter_set_unicode")
                       (list '* ffi:int)))))
    (lambda (emitter unicode)
      (let ((emitter (arg->pointer emitter yaml_emitter_t*))
            (unicode (arg->number unicode)))
        ((force ~proc) emitter unicode)))))

;; void yaml_emitter_set_break(yaml_emitter_t *emitter, yaml_break_t line_break
;;     );
(define-public yaml_emitter_set_break
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:void
                       (foreign-pointer-search "yaml_emitter_set_break")
                       (list '* ffi:int)))))
    (lambda (emitter line_break)
      (let ((emitter (arg->pointer emitter yaml_emitter_t*))
            (line_break (unwrap-enum line_break)))
        ((force ~proc) emitter line_break)))))

;; int yaml_emitter_emit(yaml_emitter_t *emitter, yaml_event_t *event);
(define-public yaml_emitter_emit
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search "yaml_emitter_emit")
                       (list '* '*)))))
    (lambda (emitter event)
      (let ((emitter (arg->pointer emitter yaml_emitter_t*))
            (event (arg->pointer event yaml_event_t*)))
        ((force ~proc) emitter event)))))

;; int yaml_emitter_open(yaml_emitter_t *emitter);
(define-public yaml_emitter_open
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search "yaml_emitter_open")
                       (list '*)))))
    (lambda (emitter)
      (let ((emitter (arg->pointer emitter yaml_emitter_t*)))
        ((force ~proc) emitter)))))

;; int yaml_emitter_close(yaml_emitter_t *emitter);
(define-public yaml_emitter_close
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search "yaml_emitter_close")
                       (list '*)))))
    (lambda (emitter)
      (let ((emitter (arg->pointer emitter yaml_emitter_t*)))
        ((force ~proc) emitter)))))

;; int yaml_emitter_dump(yaml_emitter_t *emitter, yaml_document_t *document);
(define-public yaml_emitter_dump
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search "yaml_emitter_dump")
                       (list '* '*)))))
    (lambda (emitter document)
      (let ((emitter (arg->pointer emitter yaml_emitter_t*))
            (document (arg->pointer document yaml_document_t*)))
        ((force ~proc) emitter document)))))

;; int yaml_emitter_flush(yaml_emitter_t *emitter);
(define-public yaml_emitter_flush
  (let ((~proc (delay (ffi:pointer->procedure
                       ffi:int
                       (foreign-pointer-search "yaml_emitter_flush")
                       (list '*)))))
    (lambda (emitter)
      (let ((emitter (arg->pointer emitter yaml_emitter_t*)))
        ((force ~proc) emitter)))))

;; access to enum symbols and #define'd constants:
(define yaml-libyaml-symbol-tab
  '((YAML_EMIT_END_STATE . 17)
    (YAML_EMIT_BLOCK_MAPPING_VALUE_STATE . 16)
    (YAML_EMIT_BLOCK_MAPPING_SIMPLE_VALUE_STATE . 15)
    (YAML_EMIT_BLOCK_MAPPING_KEY_STATE . 14)
    (YAML_EMIT_BLOCK_MAPPING_FIRST_KEY_STATE . 13)
    (YAML_EMIT_BLOCK_SEQUENCE_ITEM_STATE . 12)
    (YAML_EMIT_BLOCK_SEQUENCE_FIRST_ITEM_STATE . 11)
    (YAML_EMIT_FLOW_MAPPING_VALUE_STATE . 10)
    (YAML_EMIT_FLOW_MAPPING_SIMPLE_VALUE_STATE . 9)
    (YAML_EMIT_FLOW_MAPPING_KEY_STATE . 8)
    (YAML_EMIT_FLOW_MAPPING_FIRST_KEY_STATE . 7)
    (YAML_EMIT_FLOW_SEQUENCE_ITEM_STATE . 6)
    (YAML_EMIT_FLOW_SEQUENCE_FIRST_ITEM_STATE . 5)
    (YAML_EMIT_DOCUMENT_END_STATE . 4)
    (YAML_EMIT_DOCUMENT_CONTENT_STATE . 3)
    (YAML_EMIT_DOCUMENT_START_STATE . 2)
    (YAML_EMIT_FIRST_DOCUMENT_START_STATE . 1)
    (YAML_EMIT_STREAM_START_STATE . 0)
    (YAML_EMIT_END_STATE . 17)
    (YAML_EMIT_BLOCK_MAPPING_VALUE_STATE . 16)
    (YAML_EMIT_BLOCK_MAPPING_SIMPLE_VALUE_STATE . 15)
    (YAML_EMIT_BLOCK_MAPPING_KEY_STATE . 14)
    (YAML_EMIT_BLOCK_MAPPING_FIRST_KEY_STATE . 13)
    (YAML_EMIT_BLOCK_SEQUENCE_ITEM_STATE . 12)
    (YAML_EMIT_BLOCK_SEQUENCE_FIRST_ITEM_STATE . 11)
    (YAML_EMIT_FLOW_MAPPING_VALUE_STATE . 10)
    (YAML_EMIT_FLOW_MAPPING_SIMPLE_VALUE_STATE . 9)
    (YAML_EMIT_FLOW_MAPPING_KEY_STATE . 8)
    (YAML_EMIT_FLOW_MAPPING_FIRST_KEY_STATE . 7)
    (YAML_EMIT_FLOW_SEQUENCE_ITEM_STATE . 6)
    (YAML_EMIT_FLOW_SEQUENCE_FIRST_ITEM_STATE . 5)
    (YAML_EMIT_DOCUMENT_END_STATE . 4)
    (YAML_EMIT_DOCUMENT_CONTENT_STATE . 3)
    (YAML_EMIT_DOCUMENT_START_STATE . 2)
    (YAML_EMIT_FIRST_DOCUMENT_START_STATE . 1)
    (YAML_EMIT_STREAM_START_STATE . 0)
    (YAML_PARSE_END_STATE . 23)
    (YAML_PARSE_FLOW_MAPPING_EMPTY_VALUE_STATE . 22)
    (YAML_PARSE_FLOW_MAPPING_VALUE_STATE . 21)
    (YAML_PARSE_FLOW_MAPPING_KEY_STATE . 20)
    (YAML_PARSE_FLOW_MAPPING_FIRST_KEY_STATE . 19)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_END_STATE . 18)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_VALUE_STATE . 17)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_KEY_STATE . 16)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_STATE . 15)
    (YAML_PARSE_FLOW_SEQUENCE_FIRST_ENTRY_STATE . 14)
    (YAML_PARSE_BLOCK_MAPPING_VALUE_STATE . 13)
    (YAML_PARSE_BLOCK_MAPPING_KEY_STATE . 12)
    (YAML_PARSE_BLOCK_MAPPING_FIRST_KEY_STATE . 11)
    (YAML_PARSE_INDENTLESS_SEQUENCE_ENTRY_STATE . 10)
    (YAML_PARSE_BLOCK_SEQUENCE_ENTRY_STATE . 9)
    (YAML_PARSE_BLOCK_SEQUENCE_FIRST_ENTRY_STATE . 8)
    (YAML_PARSE_FLOW_NODE_STATE . 7)
    (YAML_PARSE_BLOCK_NODE_OR_INDENTLESS_SEQUENCE_STATE . 6)
    (YAML_PARSE_BLOCK_NODE_STATE . 5)
    (YAML_PARSE_DOCUMENT_END_STATE . 4)
    (YAML_PARSE_DOCUMENT_CONTENT_STATE . 3)
    (YAML_PARSE_DOCUMENT_START_STATE . 2)
    (YAML_PARSE_IMPLICIT_DOCUMENT_START_STATE . 1)
    (YAML_PARSE_STREAM_START_STATE . 0)
    (YAML_PARSE_END_STATE . 23)
    (YAML_PARSE_FLOW_MAPPING_EMPTY_VALUE_STATE . 22)
    (YAML_PARSE_FLOW_MAPPING_VALUE_STATE . 21)
    (YAML_PARSE_FLOW_MAPPING_KEY_STATE . 20)
    (YAML_PARSE_FLOW_MAPPING_FIRST_KEY_STATE . 19)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_END_STATE . 18)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_VALUE_STATE . 17)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_KEY_STATE . 16)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_STATE . 15)
    (YAML_PARSE_FLOW_SEQUENCE_FIRST_ENTRY_STATE . 14)
    (YAML_PARSE_BLOCK_MAPPING_VALUE_STATE . 13)
    (YAML_PARSE_BLOCK_MAPPING_KEY_STATE . 12)
    (YAML_PARSE_BLOCK_MAPPING_FIRST_KEY_STATE . 11)
    (YAML_PARSE_INDENTLESS_SEQUENCE_ENTRY_STATE . 10)
    (YAML_PARSE_BLOCK_SEQUENCE_ENTRY_STATE . 9)
    (YAML_PARSE_BLOCK_SEQUENCE_FIRST_ENTRY_STATE . 8)
    (YAML_PARSE_FLOW_NODE_STATE . 7)
    (YAML_PARSE_BLOCK_NODE_OR_INDENTLESS_SEQUENCE_STATE . 6)
    (YAML_PARSE_BLOCK_NODE_STATE . 5)
    (YAML_PARSE_DOCUMENT_END_STATE . 4)
    (YAML_PARSE_DOCUMENT_CONTENT_STATE . 3)
    (YAML_PARSE_DOCUMENT_START_STATE . 2)
    (YAML_PARSE_IMPLICIT_DOCUMENT_START_STATE . 1)
    (YAML_PARSE_STREAM_START_STATE . 0)
    (YAML_MAPPING_NODE . 3)
    (YAML_SEQUENCE_NODE . 2)
    (YAML_SCALAR_NODE . 1)
    (YAML_NO_NODE . 0)
    (YAML_MAPPING_NODE . 3)
    (YAML_SEQUENCE_NODE . 2)
    (YAML_SCALAR_NODE . 1)
    (YAML_NO_NODE . 0)
    (YAML_MAPPING_END_EVENT . 10)
    (YAML_MAPPING_START_EVENT . 9)
    (YAML_SEQUENCE_END_EVENT . 8)
    (YAML_SEQUENCE_START_EVENT . 7)
    (YAML_SCALAR_EVENT . 6)
    (YAML_ALIAS_EVENT . 5)
    (YAML_DOCUMENT_END_EVENT . 4)
    (YAML_DOCUMENT_START_EVENT . 3)
    (YAML_STREAM_END_EVENT . 2)
    (YAML_STREAM_START_EVENT . 1)
    (YAML_NO_EVENT . 0)
    (YAML_MAPPING_END_EVENT . 10)
    (YAML_MAPPING_START_EVENT . 9)
    (YAML_SEQUENCE_END_EVENT . 8)
    (YAML_SEQUENCE_START_EVENT . 7)
    (YAML_SCALAR_EVENT . 6)
    (YAML_ALIAS_EVENT . 5)
    (YAML_DOCUMENT_END_EVENT . 4)
    (YAML_DOCUMENT_START_EVENT . 3)
    (YAML_STREAM_END_EVENT . 2)
    (YAML_STREAM_START_EVENT . 1)
    (YAML_NO_EVENT . 0)
    (YAML_SCALAR_TOKEN . 21)
    (YAML_TAG_TOKEN . 20)
    (YAML_ANCHOR_TOKEN . 19)
    (YAML_ALIAS_TOKEN . 18)
    (YAML_VALUE_TOKEN . 17)
    (YAML_KEY_TOKEN . 16)
    (YAML_FLOW_ENTRY_TOKEN . 15)
    (YAML_BLOCK_ENTRY_TOKEN . 14)
    (YAML_FLOW_MAPPING_END_TOKEN . 13)
    (YAML_FLOW_MAPPING_START_TOKEN . 12)
    (YAML_FLOW_SEQUENCE_END_TOKEN . 11)
    (YAML_FLOW_SEQUENCE_START_TOKEN . 10)
    (YAML_BLOCK_END_TOKEN . 9)
    (YAML_BLOCK_MAPPING_START_TOKEN . 8)
    (YAML_BLOCK_SEQUENCE_START_TOKEN . 7)
    (YAML_DOCUMENT_END_TOKEN . 6)
    (YAML_DOCUMENT_START_TOKEN . 5)
    (YAML_TAG_DIRECTIVE_TOKEN . 4)
    (YAML_VERSION_DIRECTIVE_TOKEN . 3)
    (YAML_STREAM_END_TOKEN . 2)
    (YAML_STREAM_START_TOKEN . 1)
    (YAML_NO_TOKEN . 0)
    (YAML_SCALAR_TOKEN . 21)
    (YAML_TAG_TOKEN . 20)
    (YAML_ANCHOR_TOKEN . 19)
    (YAML_ALIAS_TOKEN . 18)
    (YAML_VALUE_TOKEN . 17)
    (YAML_KEY_TOKEN . 16)
    (YAML_FLOW_ENTRY_TOKEN . 15)
    (YAML_BLOCK_ENTRY_TOKEN . 14)
    (YAML_FLOW_MAPPING_END_TOKEN . 13)
    (YAML_FLOW_MAPPING_START_TOKEN . 12)
    (YAML_FLOW_SEQUENCE_END_TOKEN . 11)
    (YAML_FLOW_SEQUENCE_START_TOKEN . 10)
    (YAML_BLOCK_END_TOKEN . 9)
    (YAML_BLOCK_MAPPING_START_TOKEN . 8)
    (YAML_BLOCK_SEQUENCE_START_TOKEN . 7)
    (YAML_DOCUMENT_END_TOKEN . 6)
    (YAML_DOCUMENT_START_TOKEN . 5)
    (YAML_TAG_DIRECTIVE_TOKEN . 4)
    (YAML_VERSION_DIRECTIVE_TOKEN . 3)
    (YAML_STREAM_END_TOKEN . 2)
    (YAML_STREAM_START_TOKEN . 1)
    (YAML_NO_TOKEN . 0)
    (YAML_FLOW_MAPPING_STYLE . 2)
    (YAML_BLOCK_MAPPING_STYLE . 1)
    (YAML_ANY_MAPPING_STYLE . 0)
    (YAML_FLOW_MAPPING_STYLE . 2)
    (YAML_BLOCK_MAPPING_STYLE . 1)
    (YAML_ANY_MAPPING_STYLE . 0)
    (YAML_FLOW_SEQUENCE_STYLE . 2)
    (YAML_BLOCK_SEQUENCE_STYLE . 1)
    (YAML_ANY_SEQUENCE_STYLE . 0)
    (YAML_FLOW_SEQUENCE_STYLE . 2)
    (YAML_BLOCK_SEQUENCE_STYLE . 1)
    (YAML_ANY_SEQUENCE_STYLE . 0)
    (YAML_FOLDED_SCALAR_STYLE . 5)
    (YAML_LITERAL_SCALAR_STYLE . 4)
    (YAML_DOUBLE_QUOTED_SCALAR_STYLE . 3)
    (YAML_SINGLE_QUOTED_SCALAR_STYLE . 2)
    (YAML_PLAIN_SCALAR_STYLE . 1)
    (YAML_ANY_SCALAR_STYLE . 0)
    (YAML_FOLDED_SCALAR_STYLE . 5)
    (YAML_LITERAL_SCALAR_STYLE . 4)
    (YAML_DOUBLE_QUOTED_SCALAR_STYLE . 3)
    (YAML_SINGLE_QUOTED_SCALAR_STYLE . 2)
    (YAML_PLAIN_SCALAR_STYLE . 1)
    (YAML_ANY_SCALAR_STYLE . 0)
    (YAML_EMITTER_ERROR . 7)
    (YAML_WRITER_ERROR . 6)
    (YAML_COMPOSER_ERROR . 5)
    (YAML_PARSER_ERROR . 4)
    (YAML_SCANNER_ERROR . 3)
    (YAML_READER_ERROR . 2)
    (YAML_MEMORY_ERROR . 1)
    (YAML_NO_ERROR . 0)
    (YAML_EMITTER_ERROR . 7)
    (YAML_WRITER_ERROR . 6)
    (YAML_COMPOSER_ERROR . 5)
    (YAML_PARSER_ERROR . 4)
    (YAML_SCANNER_ERROR . 3)
    (YAML_READER_ERROR . 2)
    (YAML_MEMORY_ERROR . 1)
    (YAML_NO_ERROR . 0)
    (YAML_CRLN_BREAK . 3)
    (YAML_LN_BREAK . 2)
    (YAML_CR_BREAK . 1)
    (YAML_ANY_BREAK . 0)
    (YAML_CRLN_BREAK . 3)
    (YAML_LN_BREAK . 2)
    (YAML_CR_BREAK . 1)
    (YAML_ANY_BREAK . 0)
    (YAML_UTF16BE_ENCODING . 3)
    (YAML_UTF16LE_ENCODING . 2)
    (YAML_UTF8_ENCODING . 1)
    (YAML_ANY_ENCODING . 0)
    (YAML_UTF16BE_ENCODING . 3)
    (YAML_UTF16LE_ENCODING . 2)
    (YAML_UTF8_ENCODING . 1)
    (YAML_ANY_ENCODING . 0)
    (YAML_DEFAULT_MAPPING_TAG . "tag:yaml.org,2002:map")
    (YAML_DEFAULT_SEQUENCE_TAG . "tag:yaml.org,2002:seq")
    (YAML_DEFAULT_SCALAR_TAG . "tag:yaml.org,2002:str")
    (YAML_MAP_TAG . "tag:yaml.org,2002:map")
    (YAML_SEQ_TAG . "tag:yaml.org,2002:seq")
    (YAML_TIMESTAMP_TAG . "tag:yaml.org,2002:timestamp")
    (YAML_FLOAT_TAG . "tag:yaml.org,2002:float")
    (YAML_INT_TAG . "tag:yaml.org,2002:int")
    (YAML_STR_TAG . "tag:yaml.org,2002:str")
    (YAML_BOOL_TAG . "tag:yaml.org,2002:bool")
    (YAML_NULL_TAG . "tag:yaml.org,2002:null")))
(export yaml-libyaml-symbol-tab)

(define yaml-libyaml-symbol-val
  (lambda (k) (or (assq-ref yaml-libyaml-symbol-tab k))))
(export yaml-libyaml-symbol-val)


(define yaml-libyaml-types
  '("yaml_char_t" (struct-pointer . "yaml_version_directive_s") (struct . 
    "yaml_version_directive_s") (pointer . "yaml_version_directive_t") 
    "yaml_version_directive_t" (struct-pointer . "yaml_tag_directive_s") 
    (struct . "yaml_tag_directive_s") (pointer . "yaml_tag_directive_t") 
    "yaml_tag_directive_t" (enum . "yaml_encoding_e") "yaml_encoding_t" 
    (enum . "yaml_break_e") "yaml_break_t" (enum . "yaml_error_type_e") 
    "yaml_error_type_t" (struct-pointer . "yaml_mark_s") (struct . 
    "yaml_mark_s") (pointer . "yaml_mark_t") "yaml_mark_t" (enum . 
    "yaml_scalar_style_e") "yaml_scalar_style_t" (enum . 
    "yaml_sequence_style_e") "yaml_sequence_style_t" (enum . 
    "yaml_mapping_style_e") "yaml_mapping_style_t" (enum . "yaml_token_type_e"
    ) "yaml_token_type_t" (struct-pointer . "yaml_token_s") (struct . 
    "yaml_token_s") (pointer . "yaml_token_t") "yaml_token_t" (enum . 
    "yaml_event_type_e") "yaml_event_type_t" (struct-pointer . "yaml_event_s")
    (struct . "yaml_event_s") (pointer . "yaml_event_t") "yaml_event_t" 
    (enum . "yaml_node_type_e") "yaml_node_type_t" (pointer . "yaml_node_t") 
    "yaml_node_t" "yaml_node_item_t" (struct-pointer . "yaml_node_pair_s") 
    (struct . "yaml_node_pair_s") (pointer . "yaml_node_pair_t") 
    "yaml_node_pair_t" (struct . "yaml_node_s") "yaml_node_t" (struct-pointer 
    . "yaml_document_s") (struct . "yaml_document_s") (pointer . 
    "yaml_document_t") "yaml_document_t" (pointer . "yaml_read_handler_t") 
    "yaml_read_handler_t" (struct-pointer . "yaml_simple_key_s") (struct . 
    "yaml_simple_key_s") (pointer . "yaml_simple_key_t") "yaml_simple_key_t" 
    (enum . "yaml_parser_state_e") "yaml_parser_state_t" (struct-pointer . 
    "yaml_alias_data_s") (struct . "yaml_alias_data_s") (pointer . 
    "yaml_alias_data_t") "yaml_alias_data_t" (struct-pointer . "yaml_parser_s"
    ) (struct . "yaml_parser_s") (pointer . "yaml_parser_t") "yaml_parser_t" 
    (pointer . "yaml_write_handler_t") "yaml_write_handler_t" (enum . 
    "yaml_emitter_state_e") "yaml_emitter_state_t" (struct-pointer . 
    "yaml_anchors_s") (struct . "yaml_anchors_s") (pointer . "yaml_anchors_t")
    "yaml_anchors_t" (struct-pointer . "yaml_emitter_s") (struct . 
    "yaml_emitter_s") (pointer . "yaml_emitter_t") "yaml_emitter_t"))
(export yaml-libyaml-types)

(define (unwrap-enum arg)
  (cond
    ((number? arg) arg)
    ((symbol? arg) (yaml-libyaml-symbol-val arg))
    ((cdata? arg) (cdata-ref arg))
    (else (error "type mismatch"))))

(define-syntax case-backend
  (lambda (x)
    (syntax-case x (else)
      ((_ ((sym ...) exp ...) nxt ...)
       (let loop ((syms (syntax (sym ...))))
         (if (null? syms)
             (syntax (case-backend nxt ...))
             (if (eq? backend (syntax->datum (car syms)))
                 (syntax (begin (if #f #f) exp ...))
                 (loop (cdr syms))))))
      ((_) '#(if #f #f))
      ((_ (else exp ...)) (syntax (begin (if #f #f) exp ...))))))


;; --- last line ---
