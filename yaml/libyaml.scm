;; generated with `guild compile-ffi yaml/libyaml.ffi'

(define-module (yaml libyaml)
  #:use-module (yaml ffi-help-rt)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (bytestructures guile)
  )
(define yaml-libyaml-llibs
  (list (dynamic-link "libyaml")))

;; const char *yaml_get_version_string(void );
(define yaml_get_version_string
  (let ((~yaml_get_version_string
          (delay (fh-link-proc
                   ffi-void*
                   "yaml_get_version_string"
                   (list ffi:void)
                   yaml-libyaml-llibs))))
    (lambda (_)
      (let () ((force ~yaml_get_version_string) _)))))
(export yaml_get_version_string)

;; void yaml_get_version(int *major, int *minor, int *patch);
(define yaml_get_version
  (let ((~yaml_get_version
          (delay (fh-link-proc
                   ffi:void
                   "yaml_get_version"
                   (list ffi-void* ffi-void* ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (major minor patch)
      (let ((~major (unwrap~pointer major))
            (~minor (unwrap~pointer minor))
            (~patch (unwrap~pointer patch)))
        ((force ~yaml_get_version) ~major ~minor ~patch)))))
(export yaml_get_version)

;; typedef unsigned char yaml_char_t;
(define-public yaml_char_t-desc uint8)

;; typedef struct yaml_version_directive_s {
;;   /** The major version number. */
;;   int major;
;;   /** The minor version number. */
;;   int minor;
;; } yaml_version_directive_t;
(define-public yaml_version_directive_t-desc
  (bs:struct (list `(major ,int) `(minor ,int))))
(define-fh-compound-type yaml_version_directive_t 
 yaml_version_directive_t-desc yaml_version_directive_t? 
 make-yaml_version_directive_t)
(export yaml_version_directive_t yaml_version_directive_t? 
 make-yaml_version_directive_t)
(define-public yaml_version_directive_t*-desc
  (fh:pointer yaml_version_directive_t-desc))
(define-fh-pointer-type yaml_version_directive_t* 
 yaml_version_directive_t*-desc yaml_version_directive_t*? 
 make-yaml_version_directive_t*)
(export yaml_version_directive_t* yaml_version_directive_t*? 
 make-yaml_version_directive_t*)
(ref<->deref!
  yaml_version_directive_t*
  make-yaml_version_directive_t*
  yaml_version_directive_t
  make-yaml_version_directive_t)
(define-public struct-yaml_version_directive_s-desc
  yaml_version_directive_t-desc)
(define-fh-compound-type struct-yaml_version_directive_s 
 struct-yaml_version_directive_s-desc struct-yaml_version_directive_s? 
 make-struct-yaml_version_directive_s)
(export struct-yaml_version_directive_s struct-yaml_version_directive_s? 
 make-struct-yaml_version_directive_s)
(define-public struct-yaml_version_directive_s*-desc
  yaml_version_directive_t*-desc)
(define-fh-pointer-type struct-yaml_version_directive_s* 
 struct-yaml_version_directive_s*-desc struct-yaml_version_directive_s*? 
 make-struct-yaml_version_directive_s*)
(export struct-yaml_version_directive_s* struct-yaml_version_directive_s*? 
 make-struct-yaml_version_directive_s*)
(ref<->deref!
  struct-yaml_version_directive_s*
  make-struct-yaml_version_directive_s*
  struct-yaml_version_directive_s
  make-struct-yaml_version_directive_s)

;; typedef struct yaml_tag_directive_s {
;;   /** The tag handle. */
;;   yaml_char_t *handle;
;;   /** The tag prefix. */
;;   yaml_char_t *prefix;
;; } yaml_tag_directive_t;
(define-public yaml_tag_directive_t-desc
  (bs:struct
    (list `(handle ,(fh:pointer uint8))
          `(prefix ,(fh:pointer uint8)))))
(define-fh-compound-type yaml_tag_directive_t yaml_tag_directive_t-desc 
 yaml_tag_directive_t? make-yaml_tag_directive_t)
(export yaml_tag_directive_t yaml_tag_directive_t? make-yaml_tag_directive_t)
(define-public yaml_tag_directive_t*-desc
  (fh:pointer yaml_tag_directive_t-desc))
(define-fh-pointer-type yaml_tag_directive_t* yaml_tag_directive_t*-desc 
 yaml_tag_directive_t*? make-yaml_tag_directive_t*)
(export yaml_tag_directive_t* yaml_tag_directive_t*? 
 make-yaml_tag_directive_t*)
(ref<->deref!
  yaml_tag_directive_t*
  make-yaml_tag_directive_t*
  yaml_tag_directive_t
  make-yaml_tag_directive_t)
(define-public struct-yaml_tag_directive_s-desc
  yaml_tag_directive_t-desc)
(define-fh-compound-type struct-yaml_tag_directive_s 
 struct-yaml_tag_directive_s-desc struct-yaml_tag_directive_s? 
 make-struct-yaml_tag_directive_s)
(export struct-yaml_tag_directive_s struct-yaml_tag_directive_s? 
 make-struct-yaml_tag_directive_s)
(define-public struct-yaml_tag_directive_s*-desc
  yaml_tag_directive_t*-desc)
(define-fh-pointer-type struct-yaml_tag_directive_s* 
 struct-yaml_tag_directive_s*-desc struct-yaml_tag_directive_s*? 
 make-struct-yaml_tag_directive_s*)
(export struct-yaml_tag_directive_s* struct-yaml_tag_directive_s*? 
 make-struct-yaml_tag_directive_s*)
(ref<->deref!
  struct-yaml_tag_directive_s*
  make-struct-yaml_tag_directive_s*
  struct-yaml_tag_directive_s
  make-struct-yaml_tag_directive_s)

;; typedef enum yaml_encoding_e {
;;   YAML_ANY_ENCODING,
;;   YAML_UTF8_ENCODING,
;;   YAML_UTF16LE_ENCODING,
;;   YAML_UTF16BE_ENCODING,
;; } yaml_encoding_t;
(define yaml_encoding_t-enum-nvl
  '((YAML_ANY_ENCODING . 0)
    (YAML_UTF8_ENCODING . 1)
    (YAML_UTF16LE_ENCODING . 2)
    (YAML_UTF16BE_ENCODING . 3))
  )
(define yaml_encoding_t-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       yaml_encoding_t-enum-nvl))
(define-public (unwrap-yaml_encoding_t n)
  (cond
   ((symbol? n)
    (or (assq-ref yaml_encoding_t-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-yaml_encoding_t v)
  (assq-ref yaml_encoding_t-enum-vnl v))
(define-public unwrap-enum-yaml_encoding_e unwrap-yaml_encoding_t)
(define-public wrap-enum-yaml_encoding_e wrap-yaml_encoding_t)

;; enum yaml_encoding_e {
;;   YAML_ANY_ENCODING,
;;   YAML_UTF8_ENCODING,
;;   YAML_UTF16LE_ENCODING,
;;   YAML_UTF16BE_ENCODING,
;; };
(define enum-yaml_encoding_e-enum-nvl
  '((YAML_ANY_ENCODING . 0)
    (YAML_UTF8_ENCODING . 1)
    (YAML_UTF16LE_ENCODING . 2)
    (YAML_UTF16BE_ENCODING . 3))
  )
(define enum-yaml_encoding_e-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       enum-yaml_encoding_e-enum-nvl))
(define-public (unwrap-enum-yaml_encoding_e n)
  (cond
   ((symbol? n)
    (or (assq-ref enum-yaml_encoding_e-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-enum-yaml_encoding_e v)
  (assq-ref enum-yaml_encoding_e-enum-vnl v))

;; typedef enum yaml_break_e {
;;   YAML_ANY_BREAK,
;;   YAML_CR_BREAK,
;;   YAML_LN_BREAK,
;;   YAML_CRLN_BREAK,
;; } yaml_break_t;
(define yaml_break_t-enum-nvl
  '((YAML_ANY_BREAK . 0)
    (YAML_CR_BREAK . 1)
    (YAML_LN_BREAK . 2)
    (YAML_CRLN_BREAK . 3))
  )
(define yaml_break_t-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       yaml_break_t-enum-nvl))
(define-public (unwrap-yaml_break_t n)
  (cond
   ((symbol? n)
    (or (assq-ref yaml_break_t-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-yaml_break_t v)
  (assq-ref yaml_break_t-enum-vnl v))
(define-public unwrap-enum-yaml_break_e unwrap-yaml_break_t)
(define-public wrap-enum-yaml_break_e wrap-yaml_break_t)

;; enum yaml_break_e {
;;   YAML_ANY_BREAK,
;;   YAML_CR_BREAK,
;;   YAML_LN_BREAK,
;;   YAML_CRLN_BREAK,
;; };
(define enum-yaml_break_e-enum-nvl
  '((YAML_ANY_BREAK . 0)
    (YAML_CR_BREAK . 1)
    (YAML_LN_BREAK . 2)
    (YAML_CRLN_BREAK . 3))
  )
(define enum-yaml_break_e-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       enum-yaml_break_e-enum-nvl))
(define-public (unwrap-enum-yaml_break_e n)
  (cond
   ((symbol? n)
    (or (assq-ref enum-yaml_break_e-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-enum-yaml_break_e v)
  (assq-ref enum-yaml_break_e-enum-vnl v))

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
(define yaml_error_type_t-enum-nvl
  '((YAML_NO_ERROR . 0)
    (YAML_MEMORY_ERROR . 1)
    (YAML_READER_ERROR . 2)
    (YAML_SCANNER_ERROR . 3)
    (YAML_PARSER_ERROR . 4)
    (YAML_COMPOSER_ERROR . 5)
    (YAML_WRITER_ERROR . 6)
    (YAML_EMITTER_ERROR . 7))
  )
(define yaml_error_type_t-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       yaml_error_type_t-enum-nvl))
(define-public (unwrap-yaml_error_type_t n)
  (cond
   ((symbol? n)
    (or (assq-ref yaml_error_type_t-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-yaml_error_type_t v)
  (assq-ref yaml_error_type_t-enum-vnl v))
(define-public unwrap-enum-yaml_error_type_e unwrap-yaml_error_type_t)
(define-public wrap-enum-yaml_error_type_e wrap-yaml_error_type_t)

;; enum yaml_error_type_e {
;;   YAML_NO_ERROR,
;;   YAML_MEMORY_ERROR,
;;   YAML_READER_ERROR,
;;   YAML_SCANNER_ERROR,
;;   YAML_PARSER_ERROR,
;;   YAML_COMPOSER_ERROR,
;;   YAML_WRITER_ERROR,
;;   YAML_EMITTER_ERROR,
;; };
(define enum-yaml_error_type_e-enum-nvl
  '((YAML_NO_ERROR . 0)
    (YAML_MEMORY_ERROR . 1)
    (YAML_READER_ERROR . 2)
    (YAML_SCANNER_ERROR . 3)
    (YAML_PARSER_ERROR . 4)
    (YAML_COMPOSER_ERROR . 5)
    (YAML_WRITER_ERROR . 6)
    (YAML_EMITTER_ERROR . 7))
  )
(define enum-yaml_error_type_e-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       enum-yaml_error_type_e-enum-nvl))
(define-public (unwrap-enum-yaml_error_type_e n)
  (cond
   ((symbol? n)
    (or (assq-ref enum-yaml_error_type_e-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-enum-yaml_error_type_e v)
  (assq-ref enum-yaml_error_type_e-enum-vnl v))

;; typedef struct yaml_mark_s {
;;   /** The position index. */
;;   size_t index;
;;   /** The position line. */
;;   size_t line;
;;   /** The position column. */
;;   size_t column;
;; } yaml_mark_t;
(define-public yaml_mark_t-desc
  (bs:struct
    (list `(index ,size_t)
          `(line ,size_t)
          `(column ,size_t))))
(define-fh-compound-type yaml_mark_t yaml_mark_t-desc yaml_mark_t? 
 make-yaml_mark_t)
(export yaml_mark_t yaml_mark_t? make-yaml_mark_t)
(define-public yaml_mark_t*-desc
  (fh:pointer yaml_mark_t-desc))
(define-fh-pointer-type yaml_mark_t* yaml_mark_t*-desc yaml_mark_t*? 
 make-yaml_mark_t*)
(export yaml_mark_t* yaml_mark_t*? make-yaml_mark_t*)
(ref<->deref!
  yaml_mark_t*
  make-yaml_mark_t*
  yaml_mark_t
  make-yaml_mark_t)
(define-public struct-yaml_mark_s-desc
  yaml_mark_t-desc)
(define-fh-compound-type struct-yaml_mark_s struct-yaml_mark_s-desc 
 struct-yaml_mark_s? make-struct-yaml_mark_s)
(export struct-yaml_mark_s struct-yaml_mark_s? make-struct-yaml_mark_s)
(define-public struct-yaml_mark_s*-desc
  yaml_mark_t*-desc)
(define-fh-pointer-type struct-yaml_mark_s* struct-yaml_mark_s*-desc 
 struct-yaml_mark_s*? make-struct-yaml_mark_s*)
(export struct-yaml_mark_s* struct-yaml_mark_s*? make-struct-yaml_mark_s*)
(ref<->deref!
  struct-yaml_mark_s*
  make-struct-yaml_mark_s*
  struct-yaml_mark_s
  make-struct-yaml_mark_s)

;; typedef enum yaml_scalar_style_e {
;;   YAML_ANY_SCALAR_STYLE,
;;   YAML_PLAIN_SCALAR_STYLE,
;;   YAML_SINGLE_QUOTED_SCALAR_STYLE,
;;   YAML_DOUBLE_QUOTED_SCALAR_STYLE,
;;   YAML_LITERAL_SCALAR_STYLE,
;;   YAML_FOLDED_SCALAR_STYLE,
;; } yaml_scalar_style_t;
(define yaml_scalar_style_t-enum-nvl
  '((YAML_ANY_SCALAR_STYLE . 0)
    (YAML_PLAIN_SCALAR_STYLE . 1)
    (YAML_SINGLE_QUOTED_SCALAR_STYLE . 2)
    (YAML_DOUBLE_QUOTED_SCALAR_STYLE . 3)
    (YAML_LITERAL_SCALAR_STYLE . 4)
    (YAML_FOLDED_SCALAR_STYLE . 5))
  )
(define yaml_scalar_style_t-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       yaml_scalar_style_t-enum-nvl))
(define-public (unwrap-yaml_scalar_style_t n)
  (cond
   ((symbol? n)
    (or (assq-ref yaml_scalar_style_t-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-yaml_scalar_style_t v)
  (assq-ref yaml_scalar_style_t-enum-vnl v))
(define-public unwrap-enum-yaml_scalar_style_e unwrap-yaml_scalar_style_t)
(define-public wrap-enum-yaml_scalar_style_e wrap-yaml_scalar_style_t)

;; enum yaml_scalar_style_e {
;;   YAML_ANY_SCALAR_STYLE,
;;   YAML_PLAIN_SCALAR_STYLE,
;;   YAML_SINGLE_QUOTED_SCALAR_STYLE,
;;   YAML_DOUBLE_QUOTED_SCALAR_STYLE,
;;   YAML_LITERAL_SCALAR_STYLE,
;;   YAML_FOLDED_SCALAR_STYLE,
;; };
(define enum-yaml_scalar_style_e-enum-nvl
  '((YAML_ANY_SCALAR_STYLE . 0)
    (YAML_PLAIN_SCALAR_STYLE . 1)
    (YAML_SINGLE_QUOTED_SCALAR_STYLE . 2)
    (YAML_DOUBLE_QUOTED_SCALAR_STYLE . 3)
    (YAML_LITERAL_SCALAR_STYLE . 4)
    (YAML_FOLDED_SCALAR_STYLE . 5))
  )
(define enum-yaml_scalar_style_e-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       enum-yaml_scalar_style_e-enum-nvl))
(define-public (unwrap-enum-yaml_scalar_style_e n)
  (cond
   ((symbol? n)
    (or (assq-ref enum-yaml_scalar_style_e-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-enum-yaml_scalar_style_e v)
  (assq-ref enum-yaml_scalar_style_e-enum-vnl v))

;; typedef enum yaml_sequence_style_e {
;;   YAML_ANY_SEQUENCE_STYLE,
;;   YAML_BLOCK_SEQUENCE_STYLE,
;;   YAML_FLOW_SEQUENCE_STYLE,
;; } yaml_sequence_style_t;
(define yaml_sequence_style_t-enum-nvl
  '((YAML_ANY_SEQUENCE_STYLE . 0)
    (YAML_BLOCK_SEQUENCE_STYLE . 1)
    (YAML_FLOW_SEQUENCE_STYLE . 2))
  )
(define yaml_sequence_style_t-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       yaml_sequence_style_t-enum-nvl))
(define-public (unwrap-yaml_sequence_style_t n)
  (cond
   ((symbol? n)
    (or (assq-ref yaml_sequence_style_t-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-yaml_sequence_style_t v)
  (assq-ref yaml_sequence_style_t-enum-vnl v))
(define-public unwrap-enum-yaml_sequence_style_e unwrap-yaml_sequence_style_t)
(define-public wrap-enum-yaml_sequence_style_e wrap-yaml_sequence_style_t)

;; enum yaml_sequence_style_e {
;;   YAML_ANY_SEQUENCE_STYLE,
;;   YAML_BLOCK_SEQUENCE_STYLE,
;;   YAML_FLOW_SEQUENCE_STYLE,
;; };
(define enum-yaml_sequence_style_e-enum-nvl
  '((YAML_ANY_SEQUENCE_STYLE . 0)
    (YAML_BLOCK_SEQUENCE_STYLE . 1)
    (YAML_FLOW_SEQUENCE_STYLE . 2))
  )
(define enum-yaml_sequence_style_e-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       enum-yaml_sequence_style_e-enum-nvl))
(define-public (unwrap-enum-yaml_sequence_style_e n)
  (cond
   ((symbol? n)
    (or (assq-ref enum-yaml_sequence_style_e-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-enum-yaml_sequence_style_e v)
  (assq-ref enum-yaml_sequence_style_e-enum-vnl v))

;; typedef enum yaml_mapping_style_e {
;;   YAML_ANY_MAPPING_STYLE,
;;   YAML_BLOCK_MAPPING_STYLE,
;;   YAML_FLOW_MAPPING_STYLE,
;; } yaml_mapping_style_t;
(define yaml_mapping_style_t-enum-nvl
  '((YAML_ANY_MAPPING_STYLE . 0)
    (YAML_BLOCK_MAPPING_STYLE . 1)
    (YAML_FLOW_MAPPING_STYLE . 2))
  )
(define yaml_mapping_style_t-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       yaml_mapping_style_t-enum-nvl))
(define-public (unwrap-yaml_mapping_style_t n)
  (cond
   ((symbol? n)
    (or (assq-ref yaml_mapping_style_t-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-yaml_mapping_style_t v)
  (assq-ref yaml_mapping_style_t-enum-vnl v))
(define-public unwrap-enum-yaml_mapping_style_e unwrap-yaml_mapping_style_t)
(define-public wrap-enum-yaml_mapping_style_e wrap-yaml_mapping_style_t)

;; enum yaml_mapping_style_e {
;;   YAML_ANY_MAPPING_STYLE,
;;   YAML_BLOCK_MAPPING_STYLE,
;;   YAML_FLOW_MAPPING_STYLE,
;; };
(define enum-yaml_mapping_style_e-enum-nvl
  '((YAML_ANY_MAPPING_STYLE . 0)
    (YAML_BLOCK_MAPPING_STYLE . 1)
    (YAML_FLOW_MAPPING_STYLE . 2))
  )
(define enum-yaml_mapping_style_e-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       enum-yaml_mapping_style_e-enum-nvl))
(define-public (unwrap-enum-yaml_mapping_style_e n)
  (cond
   ((symbol? n)
    (or (assq-ref enum-yaml_mapping_style_e-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-enum-yaml_mapping_style_e v)
  (assq-ref enum-yaml_mapping_style_e-enum-vnl v))

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
(define yaml_token_type_t-enum-nvl
  '((YAML_NO_TOKEN . 0)
    (YAML_STREAM_START_TOKEN . 1)
    (YAML_STREAM_END_TOKEN . 2)
    (YAML_VERSION_DIRECTIVE_TOKEN . 3)
    (YAML_TAG_DIRECTIVE_TOKEN . 4)
    (YAML_DOCUMENT_START_TOKEN . 5)
    (YAML_DOCUMENT_END_TOKEN . 6)
    (YAML_BLOCK_SEQUENCE_START_TOKEN . 7)
    (YAML_BLOCK_MAPPING_START_TOKEN . 8)
    (YAML_BLOCK_END_TOKEN . 9)
    (YAML_FLOW_SEQUENCE_START_TOKEN . 10)
    (YAML_FLOW_SEQUENCE_END_TOKEN . 11)
    (YAML_FLOW_MAPPING_START_TOKEN . 12)
    (YAML_FLOW_MAPPING_END_TOKEN . 13)
    (YAML_BLOCK_ENTRY_TOKEN . 14)
    (YAML_FLOW_ENTRY_TOKEN . 15)
    (YAML_KEY_TOKEN . 16)
    (YAML_VALUE_TOKEN . 17)
    (YAML_ALIAS_TOKEN . 18)
    (YAML_ANCHOR_TOKEN . 19)
    (YAML_TAG_TOKEN . 20)
    (YAML_SCALAR_TOKEN . 21))
  )
(define yaml_token_type_t-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       yaml_token_type_t-enum-nvl))
(define-public (unwrap-yaml_token_type_t n)
  (cond
   ((symbol? n)
    (or (assq-ref yaml_token_type_t-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-yaml_token_type_t v)
  (assq-ref yaml_token_type_t-enum-vnl v))
(define-public unwrap-enum-yaml_token_type_e unwrap-yaml_token_type_t)
(define-public wrap-enum-yaml_token_type_e wrap-yaml_token_type_t)

;; enum yaml_token_type_e {
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
;; };
(define enum-yaml_token_type_e-enum-nvl
  '((YAML_NO_TOKEN . 0)
    (YAML_STREAM_START_TOKEN . 1)
    (YAML_STREAM_END_TOKEN . 2)
    (YAML_VERSION_DIRECTIVE_TOKEN . 3)
    (YAML_TAG_DIRECTIVE_TOKEN . 4)
    (YAML_DOCUMENT_START_TOKEN . 5)
    (YAML_DOCUMENT_END_TOKEN . 6)
    (YAML_BLOCK_SEQUENCE_START_TOKEN . 7)
    (YAML_BLOCK_MAPPING_START_TOKEN . 8)
    (YAML_BLOCK_END_TOKEN . 9)
    (YAML_FLOW_SEQUENCE_START_TOKEN . 10)
    (YAML_FLOW_SEQUENCE_END_TOKEN . 11)
    (YAML_FLOW_MAPPING_START_TOKEN . 12)
    (YAML_FLOW_MAPPING_END_TOKEN . 13)
    (YAML_BLOCK_ENTRY_TOKEN . 14)
    (YAML_FLOW_ENTRY_TOKEN . 15)
    (YAML_KEY_TOKEN . 16)
    (YAML_VALUE_TOKEN . 17)
    (YAML_ALIAS_TOKEN . 18)
    (YAML_ANCHOR_TOKEN . 19)
    (YAML_TAG_TOKEN . 20)
    (YAML_SCALAR_TOKEN . 21))
  )
(define enum-yaml_token_type_e-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       enum-yaml_token_type_e-enum-nvl))
(define-public (unwrap-enum-yaml_token_type_e n)
  (cond
   ((symbol? n)
    (or (assq-ref enum-yaml_token_type_e-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-enum-yaml_token_type_e v)
  (assq-ref enum-yaml_token_type_e-enum-vnl v))

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
(define-public yaml_token_t-desc
  (bs:struct
    (list `(type ,int)
          `(data ,(bs:union
                    (list `(stream_start
                             ,(bs:struct (list `(encoding ,int))))
                          `(alias ,(bs:struct
                                     (list `(value ,(fh:pointer uint8)))))
                          `(anchor
                             ,(bs:struct (list `(value ,(fh:pointer uint8)))))
                          `(tag ,(bs:struct
                                   (list `(handle ,(fh:pointer uint8))
                                         `(suffix ,(fh:pointer uint8)))))
                          `(scalar
                             ,(bs:struct
                                (list `(value ,(fh:pointer uint8))
                                      `(length ,size_t)
                                      `(style ,int))))
                          `(version_directive
                             ,(bs:struct (list `(major ,int) `(minor ,int))))
                          `(tag_directive
                             ,(bs:struct
                                (list `(handle ,(fh:pointer uint8))
                                      `(prefix ,(fh:pointer uint8))))))))
          `(start_mark ,yaml_mark_t-desc)
          `(end_mark ,yaml_mark_t-desc))))
(define-fh-compound-type yaml_token_t yaml_token_t-desc yaml_token_t? 
 make-yaml_token_t)
(export yaml_token_t yaml_token_t? make-yaml_token_t)
(define-public yaml_token_t*-desc
  (fh:pointer yaml_token_t-desc))
(define-fh-pointer-type yaml_token_t* yaml_token_t*-desc yaml_token_t*? 
 make-yaml_token_t*)
(export yaml_token_t* yaml_token_t*? make-yaml_token_t*)
(ref<->deref!
  yaml_token_t*
  make-yaml_token_t*
  yaml_token_t
  make-yaml_token_t)
(define-public struct-yaml_token_s-desc
  yaml_token_t-desc)
(define-fh-compound-type struct-yaml_token_s struct-yaml_token_s-desc 
 struct-yaml_token_s? make-struct-yaml_token_s)
(export struct-yaml_token_s struct-yaml_token_s? make-struct-yaml_token_s)
(define-public struct-yaml_token_s*-desc
  yaml_token_t*-desc)
(define-fh-pointer-type struct-yaml_token_s* struct-yaml_token_s*-desc 
 struct-yaml_token_s*? make-struct-yaml_token_s*)
(export struct-yaml_token_s* struct-yaml_token_s*? make-struct-yaml_token_s*)
(ref<->deref!
  struct-yaml_token_s*
  make-struct-yaml_token_s*
  struct-yaml_token_s
  make-struct-yaml_token_s)

;; void yaml_token_delete(yaml_token_t *token);
(define yaml_token_delete
  (let ((~yaml_token_delete
          (delay (fh-link-proc
                   ffi:void
                   "yaml_token_delete"
                   (list ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (token)
      (let ((~token ((fht-unwrap yaml_token_t*) token)))
        ((force ~yaml_token_delete) ~token)))))
(export yaml_token_delete)

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
(define yaml_event_type_t-enum-nvl
  '((YAML_NO_EVENT . 0)
    (YAML_STREAM_START_EVENT . 1)
    (YAML_STREAM_END_EVENT . 2)
    (YAML_DOCUMENT_START_EVENT . 3)
    (YAML_DOCUMENT_END_EVENT . 4)
    (YAML_ALIAS_EVENT . 5)
    (YAML_SCALAR_EVENT . 6)
    (YAML_SEQUENCE_START_EVENT . 7)
    (YAML_SEQUENCE_END_EVENT . 8)
    (YAML_MAPPING_START_EVENT . 9)
    (YAML_MAPPING_END_EVENT . 10))
  )
(define yaml_event_type_t-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       yaml_event_type_t-enum-nvl))
(define-public (unwrap-yaml_event_type_t n)
  (cond
   ((symbol? n)
    (or (assq-ref yaml_event_type_t-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-yaml_event_type_t v)
  (assq-ref yaml_event_type_t-enum-vnl v))
(define-public unwrap-enum-yaml_event_type_e unwrap-yaml_event_type_t)
(define-public wrap-enum-yaml_event_type_e wrap-yaml_event_type_t)

;; enum yaml_event_type_e {
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
;; };
(define enum-yaml_event_type_e-enum-nvl
  '((YAML_NO_EVENT . 0)
    (YAML_STREAM_START_EVENT . 1)
    (YAML_STREAM_END_EVENT . 2)
    (YAML_DOCUMENT_START_EVENT . 3)
    (YAML_DOCUMENT_END_EVENT . 4)
    (YAML_ALIAS_EVENT . 5)
    (YAML_SCALAR_EVENT . 6)
    (YAML_SEQUENCE_START_EVENT . 7)
    (YAML_SEQUENCE_END_EVENT . 8)
    (YAML_MAPPING_START_EVENT . 9)
    (YAML_MAPPING_END_EVENT . 10))
  )
(define enum-yaml_event_type_e-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       enum-yaml_event_type_e-enum-nvl))
(define-public (unwrap-enum-yaml_event_type_e n)
  (cond
   ((symbol? n)
    (or (assq-ref enum-yaml_event_type_e-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-enum-yaml_event_type_e v)
  (assq-ref enum-yaml_event_type_e-enum-vnl v))

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
(define-public yaml_event_t-desc
  (bs:struct
    (list `(type ,int)
          `(data ,(bs:union
                    (list `(stream_start
                             ,(bs:struct (list `(encoding ,int))))
                          `(document_start
                             ,(bs:struct
                                (list `(version_directive
                                         ,yaml_version_directive_t*-desc)
                                      `(tag_directives
                                         ,(bs:struct
                                            (list `(start ,yaml_tag_directive_t*-desc)
                                                  `(end ,yaml_tag_directive_t*-desc))))
                                      `(implicit ,int))))
                          `(document_end
                             ,(bs:struct (list `(implicit ,int))))
                          `(alias ,(bs:struct
                                     (list `(anchor ,(fh:pointer uint8)))))
                          `(scalar
                             ,(bs:struct
                                (list `(anchor ,(fh:pointer uint8))
                                      `(tag ,(fh:pointer uint8))
                                      `(value ,(fh:pointer uint8))
                                      `(length ,size_t)
                                      `(plain_implicit ,int)
                                      `(quoted_implicit ,int)
                                      `(style ,int))))
                          `(sequence_start
                             ,(bs:struct
                                (list `(anchor ,(fh:pointer uint8))
                                      `(tag ,(fh:pointer uint8))
                                      `(implicit ,int)
                                      `(style ,int))))
                          `(mapping_start
                             ,(bs:struct
                                (list `(anchor ,(fh:pointer uint8))
                                      `(tag ,(fh:pointer uint8))
                                      `(implicit ,int)
                                      `(style ,int)))))))
          `(start_mark ,yaml_mark_t-desc)
          `(end_mark ,yaml_mark_t-desc))))
(define-fh-compound-type yaml_event_t yaml_event_t-desc yaml_event_t? 
 make-yaml_event_t)
(export yaml_event_t yaml_event_t? make-yaml_event_t)
(define-public yaml_event_t*-desc
  (fh:pointer yaml_event_t-desc))
(define-fh-pointer-type yaml_event_t* yaml_event_t*-desc yaml_event_t*? 
 make-yaml_event_t*)
(export yaml_event_t* yaml_event_t*? make-yaml_event_t*)
(ref<->deref!
  yaml_event_t*
  make-yaml_event_t*
  yaml_event_t
  make-yaml_event_t)
(define-public struct-yaml_event_s-desc
  yaml_event_t-desc)
(define-fh-compound-type struct-yaml_event_s struct-yaml_event_s-desc 
 struct-yaml_event_s? make-struct-yaml_event_s)
(export struct-yaml_event_s struct-yaml_event_s? make-struct-yaml_event_s)
(define-public struct-yaml_event_s*-desc
  yaml_event_t*-desc)
(define-fh-pointer-type struct-yaml_event_s* struct-yaml_event_s*-desc 
 struct-yaml_event_s*? make-struct-yaml_event_s*)
(export struct-yaml_event_s* struct-yaml_event_s*? make-struct-yaml_event_s*)
(ref<->deref!
  struct-yaml_event_s*
  make-struct-yaml_event_s*
  struct-yaml_event_s
  make-struct-yaml_event_s)

;; int yaml_stream_start_event_initialize(yaml_event_t *event, yaml_encoding_t 
;;     encoding);
(define yaml_stream_start_event_initialize
  (let ((~yaml_stream_start_event_initialize
          (delay (fh-link-proc
                   ffi:int
                   "yaml_stream_start_event_initialize"
                   (list ffi-void* ffi:int)
                   yaml-libyaml-llibs))))
    (lambda (event encoding)
      (let ((~event ((fht-unwrap yaml_event_t*) event))
            (~encoding (unwrap-yaml_encoding_t encoding)))
        ((force ~yaml_stream_start_event_initialize)
         ~event
         ~encoding)))))
(export yaml_stream_start_event_initialize)

;; int yaml_stream_end_event_initialize(yaml_event_t *event);
(define yaml_stream_end_event_initialize
  (let ((~yaml_stream_end_event_initialize
          (delay (fh-link-proc
                   ffi:int
                   "yaml_stream_end_event_initialize"
                   (list ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (event)
      (let ((~event ((fht-unwrap yaml_event_t*) event)))
        ((force ~yaml_stream_end_event_initialize)
         ~event)))))
(export yaml_stream_end_event_initialize)

;; int yaml_document_start_event_initialize(yaml_event_t *event, 
;;     yaml_version_directive_t *version_directive, yaml_tag_directive_t *
;;     tag_directives_start, yaml_tag_directive_t *tag_directives_end, int 
;;     implicit);
(define yaml_document_start_event_initialize
  (let ((~yaml_document_start_event_initialize
          (delay (fh-link-proc
                   ffi:int
                   "yaml_document_start_event_initialize"
                   (list ffi-void*
                         ffi-void*
                         ffi-void*
                         ffi-void*
                         ffi:int)
                   yaml-libyaml-llibs))))
    (lambda (event
             version_directive
             tag_directives_start
             tag_directives_end
             implicit)
      (let ((~event ((fht-unwrap yaml_event_t*) event))
            (~version_directive
              ((fht-unwrap yaml_version_directive_t*)
               version_directive))
            (~tag_directives_start
              ((fht-unwrap yaml_tag_directive_t*)
               tag_directives_start))
            (~tag_directives_end
              ((fht-unwrap yaml_tag_directive_t*)
               tag_directives_end))
            (~implicit (unwrap~fixed implicit)))
        ((force ~yaml_document_start_event_initialize)
         ~event
         ~version_directive
         ~tag_directives_start
         ~tag_directives_end
         ~implicit)))))
(export yaml_document_start_event_initialize)

;; int yaml_document_end_event_initialize(yaml_event_t *event, int implicit);
(define yaml_document_end_event_initialize
  (let ((~yaml_document_end_event_initialize
          (delay (fh-link-proc
                   ffi:int
                   "yaml_document_end_event_initialize"
                   (list ffi-void* ffi:int)
                   yaml-libyaml-llibs))))
    (lambda (event implicit)
      (let ((~event ((fht-unwrap yaml_event_t*) event))
            (~implicit (unwrap~fixed implicit)))
        ((force ~yaml_document_end_event_initialize)
         ~event
         ~implicit)))))
(export yaml_document_end_event_initialize)

;; int yaml_alias_event_initialize(yaml_event_t *event, yaml_char_t *anchor);
(define yaml_alias_event_initialize
  (let ((~yaml_alias_event_initialize
          (delay (fh-link-proc
                   ffi:int
                   "yaml_alias_event_initialize"
                   (list ffi-void* ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (event anchor)
      (let ((~event ((fht-unwrap yaml_event_t*) event))
            (~anchor (unwrap~pointer anchor)))
        ((force ~yaml_alias_event_initialize)
         ~event
         ~anchor)))))
(export yaml_alias_event_initialize)

;; int yaml_scalar_event_initialize(yaml_event_t *event, yaml_char_t *anchor, 
;;     yaml_char_t *tag, yaml_char_t *value, int length, int plain_implicit, 
;;     int quoted_implicit, yaml_scalar_style_t style);
(define yaml_scalar_event_initialize
  (let ((~yaml_scalar_event_initialize
          (delay (fh-link-proc
                   ffi:int
                   "yaml_scalar_event_initialize"
                   (list ffi-void*
                         ffi-void*
                         ffi-void*
                         ffi-void*
                         ffi:int
                         ffi:int
                         ffi:int
                         ffi:int)
                   yaml-libyaml-llibs))))
    (lambda (event
             anchor
             tag
             value
             length
             plain_implicit
             quoted_implicit
             style)
      (let ((~event ((fht-unwrap yaml_event_t*) event))
            (~anchor (unwrap~pointer anchor))
            (~tag (unwrap~pointer tag))
            (~value (unwrap~pointer value))
            (~length (unwrap~fixed length))
            (~plain_implicit (unwrap~fixed plain_implicit))
            (~quoted_implicit (unwrap~fixed quoted_implicit))
            (~style (unwrap-yaml_scalar_style_t style)))
        ((force ~yaml_scalar_event_initialize)
         ~event
         ~anchor
         ~tag
         ~value
         ~length
         ~plain_implicit
         ~quoted_implicit
         ~style)))))
(export yaml_scalar_event_initialize)

;; int yaml_sequence_start_event_initialize(yaml_event_t *event, yaml_char_t *
;;     anchor, yaml_char_t *tag, int implicit, yaml_sequence_style_t style);
(define yaml_sequence_start_event_initialize
  (let ((~yaml_sequence_start_event_initialize
          (delay (fh-link-proc
                   ffi:int
                   "yaml_sequence_start_event_initialize"
                   (list ffi-void*
                         ffi-void*
                         ffi-void*
                         ffi:int
                         ffi:int)
                   yaml-libyaml-llibs))))
    (lambda (event anchor tag implicit style)
      (let ((~event ((fht-unwrap yaml_event_t*) event))
            (~anchor (unwrap~pointer anchor))
            (~tag (unwrap~pointer tag))
            (~implicit (unwrap~fixed implicit))
            (~style (unwrap-yaml_sequence_style_t style)))
        ((force ~yaml_sequence_start_event_initialize)
         ~event
         ~anchor
         ~tag
         ~implicit
         ~style)))))
(export yaml_sequence_start_event_initialize)

;; int yaml_sequence_end_event_initialize(yaml_event_t *event);
(define yaml_sequence_end_event_initialize
  (let ((~yaml_sequence_end_event_initialize
          (delay (fh-link-proc
                   ffi:int
                   "yaml_sequence_end_event_initialize"
                   (list ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (event)
      (let ((~event ((fht-unwrap yaml_event_t*) event)))
        ((force ~yaml_sequence_end_event_initialize)
         ~event)))))
(export yaml_sequence_end_event_initialize)

;; int yaml_mapping_start_event_initialize(yaml_event_t *event, yaml_char_t *
;;     anchor, yaml_char_t *tag, int implicit, yaml_mapping_style_t style);
(define yaml_mapping_start_event_initialize
  (let ((~yaml_mapping_start_event_initialize
          (delay (fh-link-proc
                   ffi:int
                   "yaml_mapping_start_event_initialize"
                   (list ffi-void*
                         ffi-void*
                         ffi-void*
                         ffi:int
                         ffi:int)
                   yaml-libyaml-llibs))))
    (lambda (event anchor tag implicit style)
      (let ((~event ((fht-unwrap yaml_event_t*) event))
            (~anchor (unwrap~pointer anchor))
            (~tag (unwrap~pointer tag))
            (~implicit (unwrap~fixed implicit))
            (~style (unwrap-yaml_mapping_style_t style)))
        ((force ~yaml_mapping_start_event_initialize)
         ~event
         ~anchor
         ~tag
         ~implicit
         ~style)))))
(export yaml_mapping_start_event_initialize)

;; int yaml_mapping_end_event_initialize(yaml_event_t *event);
(define yaml_mapping_end_event_initialize
  (let ((~yaml_mapping_end_event_initialize
          (delay (fh-link-proc
                   ffi:int
                   "yaml_mapping_end_event_initialize"
                   (list ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (event)
      (let ((~event ((fht-unwrap yaml_event_t*) event)))
        ((force ~yaml_mapping_end_event_initialize)
         ~event)))))
(export yaml_mapping_end_event_initialize)

;; void yaml_event_delete(yaml_event_t *event);
(define yaml_event_delete
  (let ((~yaml_event_delete
          (delay (fh-link-proc
                   ffi:void
                   "yaml_event_delete"
                   (list ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (event)
      (let ((~event ((fht-unwrap yaml_event_t*) event)))
        ((force ~yaml_event_delete) ~event)))))
(export yaml_event_delete)

;; typedef enum yaml_node_type_e {
;;   YAML_NO_NODE,
;;   YAML_SCALAR_NODE,
;;   YAML_SEQUENCE_NODE,
;;   YAML_MAPPING_NODE,
;; } yaml_node_type_t;
(define yaml_node_type_t-enum-nvl
  '((YAML_NO_NODE . 0)
    (YAML_SCALAR_NODE . 1)
    (YAML_SEQUENCE_NODE . 2)
    (YAML_MAPPING_NODE . 3))
  )
(define yaml_node_type_t-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       yaml_node_type_t-enum-nvl))
(define-public (unwrap-yaml_node_type_t n)
  (cond
   ((symbol? n)
    (or (assq-ref yaml_node_type_t-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-yaml_node_type_t v)
  (assq-ref yaml_node_type_t-enum-vnl v))
(define-public unwrap-enum-yaml_node_type_e unwrap-yaml_node_type_t)
(define-public wrap-enum-yaml_node_type_e wrap-yaml_node_type_t)

;; enum yaml_node_type_e {
;;   YAML_NO_NODE,
;;   YAML_SCALAR_NODE,
;;   YAML_SEQUENCE_NODE,
;;   YAML_MAPPING_NODE,
;; };
(define enum-yaml_node_type_e-enum-nvl
  '((YAML_NO_NODE . 0)
    (YAML_SCALAR_NODE . 1)
    (YAML_SEQUENCE_NODE . 2)
    (YAML_MAPPING_NODE . 3))
  )
(define enum-yaml_node_type_e-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       enum-yaml_node_type_e-enum-nvl))
(define-public (unwrap-enum-yaml_node_type_e n)
  (cond
   ((symbol? n)
    (or (assq-ref enum-yaml_node_type_e-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-enum-yaml_node_type_e v)
  (assq-ref enum-yaml_node_type_e-enum-vnl v))

;; typedef struct yaml_node_s yaml_node_t;
(define-public yaml_node_t-desc 'void)
(define-public yaml_node_t fh-void)
(define-public yaml_node_t? fh-void?)
(define-public make-yaml_node_t make-fh-void)
(define-public yaml_node_t*-desc (fh:pointer (delay yaml_node_t-desc)))
(define-fh-pointer-type yaml_node_t* yaml_node_t*-desc yaml_node_t*? 
 make-yaml_node_t*)
(export yaml_node_t* yaml_node_t*? make-yaml_node_t*)

;; typedef int yaml_node_item_t;
(define-public yaml_node_item_t-desc int)

;; typedef struct yaml_node_pair_s {
;;   /** The key of the element. */
;;   int key;
;;   /** The value of the element. */
;;   int value;
;; } yaml_node_pair_t;
(define-public yaml_node_pair_t-desc
  (bs:struct (list `(key ,int) `(value ,int))))
(define-fh-compound-type yaml_node_pair_t yaml_node_pair_t-desc 
 yaml_node_pair_t? make-yaml_node_pair_t)
(export yaml_node_pair_t yaml_node_pair_t? make-yaml_node_pair_t)
(define-public yaml_node_pair_t*-desc
  (fh:pointer yaml_node_pair_t-desc))
(define-fh-pointer-type yaml_node_pair_t* yaml_node_pair_t*-desc 
 yaml_node_pair_t*? make-yaml_node_pair_t*)
(export yaml_node_pair_t* yaml_node_pair_t*? make-yaml_node_pair_t*)
(ref<->deref!
  yaml_node_pair_t*
  make-yaml_node_pair_t*
  yaml_node_pair_t
  make-yaml_node_pair_t)
(define-public struct-yaml_node_pair_s-desc
  yaml_node_pair_t-desc)
(define-fh-compound-type struct-yaml_node_pair_s struct-yaml_node_pair_s-desc 
 struct-yaml_node_pair_s? make-struct-yaml_node_pair_s)
(export struct-yaml_node_pair_s struct-yaml_node_pair_s? 
 make-struct-yaml_node_pair_s)
(define-public struct-yaml_node_pair_s*-desc
  yaml_node_pair_t*-desc)
(define-fh-pointer-type struct-yaml_node_pair_s* struct-yaml_node_pair_s*-desc
 struct-yaml_node_pair_s*? make-struct-yaml_node_pair_s*)
(export struct-yaml_node_pair_s* struct-yaml_node_pair_s*? 
 make-struct-yaml_node_pair_s*)
(ref<->deref!
  struct-yaml_node_pair_s*
  make-struct-yaml_node_pair_s*
  struct-yaml_node_pair_s
  make-struct-yaml_node_pair_s)

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
(define-public struct-yaml_node_s-desc
  (bs:struct
    (list `(type ,int)
          `(tag ,(fh:pointer uint8))
          `(data ,(bs:union
                    (list `(scalar
                             ,(bs:struct
                                (list `(value ,(fh:pointer uint8))
                                      `(length ,size_t)
                                      `(style ,int))))
                          `(sequence
                             ,(bs:struct
                                (list `(items ,(bs:struct
                                                 (list `(start ,(fh:pointer
                                                                  int))
                                                       `(end ,(fh:pointer int))
                                                       `(top ,(fh:pointer
                                                                int)))))
                                      `(style ,int))))
                          `(mapping
                             ,(bs:struct
                                (list `(pairs ,(bs:struct
                                                 (list `(start ,yaml_node_pair_t*-desc)
                                                       `(end ,yaml_node_pair_t*-desc)
                                                       `(top ,yaml_node_pair_t*-desc))))
                                      `(style ,int)))))))
          `(start_mark ,yaml_mark_t-desc)
          `(end_mark ,yaml_mark_t-desc))))
(define-fh-compound-type struct-yaml_node_s struct-yaml_node_s-desc 
 struct-yaml_node_s? make-struct-yaml_node_s)
(export struct-yaml_node_s struct-yaml_node_s? make-struct-yaml_node_s)
(define-public struct-yaml_node_s*-desc
  (fh:pointer struct-yaml_node_s-desc))
(define-fh-pointer-type struct-yaml_node_s* struct-yaml_node_s*-desc 
 struct-yaml_node_s*? make-struct-yaml_node_s*)
(export struct-yaml_node_s* struct-yaml_node_s*? make-struct-yaml_node_s*)
(ref<->deref!
  struct-yaml_node_s*
  make-struct-yaml_node_s*
  struct-yaml_node_s
  make-struct-yaml_node_s)
(set! yaml_node_t-desc struct-yaml_node_s-desc)
(define-fh-compound-type yaml_node_t yaml_node_t-desc yaml_node_t? 
 make-yaml_node_t)
(export yaml_node_t yaml_node_t? make-yaml_node_t)
(define-fh-pointer-type yaml_node_t* yaml_node_t*-desc yaml_node_t*? 
 make-yaml_node_t*)
(export yaml_node_t* yaml_node_t*? make-yaml_node_t*)
(ref<->deref!
  yaml_node_t*
  make-yaml_node_t*
  yaml_node_t
  make-yaml_node_t)

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
(define-public yaml_document_t-desc
  (bs:struct
    (list `(nodes ,(bs:struct
                     (list `(start ,yaml_node_t*-desc)
                           `(end ,yaml_node_t*-desc)
                           `(top ,yaml_node_t*-desc))))
          `(version_directive
             ,yaml_version_directive_t*-desc)
          `(tag_directives
             ,(bs:struct
                (list `(start ,yaml_tag_directive_t*-desc)
                      `(end ,yaml_tag_directive_t*-desc))))
          `(start_implicit ,int)
          `(end_implicit ,int)
          `(start_mark ,yaml_mark_t-desc)
          `(end_mark ,yaml_mark_t-desc))))
(define-fh-compound-type yaml_document_t yaml_document_t-desc yaml_document_t?
 make-yaml_document_t)
(export yaml_document_t yaml_document_t? make-yaml_document_t)
(define-public yaml_document_t*-desc
  (fh:pointer yaml_document_t-desc))
(define-fh-pointer-type yaml_document_t* yaml_document_t*-desc 
 yaml_document_t*? make-yaml_document_t*)
(export yaml_document_t* yaml_document_t*? make-yaml_document_t*)
(ref<->deref!
  yaml_document_t*
  make-yaml_document_t*
  yaml_document_t
  make-yaml_document_t)
(define-public struct-yaml_document_s-desc
  yaml_document_t-desc)
(define-fh-compound-type struct-yaml_document_s struct-yaml_document_s-desc 
 struct-yaml_document_s? make-struct-yaml_document_s)
(export struct-yaml_document_s struct-yaml_document_s? 
 make-struct-yaml_document_s)
(define-public struct-yaml_document_s*-desc
  yaml_document_t*-desc)
(define-fh-pointer-type struct-yaml_document_s* struct-yaml_document_s*-desc 
 struct-yaml_document_s*? make-struct-yaml_document_s*)
(export struct-yaml_document_s* struct-yaml_document_s*? 
 make-struct-yaml_document_s*)
(ref<->deref!
  struct-yaml_document_s*
  make-struct-yaml_document_s*
  struct-yaml_document_s
  make-struct-yaml_document_s)

;; int yaml_document_initialize(yaml_document_t *document, 
;;     yaml_version_directive_t *version_directive, yaml_tag_directive_t *
;;     tag_directives_start, yaml_tag_directive_t *tag_directives_end, int 
;;     start_implicit, int end_implicit);
(define yaml_document_initialize
  (let ((~yaml_document_initialize
          (delay (fh-link-proc
                   ffi:int
                   "yaml_document_initialize"
                   (list ffi-void*
                         ffi-void*
                         ffi-void*
                         ffi-void*
                         ffi:int
                         ffi:int)
                   yaml-libyaml-llibs))))
    (lambda (document
             version_directive
             tag_directives_start
             tag_directives_end
             start_implicit
             end_implicit)
      (let ((~document
              ((fht-unwrap yaml_document_t*) document))
            (~version_directive
              ((fht-unwrap yaml_version_directive_t*)
               version_directive))
            (~tag_directives_start
              ((fht-unwrap yaml_tag_directive_t*)
               tag_directives_start))
            (~tag_directives_end
              ((fht-unwrap yaml_tag_directive_t*)
               tag_directives_end))
            (~start_implicit (unwrap~fixed start_implicit))
            (~end_implicit (unwrap~fixed end_implicit)))
        ((force ~yaml_document_initialize)
         ~document
         ~version_directive
         ~tag_directives_start
         ~tag_directives_end
         ~start_implicit
         ~end_implicit)))))
(export yaml_document_initialize)

;; void yaml_document_delete(yaml_document_t *document);
(define yaml_document_delete
  (let ((~yaml_document_delete
          (delay (fh-link-proc
                   ffi:void
                   "yaml_document_delete"
                   (list ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (document)
      (let ((~document
              ((fht-unwrap yaml_document_t*) document)))
        ((force ~yaml_document_delete) ~document)))))
(export yaml_document_delete)

;; yaml_node_t *yaml_document_get_node(yaml_document_t *document, int index);
(define yaml_document_get_node
  (let ((~yaml_document_get_node
          (delay (fh-link-proc
                   ffi-void*
                   "yaml_document_get_node"
                   (list ffi-void* ffi:int)
                   yaml-libyaml-llibs))))
    (lambda (document index)
      (let ((~document
              ((fht-unwrap yaml_document_t*) document))
            (~index (unwrap~fixed index)))
        ((fht-wrap yaml_node_t*)
         ((force ~yaml_document_get_node)
          ~document
          ~index))))))
(export yaml_document_get_node)

;; yaml_node_t *yaml_document_get_root_node(yaml_document_t *document);
(define yaml_document_get_root_node
  (let ((~yaml_document_get_root_node
          (delay (fh-link-proc
                   ffi-void*
                   "yaml_document_get_root_node"
                   (list ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (document)
      (let ((~document
              ((fht-unwrap yaml_document_t*) document)))
        ((fht-wrap yaml_node_t*)
         ((force ~yaml_document_get_root_node) ~document))))))
(export yaml_document_get_root_node)

;; int yaml_document_add_scalar(yaml_document_t *document, yaml_char_t *tag, 
;;     yaml_char_t *value, int length, yaml_scalar_style_t style);
(define yaml_document_add_scalar
  (let ((~yaml_document_add_scalar
          (delay (fh-link-proc
                   ffi:int
                   "yaml_document_add_scalar"
                   (list ffi-void*
                         ffi-void*
                         ffi-void*
                         ffi:int
                         ffi:int)
                   yaml-libyaml-llibs))))
    (lambda (document tag value length style)
      (let ((~document
              ((fht-unwrap yaml_document_t*) document))
            (~tag (unwrap~pointer tag))
            (~value (unwrap~pointer value))
            (~length (unwrap~fixed length))
            (~style (unwrap-yaml_scalar_style_t style)))
        ((force ~yaml_document_add_scalar)
         ~document
         ~tag
         ~value
         ~length
         ~style)))))
(export yaml_document_add_scalar)

;; int yaml_document_add_sequence(yaml_document_t *document, yaml_char_t *tag, 
;;     yaml_sequence_style_t style);
(define yaml_document_add_sequence
  (let ((~yaml_document_add_sequence
          (delay (fh-link-proc
                   ffi:int
                   "yaml_document_add_sequence"
                   (list ffi-void* ffi-void* ffi:int)
                   yaml-libyaml-llibs))))
    (lambda (document tag style)
      (let ((~document
              ((fht-unwrap yaml_document_t*) document))
            (~tag (unwrap~pointer tag))
            (~style (unwrap-yaml_sequence_style_t style)))
        ((force ~yaml_document_add_sequence)
         ~document
         ~tag
         ~style)))))
(export yaml_document_add_sequence)

;; int yaml_document_add_mapping(yaml_document_t *document, yaml_char_t *tag, 
;;     yaml_mapping_style_t style);
(define yaml_document_add_mapping
  (let ((~yaml_document_add_mapping
          (delay (fh-link-proc
                   ffi:int
                   "yaml_document_add_mapping"
                   (list ffi-void* ffi-void* ffi:int)
                   yaml-libyaml-llibs))))
    (lambda (document tag style)
      (let ((~document
              ((fht-unwrap yaml_document_t*) document))
            (~tag (unwrap~pointer tag))
            (~style (unwrap-yaml_mapping_style_t style)))
        ((force ~yaml_document_add_mapping)
         ~document
         ~tag
         ~style)))))
(export yaml_document_add_mapping)

;; int yaml_document_append_sequence_item(yaml_document_t *document, int 
;;     sequence, int item);
(define yaml_document_append_sequence_item
  (let ((~yaml_document_append_sequence_item
          (delay (fh-link-proc
                   ffi:int
                   "yaml_document_append_sequence_item"
                   (list ffi-void* ffi:int ffi:int)
                   yaml-libyaml-llibs))))
    (lambda (document sequence item)
      (let ((~document
              ((fht-unwrap yaml_document_t*) document))
            (~sequence (unwrap~fixed sequence))
            (~item (unwrap~fixed item)))
        ((force ~yaml_document_append_sequence_item)
         ~document
         ~sequence
         ~item)))))
(export yaml_document_append_sequence_item)

;; int yaml_document_append_mapping_pair(yaml_document_t *document, int mapping
;;     , int key, int value);
(define yaml_document_append_mapping_pair
  (let ((~yaml_document_append_mapping_pair
          (delay (fh-link-proc
                   ffi:int
                   "yaml_document_append_mapping_pair"
                   (list ffi-void* ffi:int ffi:int ffi:int)
                   yaml-libyaml-llibs))))
    (lambda (document mapping key value)
      (let ((~document
              ((fht-unwrap yaml_document_t*) document))
            (~mapping (unwrap~fixed mapping))
            (~key (unwrap~fixed key))
            (~value (unwrap~fixed value)))
        ((force ~yaml_document_append_mapping_pair)
         ~document
         ~mapping
         ~key
         ~value)))))
(export yaml_document_append_mapping_pair)

;; typedef int yaml_read_handler_t(void *data, unsigned char *buffer, size_t 
;;     size, size_t *size_read);
(define yaml_read_handler_t
  (let ((~yaml_read_handler_t
          (delay (fh-link-proc
                   ffi:int
                   "yaml_read_handler_t"
                   (list ffi-void* ffi-void* ffi:size_t ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (data buffer size size_read)
      (let ((~data (unwrap~pointer data))
            (~buffer (unwrap~pointer buffer))
            (~size (unwrap~fixed size))
            (~size_read (unwrap~pointer size_read)))
        ((force ~yaml_read_handler_t)
         ~data
         ~buffer
         ~size
         ~size_read)))))
(export yaml_read_handler_t)

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
(define-public yaml_simple_key_t-desc
  (bs:struct
    (list `(possible ,int)
          `(required ,int)
          `(token_number ,size_t)
          `(mark ,yaml_mark_t-desc))))
(define-fh-compound-type yaml_simple_key_t yaml_simple_key_t-desc 
 yaml_simple_key_t? make-yaml_simple_key_t)
(export yaml_simple_key_t yaml_simple_key_t? make-yaml_simple_key_t)
(define-public yaml_simple_key_t*-desc
  (fh:pointer yaml_simple_key_t-desc))
(define-fh-pointer-type yaml_simple_key_t* yaml_simple_key_t*-desc 
 yaml_simple_key_t*? make-yaml_simple_key_t*)
(export yaml_simple_key_t* yaml_simple_key_t*? make-yaml_simple_key_t*)
(ref<->deref!
  yaml_simple_key_t*
  make-yaml_simple_key_t*
  yaml_simple_key_t
  make-yaml_simple_key_t)
(define-public struct-yaml_simple_key_s-desc
  yaml_simple_key_t-desc)
(define-fh-compound-type struct-yaml_simple_key_s 
 struct-yaml_simple_key_s-desc struct-yaml_simple_key_s? 
 make-struct-yaml_simple_key_s)
(export struct-yaml_simple_key_s struct-yaml_simple_key_s? 
 make-struct-yaml_simple_key_s)
(define-public struct-yaml_simple_key_s*-desc
  yaml_simple_key_t*-desc)
(define-fh-pointer-type struct-yaml_simple_key_s* 
 struct-yaml_simple_key_s*-desc struct-yaml_simple_key_s*? 
 make-struct-yaml_simple_key_s*)
(export struct-yaml_simple_key_s* struct-yaml_simple_key_s*? 
 make-struct-yaml_simple_key_s*)
(ref<->deref!
  struct-yaml_simple_key_s*
  make-struct-yaml_simple_key_s*
  struct-yaml_simple_key_s
  make-struct-yaml_simple_key_s)

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
(define yaml_parser_state_t-enum-nvl
  '((YAML_PARSE_STREAM_START_STATE . 0)
    (YAML_PARSE_IMPLICIT_DOCUMENT_START_STATE . 1)
    (YAML_PARSE_DOCUMENT_START_STATE . 2)
    (YAML_PARSE_DOCUMENT_CONTENT_STATE . 3)
    (YAML_PARSE_DOCUMENT_END_STATE . 4)
    (YAML_PARSE_BLOCK_NODE_STATE . 5)
    (YAML_PARSE_BLOCK_NODE_OR_INDENTLESS_SEQUENCE_STATE
      .
      6)
    (YAML_PARSE_FLOW_NODE_STATE . 7)
    (YAML_PARSE_BLOCK_SEQUENCE_FIRST_ENTRY_STATE . 8)
    (YAML_PARSE_BLOCK_SEQUENCE_ENTRY_STATE . 9)
    (YAML_PARSE_INDENTLESS_SEQUENCE_ENTRY_STATE . 10)
    (YAML_PARSE_BLOCK_MAPPING_FIRST_KEY_STATE . 11)
    (YAML_PARSE_BLOCK_MAPPING_KEY_STATE . 12)
    (YAML_PARSE_BLOCK_MAPPING_VALUE_STATE . 13)
    (YAML_PARSE_FLOW_SEQUENCE_FIRST_ENTRY_STATE . 14)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_STATE . 15)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_KEY_STATE
      .
      16)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_VALUE_STATE
      .
      17)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_END_STATE
      .
      18)
    (YAML_PARSE_FLOW_MAPPING_FIRST_KEY_STATE . 19)
    (YAML_PARSE_FLOW_MAPPING_KEY_STATE . 20)
    (YAML_PARSE_FLOW_MAPPING_VALUE_STATE . 21)
    (YAML_PARSE_FLOW_MAPPING_EMPTY_VALUE_STATE . 22)
    (YAML_PARSE_END_STATE . 23))
  )
(define yaml_parser_state_t-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       yaml_parser_state_t-enum-nvl))
(define-public (unwrap-yaml_parser_state_t n)
  (cond
   ((symbol? n)
    (or (assq-ref yaml_parser_state_t-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-yaml_parser_state_t v)
  (assq-ref yaml_parser_state_t-enum-vnl v))
(define-public unwrap-enum-yaml_parser_state_e unwrap-yaml_parser_state_t)
(define-public wrap-enum-yaml_parser_state_e wrap-yaml_parser_state_t)

;; enum yaml_parser_state_e {
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
;; };
(define enum-yaml_parser_state_e-enum-nvl
  '((YAML_PARSE_STREAM_START_STATE . 0)
    (YAML_PARSE_IMPLICIT_DOCUMENT_START_STATE . 1)
    (YAML_PARSE_DOCUMENT_START_STATE . 2)
    (YAML_PARSE_DOCUMENT_CONTENT_STATE . 3)
    (YAML_PARSE_DOCUMENT_END_STATE . 4)
    (YAML_PARSE_BLOCK_NODE_STATE . 5)
    (YAML_PARSE_BLOCK_NODE_OR_INDENTLESS_SEQUENCE_STATE
      .
      6)
    (YAML_PARSE_FLOW_NODE_STATE . 7)
    (YAML_PARSE_BLOCK_SEQUENCE_FIRST_ENTRY_STATE . 8)
    (YAML_PARSE_BLOCK_SEQUENCE_ENTRY_STATE . 9)
    (YAML_PARSE_INDENTLESS_SEQUENCE_ENTRY_STATE . 10)
    (YAML_PARSE_BLOCK_MAPPING_FIRST_KEY_STATE . 11)
    (YAML_PARSE_BLOCK_MAPPING_KEY_STATE . 12)
    (YAML_PARSE_BLOCK_MAPPING_VALUE_STATE . 13)
    (YAML_PARSE_FLOW_SEQUENCE_FIRST_ENTRY_STATE . 14)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_STATE . 15)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_KEY_STATE
      .
      16)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_VALUE_STATE
      .
      17)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_END_STATE
      .
      18)
    (YAML_PARSE_FLOW_MAPPING_FIRST_KEY_STATE . 19)
    (YAML_PARSE_FLOW_MAPPING_KEY_STATE . 20)
    (YAML_PARSE_FLOW_MAPPING_VALUE_STATE . 21)
    (YAML_PARSE_FLOW_MAPPING_EMPTY_VALUE_STATE . 22)
    (YAML_PARSE_END_STATE . 23))
  )
(define enum-yaml_parser_state_e-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       enum-yaml_parser_state_e-enum-nvl))
(define-public (unwrap-enum-yaml_parser_state_e n)
  (cond
   ((symbol? n)
    (or (assq-ref enum-yaml_parser_state_e-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-enum-yaml_parser_state_e v)
  (assq-ref enum-yaml_parser_state_e-enum-vnl v))

;; typedef struct yaml_alias_data_s {
;;   /** The anchor. */
;;   yaml_char_t *anchor;
;;   /** The node id. */
;;   int index;
;;   /** The anchor mark. */
;;   yaml_mark_t mark;
;; } yaml_alias_data_t;
(define-public yaml_alias_data_t-desc
  (bs:struct
    (list `(anchor ,(fh:pointer uint8))
          `(index ,int)
          `(mark ,yaml_mark_t-desc))))
(define-fh-compound-type yaml_alias_data_t yaml_alias_data_t-desc 
 yaml_alias_data_t? make-yaml_alias_data_t)
(export yaml_alias_data_t yaml_alias_data_t? make-yaml_alias_data_t)
(define-public yaml_alias_data_t*-desc
  (fh:pointer yaml_alias_data_t-desc))
(define-fh-pointer-type yaml_alias_data_t* yaml_alias_data_t*-desc 
 yaml_alias_data_t*? make-yaml_alias_data_t*)
(export yaml_alias_data_t* yaml_alias_data_t*? make-yaml_alias_data_t*)
(ref<->deref!
  yaml_alias_data_t*
  make-yaml_alias_data_t*
  yaml_alias_data_t
  make-yaml_alias_data_t)
(define-public struct-yaml_alias_data_s-desc
  yaml_alias_data_t-desc)
(define-fh-compound-type struct-yaml_alias_data_s 
 struct-yaml_alias_data_s-desc struct-yaml_alias_data_s? 
 make-struct-yaml_alias_data_s)
(export struct-yaml_alias_data_s struct-yaml_alias_data_s? 
 make-struct-yaml_alias_data_s)
(define-public struct-yaml_alias_data_s*-desc
  yaml_alias_data_t*-desc)
(define-fh-pointer-type struct-yaml_alias_data_s* 
 struct-yaml_alias_data_s*-desc struct-yaml_alias_data_s*? 
 make-struct-yaml_alias_data_s*)
(export struct-yaml_alias_data_s* struct-yaml_alias_data_s*? 
 make-struct-yaml_alias_data_s*)
(ref<->deref!
  struct-yaml_alias_data_s*
  make-struct-yaml_alias_data_s*
  struct-yaml_alias_data_s
  make-struct-yaml_alias_data_s)

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
;;   /* Does the tokens queue contain a token ready for dequeueing. */
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
(define-public yaml_parser_t-desc
  (bs:struct
    (list `(error ,int)
          `(problem ,(fh:pointer int8))
          `(problem_offset ,size_t)
          `(problem_value ,int)
          `(problem_mark ,yaml_mark_t-desc)
          `(context ,(fh:pointer int8))
          `(context_mark ,yaml_mark_t-desc)
          `(read_handler ,(fh:pointer 'void))
          `(read_handler_data ,(fh:pointer 'void))
          `(input ,(bs:union
                     (list `(string
                              ,(bs:struct
                                 (list `(start ,(fh:pointer uint8))
                                       `(end ,(fh:pointer uint8))
                                       `(current ,(fh:pointer uint8)))))
                           `(file ,(fh:pointer 'void)))))
          `(eof ,int)
          `(buffer
             ,(bs:struct
                (list `(start ,(fh:pointer uint8))
                      `(end ,(fh:pointer uint8))
                      `(pointer ,(fh:pointer uint8))
                      `(last ,(fh:pointer uint8)))))
          `(unread ,size_t)
          `(raw_buffer
             ,(bs:struct
                (list `(start ,(fh:pointer uint8))
                      `(end ,(fh:pointer uint8))
                      `(pointer ,(fh:pointer uint8))
                      `(last ,(fh:pointer uint8)))))
          `(encoding ,int)
          `(offset ,size_t)
          `(mark ,yaml_mark_t-desc)
          `(stream_start_produced ,int)
          `(stream_end_produced ,int)
          `(flow_level ,int)
          `(tokens
             ,(bs:struct
                (list `(start ,yaml_token_t*-desc)
                      `(end ,yaml_token_t*-desc)
                      `(head ,yaml_token_t*-desc)
                      `(tail ,yaml_token_t*-desc))))
          `(tokens_parsed ,size_t)
          `(token_available ,int)
          `(indents
             ,(bs:struct
                (list `(start ,(fh:pointer int))
                      `(end ,(fh:pointer int))
                      `(top ,(fh:pointer int)))))
          `(indent ,int)
          `(simple_key_allowed ,int)
          `(simple_keys
             ,(bs:struct
                (list `(start ,yaml_simple_key_t*-desc)
                      `(end ,yaml_simple_key_t*-desc)
                      `(top ,yaml_simple_key_t*-desc))))
          `(states
             ,(bs:struct
                (list `(start ,(fh:pointer int))
                      `(end ,(fh:pointer int))
                      `(top ,(fh:pointer int)))))
          `(state ,int)
          `(marks ,(bs:struct
                     (list `(start ,yaml_mark_t*-desc)
                           `(end ,yaml_mark_t*-desc)
                           `(top ,yaml_mark_t*-desc))))
          `(tag_directives
             ,(bs:struct
                (list `(start ,yaml_tag_directive_t*-desc)
                      `(end ,yaml_tag_directive_t*-desc)
                      `(top ,yaml_tag_directive_t*-desc))))
          `(aliases
             ,(bs:struct
                (list `(start ,yaml_alias_data_t*-desc)
                      `(end ,yaml_alias_data_t*-desc)
                      `(top ,yaml_alias_data_t*-desc))))
          `(document ,yaml_document_t*-desc))))
(define-fh-compound-type yaml_parser_t yaml_parser_t-desc yaml_parser_t? 
 make-yaml_parser_t)
(export yaml_parser_t yaml_parser_t? make-yaml_parser_t)
(define-public yaml_parser_t*-desc
  (fh:pointer yaml_parser_t-desc))
(define-fh-pointer-type yaml_parser_t* yaml_parser_t*-desc yaml_parser_t*? 
 make-yaml_parser_t*)
(export yaml_parser_t* yaml_parser_t*? make-yaml_parser_t*)
(ref<->deref!
  yaml_parser_t*
  make-yaml_parser_t*
  yaml_parser_t
  make-yaml_parser_t)
(define-public struct-yaml_parser_s-desc
  yaml_parser_t-desc)
(define-fh-compound-type struct-yaml_parser_s struct-yaml_parser_s-desc 
 struct-yaml_parser_s? make-struct-yaml_parser_s)
(export struct-yaml_parser_s struct-yaml_parser_s? make-struct-yaml_parser_s)
(define-public struct-yaml_parser_s*-desc
  yaml_parser_t*-desc)
(define-fh-pointer-type struct-yaml_parser_s* struct-yaml_parser_s*-desc 
 struct-yaml_parser_s*? make-struct-yaml_parser_s*)
(export struct-yaml_parser_s* struct-yaml_parser_s*? 
 make-struct-yaml_parser_s*)
(ref<->deref!
  struct-yaml_parser_s*
  make-struct-yaml_parser_s*
  struct-yaml_parser_s
  make-struct-yaml_parser_s)

;; int yaml_parser_initialize(yaml_parser_t *parser);
(define yaml_parser_initialize
  (let ((~yaml_parser_initialize
          (delay (fh-link-proc
                   ffi:int
                   "yaml_parser_initialize"
                   (list ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (parser)
      (let ((~parser ((fht-unwrap yaml_parser_t*) parser)))
        ((force ~yaml_parser_initialize) ~parser)))))
(export yaml_parser_initialize)

;; void yaml_parser_delete(yaml_parser_t *parser);
(define yaml_parser_delete
  (let ((~yaml_parser_delete
          (delay (fh-link-proc
                   ffi:void
                   "yaml_parser_delete"
                   (list ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (parser)
      (let ((~parser ((fht-unwrap yaml_parser_t*) parser)))
        ((force ~yaml_parser_delete) ~parser)))))
(export yaml_parser_delete)

;; void yaml_parser_set_input_string(yaml_parser_t *parser, const unsigned char
;;      *input, size_t size);
(define yaml_parser_set_input_string
  (let ((~yaml_parser_set_input_string
          (delay (fh-link-proc
                   ffi:void
                   "yaml_parser_set_input_string"
                   (list ffi-void* ffi-void* ffi:size_t)
                   yaml-libyaml-llibs))))
    (lambda (parser input size)
      (let ((~parser ((fht-unwrap yaml_parser_t*) parser))
            (~input (unwrap~pointer input))
            (~size (unwrap~fixed size)))
        ((force ~yaml_parser_set_input_string)
         ~parser
         ~input
         ~size)))))
(export yaml_parser_set_input_string)

;; void yaml_parser_set_input_file(yaml_parser_t *parser, FILE *file);
(define yaml_parser_set_input_file
  (let ((~yaml_parser_set_input_file
          (delay (fh-link-proc
                   ffi:void
                   "yaml_parser_set_input_file"
                   (list ffi-void* ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (parser file)
      (let ((~parser ((fht-unwrap yaml_parser_t*) parser))
            (~file (unwrap~pointer file)))
        ((force ~yaml_parser_set_input_file)
         ~parser
         ~file)))))
(export yaml_parser_set_input_file)

;; void yaml_parser_set_input(yaml_parser_t *parser, yaml_read_handler_t *
;;     handler, void *data);
(define yaml_parser_set_input
  (let ((~yaml_parser_set_input
          (delay (fh-link-proc
                   ffi:void
                   "yaml_parser_set_input"
                   (list ffi-void* ffi-void* ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (parser handler data)
      (let ((~parser ((fht-unwrap yaml_parser_t*) parser))
            (~handler
              ((make-fctn-param-unwrapper
                 ffi:int
                 (list ffi-void* ffi-void* ffi:long ffi-void*))
               handler))
            (~data (unwrap~pointer data)))
        ((force ~yaml_parser_set_input)
         ~parser
         ~handler
         ~data)))))
(export yaml_parser_set_input)

;; void yaml_parser_set_encoding(yaml_parser_t *parser, yaml_encoding_t 
;;     encoding);
(define yaml_parser_set_encoding
  (let ((~yaml_parser_set_encoding
          (delay (fh-link-proc
                   ffi:void
                   "yaml_parser_set_encoding"
                   (list ffi-void* ffi:int)
                   yaml-libyaml-llibs))))
    (lambda (parser encoding)
      (let ((~parser ((fht-unwrap yaml_parser_t*) parser))
            (~encoding (unwrap-yaml_encoding_t encoding)))
        ((force ~yaml_parser_set_encoding)
         ~parser
         ~encoding)))))
(export yaml_parser_set_encoding)

;; int yaml_parser_scan(yaml_parser_t *parser, yaml_token_t *token);
(define yaml_parser_scan
  (let ((~yaml_parser_scan
          (delay (fh-link-proc
                   ffi:int
                   "yaml_parser_scan"
                   (list ffi-void* ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (parser token)
      (let ((~parser ((fht-unwrap yaml_parser_t*) parser))
            (~token ((fht-unwrap yaml_token_t*) token)))
        ((force ~yaml_parser_scan) ~parser ~token)))))
(export yaml_parser_scan)

;; int yaml_parser_parse(yaml_parser_t *parser, yaml_event_t *event);
(define yaml_parser_parse
  (let ((~yaml_parser_parse
          (delay (fh-link-proc
                   ffi:int
                   "yaml_parser_parse"
                   (list ffi-void* ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (parser event)
      (let ((~parser ((fht-unwrap yaml_parser_t*) parser))
            (~event ((fht-unwrap yaml_event_t*) event)))
        ((force ~yaml_parser_parse) ~parser ~event)))))
(export yaml_parser_parse)

;; int yaml_parser_load(yaml_parser_t *parser, yaml_document_t *document);
(define yaml_parser_load
  (let ((~yaml_parser_load
          (delay (fh-link-proc
                   ffi:int
                   "yaml_parser_load"
                   (list ffi-void* ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (parser document)
      (let ((~parser ((fht-unwrap yaml_parser_t*) parser))
            (~document
              ((fht-unwrap yaml_document_t*) document)))
        ((force ~yaml_parser_load) ~parser ~document)))))
(export yaml_parser_load)

;; typedef int yaml_write_handler_t(void *data, unsigned char *buffer, size_t 
;;     size);
(define yaml_write_handler_t
  (let ((~yaml_write_handler_t
          (delay (fh-link-proc
                   ffi:int
                   "yaml_write_handler_t"
                   (list ffi-void* ffi-void* ffi:size_t)
                   yaml-libyaml-llibs))))
    (lambda (data buffer size)
      (let ((~data (unwrap~pointer data))
            (~buffer (unwrap~pointer buffer))
            (~size (unwrap~fixed size)))
        ((force ~yaml_write_handler_t)
         ~data
         ~buffer
         ~size)))))
(export yaml_write_handler_t)

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
(define yaml_emitter_state_t-enum-nvl
  '((YAML_EMIT_STREAM_START_STATE . 0)
    (YAML_EMIT_FIRST_DOCUMENT_START_STATE . 1)
    (YAML_EMIT_DOCUMENT_START_STATE . 2)
    (YAML_EMIT_DOCUMENT_CONTENT_STATE . 3)
    (YAML_EMIT_DOCUMENT_END_STATE . 4)
    (YAML_EMIT_FLOW_SEQUENCE_FIRST_ITEM_STATE . 5)
    (YAML_EMIT_FLOW_SEQUENCE_ITEM_STATE . 6)
    (YAML_EMIT_FLOW_MAPPING_FIRST_KEY_STATE . 7)
    (YAML_EMIT_FLOW_MAPPING_KEY_STATE . 8)
    (YAML_EMIT_FLOW_MAPPING_SIMPLE_VALUE_STATE . 9)
    (YAML_EMIT_FLOW_MAPPING_VALUE_STATE . 10)
    (YAML_EMIT_BLOCK_SEQUENCE_FIRST_ITEM_STATE . 11)
    (YAML_EMIT_BLOCK_SEQUENCE_ITEM_STATE . 12)
    (YAML_EMIT_BLOCK_MAPPING_FIRST_KEY_STATE . 13)
    (YAML_EMIT_BLOCK_MAPPING_KEY_STATE . 14)
    (YAML_EMIT_BLOCK_MAPPING_SIMPLE_VALUE_STATE . 15)
    (YAML_EMIT_BLOCK_MAPPING_VALUE_STATE . 16)
    (YAML_EMIT_END_STATE . 17))
  )
(define yaml_emitter_state_t-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       yaml_emitter_state_t-enum-nvl))
(define-public (unwrap-yaml_emitter_state_t n)
  (cond
   ((symbol? n)
    (or (assq-ref yaml_emitter_state_t-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-yaml_emitter_state_t v)
  (assq-ref yaml_emitter_state_t-enum-vnl v))
(define-public unwrap-enum-yaml_emitter_state_e unwrap-yaml_emitter_state_t)
(define-public wrap-enum-yaml_emitter_state_e wrap-yaml_emitter_state_t)

;; enum yaml_emitter_state_e {
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
;; };
(define enum-yaml_emitter_state_e-enum-nvl
  '((YAML_EMIT_STREAM_START_STATE . 0)
    (YAML_EMIT_FIRST_DOCUMENT_START_STATE . 1)
    (YAML_EMIT_DOCUMENT_START_STATE . 2)
    (YAML_EMIT_DOCUMENT_CONTENT_STATE . 3)
    (YAML_EMIT_DOCUMENT_END_STATE . 4)
    (YAML_EMIT_FLOW_SEQUENCE_FIRST_ITEM_STATE . 5)
    (YAML_EMIT_FLOW_SEQUENCE_ITEM_STATE . 6)
    (YAML_EMIT_FLOW_MAPPING_FIRST_KEY_STATE . 7)
    (YAML_EMIT_FLOW_MAPPING_KEY_STATE . 8)
    (YAML_EMIT_FLOW_MAPPING_SIMPLE_VALUE_STATE . 9)
    (YAML_EMIT_FLOW_MAPPING_VALUE_STATE . 10)
    (YAML_EMIT_BLOCK_SEQUENCE_FIRST_ITEM_STATE . 11)
    (YAML_EMIT_BLOCK_SEQUENCE_ITEM_STATE . 12)
    (YAML_EMIT_BLOCK_MAPPING_FIRST_KEY_STATE . 13)
    (YAML_EMIT_BLOCK_MAPPING_KEY_STATE . 14)
    (YAML_EMIT_BLOCK_MAPPING_SIMPLE_VALUE_STATE . 15)
    (YAML_EMIT_BLOCK_MAPPING_VALUE_STATE . 16)
    (YAML_EMIT_END_STATE . 17))
  )
(define enum-yaml_emitter_state_e-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       enum-yaml_emitter_state_e-enum-nvl))
(define-public (unwrap-enum-yaml_emitter_state_e n)
  (cond
   ((symbol? n)
    (or (assq-ref enum-yaml_emitter_state_e-enum-nvl n) (error "bad arg")))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-enum-yaml_emitter_state_e v)
  (assq-ref enum-yaml_emitter_state_e-enum-vnl v))

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
;;   /** A pointer for passing to the white handler. */
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
;;   struct {
;;     /** The number of references. */
;;     int references;
;;     /** The anchor id. */
;;     int anchor;
;;     /** If the node has been emitted? */
;;     int serialized;
;;   } *anchors;
;;   /** The last assigned anchor id. */
;;   int last_anchor_id;
;;   /** The currently emitted document. */
;;   yaml_document_t *document;
;;   /**
;;    * @}
;;    */
;; } yaml_emitter_t;
(define-public yaml_emitter_t-desc
  (bs:struct
    (list `(error ,int)
          `(problem ,(fh:pointer int8))
          `(write_handler ,(fh:pointer 'void))
          `(write_handler_data ,(fh:pointer 'void))
          `(output
             ,(bs:union
                (list `(string
                         ,(bs:struct
                            (list `(buffer ,(fh:pointer uint8))
                                  `(size ,size_t)
                                  `(size_written ,(fh:pointer size_t)))))
                      `(file ,(fh:pointer 'void)))))
          `(buffer
             ,(bs:struct
                (list `(start ,(fh:pointer uint8))
                      `(end ,(fh:pointer uint8))
                      `(pointer ,(fh:pointer uint8))
                      `(last ,(fh:pointer uint8)))))
          `(raw_buffer
             ,(bs:struct
                (list `(start ,(fh:pointer uint8))
                      `(end ,(fh:pointer uint8))
                      `(pointer ,(fh:pointer uint8))
                      `(last ,(fh:pointer uint8)))))
          `(encoding ,int)
          `(canonical ,int)
          `(best_indent ,int)
          `(best_width ,int)
          `(unicode ,int)
          `(line_break ,int)
          `(states
             ,(bs:struct
                (list `(start ,(fh:pointer int))
                      `(end ,(fh:pointer int))
                      `(top ,(fh:pointer int)))))
          `(state ,int)
          `(events
             ,(bs:struct
                (list `(start ,yaml_event_t*-desc)
                      `(end ,yaml_event_t*-desc)
                      `(head ,yaml_event_t*-desc)
                      `(tail ,yaml_event_t*-desc))))
          `(indents
             ,(bs:struct
                (list `(start ,(fh:pointer int))
                      `(end ,(fh:pointer int))
                      `(top ,(fh:pointer int)))))
          `(tag_directives
             ,(bs:struct
                (list `(start ,yaml_tag_directive_t*-desc)
                      `(end ,yaml_tag_directive_t*-desc)
                      `(top ,yaml_tag_directive_t*-desc))))
          `(indent ,int)
          `(flow_level ,int)
          `(root_context ,int)
          `(sequence_context ,int)
          `(mapping_context ,int)
          `(simple_key_context ,int)
          `(line ,int)
          `(column ,int)
          `(whitespace ,int)
          `(indention ,int)
          `(open_ended ,int)
          `(anchor_data
             ,(bs:struct
                (list `(anchor ,(fh:pointer uint8))
                      `(anchor_length ,size_t)
                      `(alias ,int))))
          `(tag_data
             ,(bs:struct
                (list `(handle ,(fh:pointer uint8))
                      `(handle_length ,size_t)
                      `(suffix ,(fh:pointer uint8))
                      `(suffix_length ,size_t))))
          `(scalar_data
             ,(bs:struct
                (list `(value ,(fh:pointer uint8))
                      `(length ,size_t)
                      `(multiline ,int)
                      `(flow_plain_allowed ,int)
                      `(block_plain_allowed ,int)
                      `(single_quoted_allowed ,int)
                      `(block_allowed ,int)
                      `(style ,int))))
          `(opened ,int)
          `(closed ,int)
          `(anchors
             ,(fh:pointer
                (bs:struct
                  (list `(references ,int)
                        `(anchor ,int)
                        `(serialized ,int)))))
          `(last_anchor_id ,int)
          `(document ,yaml_document_t*-desc))))
(define-fh-compound-type yaml_emitter_t yaml_emitter_t-desc yaml_emitter_t? 
 make-yaml_emitter_t)
(export yaml_emitter_t yaml_emitter_t? make-yaml_emitter_t)
(define-public yaml_emitter_t*-desc
  (fh:pointer yaml_emitter_t-desc))
(define-fh-pointer-type yaml_emitter_t* yaml_emitter_t*-desc yaml_emitter_t*? 
 make-yaml_emitter_t*)
(export yaml_emitter_t* yaml_emitter_t*? make-yaml_emitter_t*)
(ref<->deref!
  yaml_emitter_t*
  make-yaml_emitter_t*
  yaml_emitter_t
  make-yaml_emitter_t)
(define-public struct-yaml_emitter_s-desc
  yaml_emitter_t-desc)
(define-fh-compound-type struct-yaml_emitter_s struct-yaml_emitter_s-desc 
 struct-yaml_emitter_s? make-struct-yaml_emitter_s)
(export struct-yaml_emitter_s struct-yaml_emitter_s? 
 make-struct-yaml_emitter_s)
(define-public struct-yaml_emitter_s*-desc
  yaml_emitter_t*-desc)
(define-fh-pointer-type struct-yaml_emitter_s* struct-yaml_emitter_s*-desc 
 struct-yaml_emitter_s*? make-struct-yaml_emitter_s*)
(export struct-yaml_emitter_s* struct-yaml_emitter_s*? 
 make-struct-yaml_emitter_s*)
(ref<->deref!
  struct-yaml_emitter_s*
  make-struct-yaml_emitter_s*
  struct-yaml_emitter_s
  make-struct-yaml_emitter_s)

;; int yaml_emitter_initialize(yaml_emitter_t *emitter);
(define yaml_emitter_initialize
  (let ((~yaml_emitter_initialize
          (delay (fh-link-proc
                   ffi:int
                   "yaml_emitter_initialize"
                   (list ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (emitter)
      (let ((~emitter ((fht-unwrap yaml_emitter_t*) emitter)))
        ((force ~yaml_emitter_initialize) ~emitter)))))
(export yaml_emitter_initialize)

;; void yaml_emitter_delete(yaml_emitter_t *emitter);
(define yaml_emitter_delete
  (let ((~yaml_emitter_delete
          (delay (fh-link-proc
                   ffi:void
                   "yaml_emitter_delete"
                   (list ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (emitter)
      (let ((~emitter ((fht-unwrap yaml_emitter_t*) emitter)))
        ((force ~yaml_emitter_delete) ~emitter)))))
(export yaml_emitter_delete)

;; void yaml_emitter_set_output_string(yaml_emitter_t *emitter, unsigned char *
;;     output, size_t size, size_t *size_written);
(define yaml_emitter_set_output_string
  (let ((~yaml_emitter_set_output_string
          (delay (fh-link-proc
                   ffi:void
                   "yaml_emitter_set_output_string"
                   (list ffi-void* ffi-void* ffi:size_t ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (emitter output size size_written)
      (let ((~emitter ((fht-unwrap yaml_emitter_t*) emitter))
            (~output (unwrap~pointer output))
            (~size (unwrap~fixed size))
            (~size_written (unwrap~pointer size_written)))
        ((force ~yaml_emitter_set_output_string)
         ~emitter
         ~output
         ~size
         ~size_written)))))
(export yaml_emitter_set_output_string)

;; void yaml_emitter_set_output_file(yaml_emitter_t *emitter, FILE *file);
(define yaml_emitter_set_output_file
  (let ((~yaml_emitter_set_output_file
          (delay (fh-link-proc
                   ffi:void
                   "yaml_emitter_set_output_file"
                   (list ffi-void* ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (emitter file)
      (let ((~emitter ((fht-unwrap yaml_emitter_t*) emitter))
            (~file (unwrap~pointer file)))
        ((force ~yaml_emitter_set_output_file)
         ~emitter
         ~file)))))
(export yaml_emitter_set_output_file)

;; void yaml_emitter_set_output(yaml_emitter_t *emitter, yaml_write_handler_t *
;;     handler, void *data);
(define yaml_emitter_set_output
  (let ((~yaml_emitter_set_output
          (delay (fh-link-proc
                   ffi:void
                   "yaml_emitter_set_output"
                   (list ffi-void* ffi-void* ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (emitter handler data)
      (let ((~emitter ((fht-unwrap yaml_emitter_t*) emitter))
            (~handler
              ((make-fctn-param-unwrapper
                 ffi:int
                 (list ffi-void* ffi-void* ffi:long))
               handler))
            (~data (unwrap~pointer data)))
        ((force ~yaml_emitter_set_output)
         ~emitter
         ~handler
         ~data)))))
(export yaml_emitter_set_output)

;; void yaml_emitter_set_encoding(yaml_emitter_t *emitter, yaml_encoding_t 
;;     encoding);
(define yaml_emitter_set_encoding
  (let ((~yaml_emitter_set_encoding
          (delay (fh-link-proc
                   ffi:void
                   "yaml_emitter_set_encoding"
                   (list ffi-void* ffi:int)
                   yaml-libyaml-llibs))))
    (lambda (emitter encoding)
      (let ((~emitter ((fht-unwrap yaml_emitter_t*) emitter))
            (~encoding (unwrap-yaml_encoding_t encoding)))
        ((force ~yaml_emitter_set_encoding)
         ~emitter
         ~encoding)))))
(export yaml_emitter_set_encoding)

;; void yaml_emitter_set_canonical(yaml_emitter_t *emitter, int canonical);
(define yaml_emitter_set_canonical
  (let ((~yaml_emitter_set_canonical
          (delay (fh-link-proc
                   ffi:void
                   "yaml_emitter_set_canonical"
                   (list ffi-void* ffi:int)
                   yaml-libyaml-llibs))))
    (lambda (emitter canonical)
      (let ((~emitter ((fht-unwrap yaml_emitter_t*) emitter))
            (~canonical (unwrap~fixed canonical)))
        ((force ~yaml_emitter_set_canonical)
         ~emitter
         ~canonical)))))
(export yaml_emitter_set_canonical)

;; void yaml_emitter_set_indent(yaml_emitter_t *emitter, int indent);
(define yaml_emitter_set_indent
  (let ((~yaml_emitter_set_indent
          (delay (fh-link-proc
                   ffi:void
                   "yaml_emitter_set_indent"
                   (list ffi-void* ffi:int)
                   yaml-libyaml-llibs))))
    (lambda (emitter indent)
      (let ((~emitter ((fht-unwrap yaml_emitter_t*) emitter))
            (~indent (unwrap~fixed indent)))
        ((force ~yaml_emitter_set_indent)
         ~emitter
         ~indent)))))
(export yaml_emitter_set_indent)

;; void yaml_emitter_set_width(yaml_emitter_t *emitter, int width);
(define yaml_emitter_set_width
  (let ((~yaml_emitter_set_width
          (delay (fh-link-proc
                   ffi:void
                   "yaml_emitter_set_width"
                   (list ffi-void* ffi:int)
                   yaml-libyaml-llibs))))
    (lambda (emitter width)
      (let ((~emitter ((fht-unwrap yaml_emitter_t*) emitter))
            (~width (unwrap~fixed width)))
        ((force ~yaml_emitter_set_width) ~emitter ~width)))))
(export yaml_emitter_set_width)

;; void yaml_emitter_set_unicode(yaml_emitter_t *emitter, int unicode);
(define yaml_emitter_set_unicode
  (let ((~yaml_emitter_set_unicode
          (delay (fh-link-proc
                   ffi:void
                   "yaml_emitter_set_unicode"
                   (list ffi-void* ffi:int)
                   yaml-libyaml-llibs))))
    (lambda (emitter unicode)
      (let ((~emitter ((fht-unwrap yaml_emitter_t*) emitter))
            (~unicode (unwrap~fixed unicode)))
        ((force ~yaml_emitter_set_unicode)
         ~emitter
         ~unicode)))))
(export yaml_emitter_set_unicode)

;; void yaml_emitter_set_break(yaml_emitter_t *emitter, yaml_break_t line_break
;;     );
(define yaml_emitter_set_break
  (let ((~yaml_emitter_set_break
          (delay (fh-link-proc
                   ffi:void
                   "yaml_emitter_set_break"
                   (list ffi-void* ffi:int)
                   yaml-libyaml-llibs))))
    (lambda (emitter line_break)
      (let ((~emitter ((fht-unwrap yaml_emitter_t*) emitter))
            (~line_break (unwrap-yaml_break_t line_break)))
        ((force ~yaml_emitter_set_break)
         ~emitter
         ~line_break)))))
(export yaml_emitter_set_break)

;; int yaml_emitter_emit(yaml_emitter_t *emitter, yaml_event_t *event);
(define yaml_emitter_emit
  (let ((~yaml_emitter_emit
          (delay (fh-link-proc
                   ffi:int
                   "yaml_emitter_emit"
                   (list ffi-void* ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (emitter event)
      (let ((~emitter ((fht-unwrap yaml_emitter_t*) emitter))
            (~event ((fht-unwrap yaml_event_t*) event)))
        ((force ~yaml_emitter_emit) ~emitter ~event)))))
(export yaml_emitter_emit)

;; int yaml_emitter_open(yaml_emitter_t *emitter);
(define yaml_emitter_open
  (let ((~yaml_emitter_open
          (delay (fh-link-proc
                   ffi:int
                   "yaml_emitter_open"
                   (list ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (emitter)
      (let ((~emitter ((fht-unwrap yaml_emitter_t*) emitter)))
        ((force ~yaml_emitter_open) ~emitter)))))
(export yaml_emitter_open)

;; int yaml_emitter_close(yaml_emitter_t *emitter);
(define yaml_emitter_close
  (let ((~yaml_emitter_close
          (delay (fh-link-proc
                   ffi:int
                   "yaml_emitter_close"
                   (list ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (emitter)
      (let ((~emitter ((fht-unwrap yaml_emitter_t*) emitter)))
        ((force ~yaml_emitter_close) ~emitter)))))
(export yaml_emitter_close)

;; int yaml_emitter_dump(yaml_emitter_t *emitter, yaml_document_t *document);
(define yaml_emitter_dump
  (let ((~yaml_emitter_dump
          (delay (fh-link-proc
                   ffi:int
                   "yaml_emitter_dump"
                   (list ffi-void* ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (emitter document)
      (let ((~emitter ((fht-unwrap yaml_emitter_t*) emitter))
            (~document
              ((fht-unwrap yaml_document_t*) document)))
        ((force ~yaml_emitter_dump) ~emitter ~document)))))
(export yaml_emitter_dump)

;; int yaml_emitter_flush(yaml_emitter_t *emitter);
(define yaml_emitter_flush
  (let ((~yaml_emitter_flush
          (delay (fh-link-proc
                   ffi:int
                   "yaml_emitter_flush"
                   (list ffi-void*)
                   yaml-libyaml-llibs))))
    (lambda (emitter)
      (let ((~emitter ((fht-unwrap yaml_emitter_t*) emitter)))
        ((force ~yaml_emitter_flush) ~emitter)))))
(export yaml_emitter_flush)

;; access to enum symbols and #define'd constants:
(define yaml-libyaml-symbol-tab
  '((YAML_ANY_ENCODING . 0)
    (YAML_UTF8_ENCODING . 1)
    (YAML_UTF16LE_ENCODING . 2)
    (YAML_UTF16BE_ENCODING . 3)
    (YAML_ANY_BREAK . 0)
    (YAML_CR_BREAK . 1)
    (YAML_LN_BREAK . 2)
    (YAML_CRLN_BREAK . 3)
    (YAML_NO_ERROR . 0)
    (YAML_MEMORY_ERROR . 1)
    (YAML_READER_ERROR . 2)
    (YAML_SCANNER_ERROR . 3)
    (YAML_PARSER_ERROR . 4)
    (YAML_COMPOSER_ERROR . 5)
    (YAML_WRITER_ERROR . 6)
    (YAML_EMITTER_ERROR . 7)
    (YAML_ANY_SCALAR_STYLE . 0)
    (YAML_PLAIN_SCALAR_STYLE . 1)
    (YAML_SINGLE_QUOTED_SCALAR_STYLE . 2)
    (YAML_DOUBLE_QUOTED_SCALAR_STYLE . 3)
    (YAML_LITERAL_SCALAR_STYLE . 4)
    (YAML_FOLDED_SCALAR_STYLE . 5)
    (YAML_ANY_SEQUENCE_STYLE . 0)
    (YAML_BLOCK_SEQUENCE_STYLE . 1)
    (YAML_FLOW_SEQUENCE_STYLE . 2)
    (YAML_ANY_MAPPING_STYLE . 0)
    (YAML_BLOCK_MAPPING_STYLE . 1)
    (YAML_FLOW_MAPPING_STYLE . 2)
    (YAML_NO_TOKEN . 0)
    (YAML_STREAM_START_TOKEN . 1)
    (YAML_STREAM_END_TOKEN . 2)
    (YAML_VERSION_DIRECTIVE_TOKEN . 3)
    (YAML_TAG_DIRECTIVE_TOKEN . 4)
    (YAML_DOCUMENT_START_TOKEN . 5)
    (YAML_DOCUMENT_END_TOKEN . 6)
    (YAML_BLOCK_SEQUENCE_START_TOKEN . 7)
    (YAML_BLOCK_MAPPING_START_TOKEN . 8)
    (YAML_BLOCK_END_TOKEN . 9)
    (YAML_FLOW_SEQUENCE_START_TOKEN . 10)
    (YAML_FLOW_SEQUENCE_END_TOKEN . 11)
    (YAML_FLOW_MAPPING_START_TOKEN . 12)
    (YAML_FLOW_MAPPING_END_TOKEN . 13)
    (YAML_BLOCK_ENTRY_TOKEN . 14)
    (YAML_FLOW_ENTRY_TOKEN . 15)
    (YAML_KEY_TOKEN . 16)
    (YAML_VALUE_TOKEN . 17)
    (YAML_ALIAS_TOKEN . 18)
    (YAML_ANCHOR_TOKEN . 19)
    (YAML_TAG_TOKEN . 20)
    (YAML_SCALAR_TOKEN . 21)
    (YAML_NO_EVENT . 0)
    (YAML_STREAM_START_EVENT . 1)
    (YAML_STREAM_END_EVENT . 2)
    (YAML_DOCUMENT_START_EVENT . 3)
    (YAML_DOCUMENT_END_EVENT . 4)
    (YAML_ALIAS_EVENT . 5)
    (YAML_SCALAR_EVENT . 6)
    (YAML_SEQUENCE_START_EVENT . 7)
    (YAML_SEQUENCE_END_EVENT . 8)
    (YAML_MAPPING_START_EVENT . 9)
    (YAML_MAPPING_END_EVENT . 10)
    (YAML_NO_NODE . 0)
    (YAML_SCALAR_NODE . 1)
    (YAML_SEQUENCE_NODE . 2)
    (YAML_MAPPING_NODE . 3)
    (YAML_PARSE_STREAM_START_STATE . 0)
    (YAML_PARSE_IMPLICIT_DOCUMENT_START_STATE . 1)
    (YAML_PARSE_DOCUMENT_START_STATE . 2)
    (YAML_PARSE_DOCUMENT_CONTENT_STATE . 3)
    (YAML_PARSE_DOCUMENT_END_STATE . 4)
    (YAML_PARSE_BLOCK_NODE_STATE . 5)
    (YAML_PARSE_BLOCK_NODE_OR_INDENTLESS_SEQUENCE_STATE
      .
      6)
    (YAML_PARSE_FLOW_NODE_STATE . 7)
    (YAML_PARSE_BLOCK_SEQUENCE_FIRST_ENTRY_STATE . 8)
    (YAML_PARSE_BLOCK_SEQUENCE_ENTRY_STATE . 9)
    (YAML_PARSE_INDENTLESS_SEQUENCE_ENTRY_STATE . 10)
    (YAML_PARSE_BLOCK_MAPPING_FIRST_KEY_STATE . 11)
    (YAML_PARSE_BLOCK_MAPPING_KEY_STATE . 12)
    (YAML_PARSE_BLOCK_MAPPING_VALUE_STATE . 13)
    (YAML_PARSE_FLOW_SEQUENCE_FIRST_ENTRY_STATE . 14)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_STATE . 15)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_KEY_STATE
      .
      16)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_VALUE_STATE
      .
      17)
    (YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_END_STATE
      .
      18)
    (YAML_PARSE_FLOW_MAPPING_FIRST_KEY_STATE . 19)
    (YAML_PARSE_FLOW_MAPPING_KEY_STATE . 20)
    (YAML_PARSE_FLOW_MAPPING_VALUE_STATE . 21)
    (YAML_PARSE_FLOW_MAPPING_EMPTY_VALUE_STATE . 22)
    (YAML_PARSE_END_STATE . 23)
    (YAML_EMIT_STREAM_START_STATE . 0)
    (YAML_EMIT_FIRST_DOCUMENT_START_STATE . 1)
    (YAML_EMIT_DOCUMENT_START_STATE . 2)
    (YAML_EMIT_DOCUMENT_CONTENT_STATE . 3)
    (YAML_EMIT_DOCUMENT_END_STATE . 4)
    (YAML_EMIT_FLOW_SEQUENCE_FIRST_ITEM_STATE . 5)
    (YAML_EMIT_FLOW_SEQUENCE_ITEM_STATE . 6)
    (YAML_EMIT_FLOW_MAPPING_FIRST_KEY_STATE . 7)
    (YAML_EMIT_FLOW_MAPPING_KEY_STATE . 8)
    (YAML_EMIT_FLOW_MAPPING_SIMPLE_VALUE_STATE . 9)
    (YAML_EMIT_FLOW_MAPPING_VALUE_STATE . 10)
    (YAML_EMIT_BLOCK_SEQUENCE_FIRST_ITEM_STATE . 11)
    (YAML_EMIT_BLOCK_SEQUENCE_ITEM_STATE . 12)
    (YAML_EMIT_BLOCK_MAPPING_FIRST_KEY_STATE . 13)
    (YAML_EMIT_BLOCK_MAPPING_KEY_STATE . 14)
    (YAML_EMIT_BLOCK_MAPPING_SIMPLE_VALUE_STATE . 15)
    (YAML_EMIT_BLOCK_MAPPING_VALUE_STATE . 16)
    (YAML_EMIT_END_STATE . 17)
    (YAML_NULL_TAG . "tag:yaml.org,2002:null")
    (YAML_BOOL_TAG . "tag:yaml.org,2002:bool")
    (YAML_STR_TAG . "tag:yaml.org,2002:str")
    (YAML_INT_TAG . "tag:yaml.org,2002:int")
    (YAML_FLOAT_TAG . "tag:yaml.org,2002:float")
    (YAML_TIMESTAMP_TAG
      .
      "tag:yaml.org,2002:timestamp")
    (YAML_SEQ_TAG . "tag:yaml.org,2002:seq")
    (YAML_MAP_TAG . "tag:yaml.org,2002:map")
    (YAML_DEFAULT_SCALAR_TAG
      .
      "tag:yaml.org,2002:str")
    (YAML_DEFAULT_SEQUENCE_TAG
      .
      "tag:yaml.org,2002:seq")
    (YAML_DEFAULT_MAPPING_TAG
      .
      "tag:yaml.org,2002:map")))
(define yaml-libyaml-symbol-val
  (lambda (k)
    (or (assq-ref yaml-libyaml-symbol-tab k))))
(export yaml-libyaml-symbol-val)

(define (unwrap-enum obj)
  (cond ((number? obj) obj)
        ((symbol? obj) (yaml-libyaml-symbol-val obj))
        ((fh-object? obj) (struct-ref obj 0))
        (else (error "type mismatch"))))

(define yaml-libyaml-types
  '((struct . "yaml_version_directive_s") (pointer . 
    "yaml_version_directive_t") "yaml_version_directive_t" (struct . 
    "yaml_tag_directive_s") (pointer . "yaml_tag_directive_t") 
    "yaml_tag_directive_t" (struct . "yaml_mark_s") (pointer . "yaml_mark_t") 
    "yaml_mark_t" (struct . "yaml_token_s") (pointer . "yaml_token_t") 
    "yaml_token_t" (struct . "yaml_event_s") (pointer . "yaml_event_t") 
    "yaml_event_t" (pointer . "yaml_node_t") "yaml_node_t" (struct . 
    "yaml_node_pair_s") (pointer . "yaml_node_pair_t") "yaml_node_pair_t" 
    (struct . "yaml_node_s") (struct . "yaml_document_s") (pointer . 
    "yaml_document_t") "yaml_document_t" (struct . "yaml_simple_key_s") 
    (pointer . "yaml_simple_key_t") "yaml_simple_key_t" (struct . 
    "yaml_alias_data_s") (pointer . "yaml_alias_data_t") "yaml_alias_data_t" 
    (struct . "yaml_parser_s") (pointer . "yaml_parser_t") "yaml_parser_t" 
    (struct . "yaml_emitter_s") (pointer . "yaml_emitter_t") "yaml_emitter_t"))
(export yaml-libyaml-types)

;; --- last line ---
