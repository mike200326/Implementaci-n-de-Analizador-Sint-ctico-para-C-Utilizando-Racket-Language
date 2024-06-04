#lang racket

(require racket/future)

; Helper function to read file content into lines
(define (file->lines file)
  (displayln (string-append "Reading file: " file))
  (call-with-input-file file
    (lambda (in)
      (let loop ([lines '()])
        (let ([line (read-line in 'any)])
          (if (eof-object? line)
              (begin
                (displayln (string-append "File read successfully: " file))
                (reverse lines))
              (loop (cons line lines))))))))

; Helper function to write lines to a file
(define (display-lines-to-file lines file #:exists exists)
  (displayln (string-append "Writing to file: " file))
  (call-with-output-file file #:exists exists
    (lambda (out)
      (for-each (lambda (line)
                  (display line out)
                  (newline out))
                lines))))

; Function to replace all instances in a string
(define (string-replace-all s replacements)
  (foldl (lambda (pair acc)
           (regexp-replace* (car pair) acc (cdr pair)))
         s
         replacements))

; Escape HTML special characters
(define (escape-html text)
  (string-replace-all text
                      (list (cons "&" "&amp;")
                            (cons "<" "&lt;")
                            (cons ">" "&gt;")
                            (cons "\"" "&quot;")
                            (cons "'" "&#x27;"))))

; Replace tokens in C++ code with HTML formatted tokens
(define (replace-match in-path)
  (displayln (string-append "Replacing tokens in file: " in-path))
  (define lines (file->lines in-path))
  (let loop
    ([lines lines] [res empty])
    (if (null? lines)
        res
        (let*
          ([dis-line (car lines)]
           ; Escape HTML characters first
           [dis-line (escape-html dis-line)]
           ; Replace keywords first to avoid nested tags
           [dis-line (regexp-replace* #px"\\b(int|float|double|char|void|bool|if|else|for|while|do|return|class|struct|public|private|protected|const|static|virtual|override|namespace|using|new|delete|sizeof|typedef|typename|template|try|catch|throw|this|true|false)\\b" dis-line "<span class='keywords'>\\0</span>")]
           ; Replace preprocessor directives
           [dis-line (regexp-replace* #px"#\\w+" dis-line "<span class='preprocessor'>\\0</span>")]
           ; Replace strings
           [dis-line (regexp-replace* #px"\"([^\"]*)\"" dis-line "<span class='strings'>\"\\1\"</span>")]
           ; Replace characters
           [dis-line (regexp-replace* #px"'([^']*)'" dis-line "<span class='chars'>'\\1'</span>")]
           ; Replace comments
           [dis-line (regexp-replace* #px"//.*" dis-line "<span class='comments'>\\0</span>")]
           [dis-line (regexp-replace* #px"/\\*.*?\\*/" dis-line "<span class='comments'>\\0</span>")]
           ; Replace numbers
           [dis-line (regexp-replace* #px"\\b\\d+\\.?\\d*\\b" dis-line "<span class='numbers'>\\0</span>")])
          (loop (cdr lines) (append res (list dis-line)))))))

; Create highlighted syntax HTML file for one C++ file
(define (write-file in-path)
  (define out-dir "results") ; Output directory
  ; Create results directory if it doesn't exist
  (unless (directory-exists? out-dir)
    (make-directory out-dir))
  (define out-path
    (string-append out-dir "/" (regexp-replace #px".*?([^/]+)\\.cpp" in-path "\\1.html")))
  (displayln (string-append "Creating output file: " out-path))
  ; Add HTML to the top
  (call-with-output-file out-path #:exists 'truncate
    (lambda (out)
      (display "<!DOCTYPE html>
        <html lang=\"en\">
        <head>
            <meta charset=\"UTF-8\">
            <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">
            <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
            <title>C++ Highlighter</title>
            <style>
            .keywords { color: blue; font-weight: bold; }
            .strings { color: green; }
            .chars { color: orange; }
            .comments { color: gray; font-style: italic; }
            .numbers { color: red; }
            .preprocessor { color: purple; font-weight: bold; }
            </style>
        </head>
        <body>
        <h1>C++ Highlighter</h1>
        <pre><code>"
        out)))

  ; Add highlighted syntax
  (display-lines-to-file (replace-match in-path) out-path #:exists 'append)

  ; Add HTML to bottom
  (call-with-output-file out-path #:exists 'append
    (lambda (out)
      (display "</code></pre>
        </body>
        </html>"
        out)))
  (displayln (string-append "Output file created successfully: " out-path)))

; Create highlighted syntax HTML file given a directory with C++ files
(define (write-files dir-path)
  (let ([out-dir "results"])
    ; Create results directory if it doesn't exist
    (unless (directory-exists? out-dir)
      (make-directory out-dir))
    (let loop
      ([files (map path->string (directory-list dir-path))] [dir dir-path])
      (cond
        [(empty? files)]
        [else
          (write-file (string-append dir "/" (car files)))
          (loop (cdr files) dir)]))))

(define (make-threads files dir-path)
  (list (future (lambda ()
    (let loop
      ([lst files])
        (cond
          [(empty? lst)]
          [else
            (write-file (string-append dir-path "/" (car lst)))
            (loop (cdr lst))]))))))

; Concurrent execution
(define (write-files-parallel dir-path threads)
  (let ([out-dir "results"])
    ; Create results directory if it doesn't exist
    (unless (directory-exists? out-dir)
      (make-directory out-dir)))
  ; Get all filenames
  (define files (map path->string (directory-list dir-path)))
  (displayln (string-append "Found files: " (number->string (length files))))
  ; Distribute files among threads
  (let loop
    ([files files] [futures empty] [counter 1] [total-files (length files)])
    (cond
      [(empty? files)
        (for-each touch futures)]
      [(< counter threads)
        (let-values ([(head tail)  (split-at files (- (length files) (floor (/ total-files threads))))])
          (loop head (append futures (make-threads tail dir-path)) (add1 counter) total-files))]
      [(= counter threads)
        (loop empty (append futures (make-threads files dir-path)) counter total-files)])))

; Measure execution time for the whole algorithm, takes in "<filename>.cpp"
(define (timer doc)
  (define begin (current-inexact-milliseconds))
  (write-file doc)
  (- (current-inexact-milliseconds) begin))

; Measure execution time for the replace-match function, takes in "<filename>.cpp"
(define (timer2 doc)
  (define begin (current-inexact-milliseconds))
  (replace-match doc)
  (- (current-inexact-milliseconds) begin))

; Example usage:
(define (main)
  (define single-file "examples/file1.cpp")
  (define dir-path "examples")
  (define threads 4) ; Number of threads to use

  ; Ensure results directory is created
  (unless (directory-exists? "results")
    (make-directory "results"))

  ; Measure and print execution time for a single file
  (displayln (string-append "Execution time for single file: " (number->string (timer single-file)) " ms"))

  ; Measure and print execution time for the replace-match function
  (displayln (string-append "Execution time for replace-match: " (number->string (timer2 single-file)) " ms"))

  ; Process files in parallel
  (displayln "Processing files in parallel...")
  (write-files-parallel dir-path threads)
  (displayln "Parallel processing completed."))

; Run the main function
(main)