#lang racket

(require racket/future)

; Función auxiliar para leer el contenido del archivo en líneas
(define (file->lines file)
  (displayln (string-append "Leyendo archivo: " file))
  (call-with-input-file file
    (lambda (in)
      (let loop ([lines '()])
        (let ([line (read-line in 'any)])
          (if (eof-object? line)
              (begin
                (displayln (string-append "Archivo leído exitosamente: " file))
                (reverse lines))
              (loop (cons line lines))))))))

; Función auxiliar para escribir líneas en un archivo
(define (display-lines-to-file lines file #:exists exists)
  (displayln (string-append "Escribiendo en archivo: " file))
  (call-with-output-file file #:exists exists
    (lambda (out)
      (for-each (lambda (line)
                  (display line out)
                  (newline out))
                lines))))

; Función para reemplazar todas las instancias en una cadena
(define (string-replace-all s replacements)
  (foldl (lambda (pair acc)
           (regexp-replace* (car pair) acc (cdr pair)))
         s
         replacements))

; Función para reemplazar tokens en código C++ con tokens HTML formateados
(define (replace-match in-path)
  (displayln (string-append "Reemplazando tokens en archivo: " in-path))
  (define lines (file->lines in-path))
  (let loop
    ([lines lines] [res empty])
    (if (null? lines)
        res
        (let*
           ([dis-line (car lines)]
            [dis-line (string-replace dis-line "<" "&lt;")] ; Reemplazar "<" con "&lt;"
            [dis-line (string-replace dis-line ">" "&gt;")] ; Reemplazar ">" con "&gt;"
            ; Reemplazar palabras clave primero para evitar etiquetas anidadas
            [dis-line (regexp-replace* #px"\\b(int|float|double|char|void|bool|if|else|for|while|do|return|class|struct|public|private|protected|const|static|virtual|override|namespace|using|new|delete|sizeof|typedef|typename|template|try|catch|throw|this|true|false)\\b" dis-line "<span class=keywords>\\0</span>")]
            ; Reemplazar directivas del preprocesador
            [dis-line (regexp-replace* #px"#\\w+" dis-line "<span class=preprocessor>\\0</span>")]
            ; Reemplazar cadenas
            [dis-line (regexp-replace* #px"\"([^\"]*)\"" dis-line "<span class=strings>\"\\1\"</span>")]
            ; Reemplazar caracteres
            [dis-line (regexp-replace* #px"'([^']*)'" dis-line "<span class=chars>'\\1'</span>")]
            ; Reemplazar comentarios
            [dis-line (regexp-replace* #px"//.*" dis-line "<span class=comments>\\0</span>")]
            [dis-line (regexp-replace* #px"/\\[\\^\\]/" dis-line "<span class=comments>\\0</span>")] ; Corregido el regex aquí
            ; Reemplazar números
            [dis-line (regexp-replace* #px"\\b\\d+\\.?\\d*\\b" dis-line "<span class=numbers>\\0</span>")])
          (loop (cdr lines) (append res (list dis-line)))))))

; Función para verificar la sintaxis del código C++
(define (check-syntax file)
  (system (string-append "g++ -fsyntax-only " file " 2> syntax_check.txt"))
  (let ([errors (file->lines "syntax_check.txt")])
    (if (null? errors)
        "sintaxis correcta"
        "sintaxis incorrecta")))

; Crear archivo HTML con sintaxis resaltada para un archivo C++ dado
(define (write-file in-path)
  (define out-dir "results") ; Directorio de salida
  ; Crear directorio de resultados si no existe
  (unless (directory-exists? out-dir)
    (make-directory out-dir))
  (define out-path
    (string-append out-dir "/" (regexp-replace #px".*?([^/]+)\\.cpp" in-path "\\1.html")))
  (displayln (string-append "Creando archivo de salida: " out-path))
  ; Agregar HTML al inicio
  (call-with-output-file out-path #:exists 'truncate
    (lambda (out)
      (display "<!DOCTYPE html>
        <html lang=\"es\">
        <head>
            <meta charset=\"UTF-8\">
            <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">
            <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
            <title>Resaltador de C++</title>
            <style>
            .keywords { color: blue; font-weight: bold; }
            .strings { color: green; }
            .chars { color: orange; }
            .comments { color: gray; font-style: italic; }
            .numbers { color: red; }
            .preprocessor { color: purple; font-weight: bold; }
            .footer { margin-top: 20px; border: 1px solid #ccc; padding: 10px; background-color: #f0f0f0; }
            </style>
        </head>
        <body>
        <h1>Resaltador de C++</h1>
        <pre><code>"
        out)))

  ; Agregar sintaxis resaltada
  (display-lines-to-file (replace-match in-path) out-path #:exists 'append)

  ; Agregar el recuadro en la parte inferior con el resultado de la verificación de sintaxis
  (call-with-output-file out-path #:exists 'append
    (lambda (out)
      (define syntax-result (check-syntax in-path))
      (display (string-append "<div class=\"footer\">" syntax-result "</div>")
        out)))

  ; Cerrar la etiqueta HTML
  (call-with-output-file out-path #:exists 'append
    (lambda (out)
      (display "</code></pre>
        </body>
        </html>"
        out)))
  (displayln (string-append "Archivo de salida creado exitosamente: " out-path)))

; Crear archivos HTML con sintaxis resaltada para todos los archivos C++ en un directorio dado
(define (write-files dir-path)
  (let ([out-dir "results"])
    ; Crear directorio de resultados si no existe
    (unless (directory-exists? out-dir)
      (make-directory out-dir))
    (let loop
      ([files (map path->string (directory-list dir-path))] [dir dir-path])
      (cond
        [(empty? files)]
        [else
          (write-file (string-append dir "/" (car files)))
          (loop (cdr files) dir)]))))

; Crear hilos para ejecución concurrente
(define (make-threads files dir-path)
  (list (future (lambda ()
    (let loop
      ([lst files])
      (cond
        [(empty? lst) 'done]
        [else
          (write-file (string-append dir-path "/" (car lst)))
          (loop (cdr lst))]))))))

; Ejecución concurrente
(define (write-files-parallel dir-path threads)
  (let ([out-dir "results"])
    ; Crear directorio de resultados si no existe
    (unless (directory-exists? out-dir)
      (make-directory out-dir)))
  ; Obtener todos los nombres de archivo
  (define files (map path->string (directory-list dir-path)))
  (displayln (string-append "Archivos encontrados: " (number->string (length files))))
  ; Distribuir archivos entre hilos
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

; Medir tiempo de ejecución para todo el algoritmo, toma "<nombre_archivo>.cpp"
(define (timer doc)
  (define begin (current-inexact-milliseconds))
  (write-file doc)
  (- (current-inexact-milliseconds) begin))

; Medir tiempo de ejecución para la función replace-match, toma "<nombre_archivo>.cpp"
(define (timer2 doc)
  (define begin (current-inexact-milliseconds))
  (replace-match doc)
  (- (current-inexact-milliseconds) begin))

; Función principal
(define (main)
  (define single-file "examples/file1.cpp")
  (define dir-path "examples")
  (define threads 4) ; Número de hilos a utilizar

  ; Asegurar que el directorio de resultados esté creado
  (unless (directory-exists? "results")
    (make-directory "results"))

   ; Medir e imprimir tiempo de ejecución para la función replace-match
  (displayln (string-append "Tiempo de ejecución para replace-match: " (number->string (timer2 single-file)) " ms"))

  ; Procesar archivos en paralelo
  (displayln "Procesando archivos en paralelo...")
  (write-files-parallel dir-path threads)
  (displayln "Procesamiento en paralelo completado."))

; Ejecutar la función principal
(main)