#!/usr/bin/newlisp

;;; purpose

; A command line programmer's todo list manager for the French

;;; defs

; defines default config filename
(define config-file "todo.conf")

; defines main text todo list filename
(context 'SETTINGS)
(define todo-file "todo.txt")
(context MAIN)

; convert todo filename to todo list title
(define (filename-to-title filename)
    (letn (no-ext  ((parse filename ".") 0) ; extract filename without extension
          (no-dash (if (member "-" no-ext) (join (map title-case (parse no-ext "-")) " ")
                                           (title-case no-ext))))
        (trim no-dash)))

; replace each pair of strings in the list l in the source s
; i.e. (multi-replace '("some" "none" "something" "else") "some string, something"))
(define (multi-replace l s)
    (if (or (null? l) (null? s)) s
        (if (< (length l) 2) s
            (let (r1 (l 0) 
                  r2 (l 1)) 
                (multi-replace (2 l) (replace (l 0) s (l 1)))))))

; add missing accents from common keywords
(define (add-accents s)
    (multi-replace '("design"         "désign"
                     "implementation" "implémentation"
                     "procedure"      "procédure"
                     "fenetre"        "fenêtre")
                   s))

; convert a todo item to test
(define (convert-to-test s)
    (multi-replace '("désign et implémentation d'une"          "test de la"
                     "implémentation d'une"                    "test de la"
                     "travail sur la"                          "test de la"
                     "désign et début d'implémentation d'une"  "test de la"
                     "mise à jour du code"                     "test du code"
                     "mise à jour de la procédure"             "test de la procédure"
                     "dans la fenêtre"                         "de la fenêtre")
                    (if (member "mise à jour" s)
                        (multi-replace '("de la fenêtre" "à jour de la fenêtre"
                                         "dans la fenêtre" "à jour de la fenêtre") s)
                        s)))

; define todo item editor shortcuts
(define shortcuts '(" dp " "désign et implémentation d'une procédure "
                    " up " "mise à jour de la procédure "
                    " uc " "mise à jour du code "
                    " fp " " dans la fenêtre principale de l'application "
                    " fe " " dans la fenêtre de l'éditeur "
                    " f "  " dans la fenêtre "))

; print pairs of items with ':' in between
(define (print-pairs l)
    (if (null? l) nil
        (if (< (length l) 2) nil
            (let (r1 (l 0) 
                  r2 (l 1))
                (println r1 ": " r2) 
                (print-pairs (2 l))))))

(define html-header "<!DOCTYPE html><html><head><title>Todo List</title><meta http-equiv='content-type' content='text/html;charset=utf-8' /></head><body><font face='Arial' point-size='8'>")
(define list-header "<ul><li>")
(define html-footer "</font></body></html>")

; convert a raw list into an HTML bullet list
(define (make-html-list s)

    ; original Xojo code
    ;If sList.Right(1) = ";" Then sList = sList.Left(sList.Len - 1) + ".</li></ul></ul>"
    ;If sList.Right(1) = "." Then sList = sList.Left(sList.Len - 1) + ".</li></ul></ul>"
    ;If sList.Right(1) = ":" Then sList = sList.Left(sList.Len - 1) + ":</li></ul>"
    ;sList = sList.ReplaceAll(";" + EndOfLine, ";</li><li>")
    ;sList = sList.ReplaceAll(":" + EndOfLine, ":<ul><li>")
    ;sList = sList.ReplaceAll("." + EndOfLine, ".</li></ul><li>")
    ;sHTMLContent = sHTMLContent + sList

    (when (empty? s) nil)
    (if (= (last s) ";") (set s (append (chop s 1) ".</li></ul></ul>")))
    (if (= (last s) ".") (set s (append (chop s 1) ".</li></ul></ul>")))
    (if (= (last s) ":") (set s (append (chop s 1) ":</li></ul>")))
    (setq s (replace ";\n" s ";</li><li>"))
    (setq s (replace ":\n" s ":<ul><li>"))
    (setq s (replace ".\n" s ".</li></ul><li>"))
    (setq s (append html-header list-header s html-footer)))

; convert existing text todo list to html
(define (write-html-list src-filename dst-filename)
    (let   (todo-text (read-file  src-filename))
        (if todo-text (begin (println "Writing html list...\n")
                             (write-file dst-filename 
                                         (make-html-list todo-text)))
                      (println "No todo list to process."))))

; display the contents of the current list file
(define (print-list)
    (println (read-file SETTINGS:todo-file)))

; adds a semi-colon if missing and add end of line
(define (tidy-up s)
    (if (or (ends-with s ";")
            (ends-with s ";\n")) (append (trim s) "\n")
                                 (append (trim s) ";\n")))

; helper to get a name from a command line
(define (get-dashed-name-from-command line)
    (join (1 (parse line " ")) "-"))

; main input handler
(define (handle-input line)
    (case line
        ("q"    (save-and-quit config-file))
        ("p"    (print-list) (input-loop))
        ("e"    (process (append "/usr/bin/vim " SETTINGS:todo-file 0 0)))
        ("w"    (write-html-list SETTINGS:todo-file "todo.html") (input-loop))
        ("h"    (print-help) (input-loop))
        ("help" (print-help) (input-loop)))

    (when (= (first line) "(")      (eval-string line)      (println) (input-loop))
    (when (starts-with line "n ")   (create-new-todo-list   (get-dashed-name-from-command line)) (input-loop))
    (when (starts-with line "l ")   (load-todo-list         (get-dashed-name-from-command line)) (input-loop))      
    (when (= (length line) 1)       (println "Unknown command. Enter h for help.\n")
                                    (input-loop)) ; default when a single letter is entered

    (letn (result       (append " "   (add-accents line) " ")                   ; add accents
           todo-item    (tidy-up (multi-replace shortcuts result))  ; replace shortcuts by their full text
           display-todo (print "Added  : " todo-item)
           test-item    (tidy-up (convert-to-test todo-item))  ; generate a matching test line - often todo-item is empty!
           display-test (print "Tested : " test-item))
        (append-file SETTINGS:todo-file todo-item)                                      ; record todo line in file
        (append-file SETTINGS:todo-file test-item))                                     ; record test line in file
    )

; create new todo list file
(define (create-new-todo-list list-name)
    (let (filename (append list-name ".txt"))
        (setq SETTINGS:todo-file filename)))

; load todo list
(define (load-todo-list list-name)
    (let (filename (append list-name ".txt"))
        (if (file? filename)
            (setq SETTINGS:todo-file filename)
            (println "Todo list does not exist.\n"))))

; display an intro and help page
(define (print-help)
    (println "Todo Master v1.0 - by Dexter Santucci - December 2018")
    (println "Commands:\n"
             " h : this help page\n"
             " n [todo list name]: create new todo list\n"
             " l [todo list name]: load todo list\n"
             " p : print current list\n"
             " w : export current list to HTML\n"
             " e : edit current list\n"
             " q : quit\n")
    (println "Shortcuts:")
    (print-pairs shortcuts)
    (println))

; main loop procedure
(define (input-loop)
    (print "Command: ")
    (define input-line (trim (read-line)))
    (when (or (null? input-line)
              (nil?  input-line)) (input-loop))
    (handle-input input-line)                   ; handle different inputs
    (input-loop))                               ; launch loop again

; attempt to load settings file
(define (maybe-load-settings filename)
    (if (file? filename) (eval-string (read-file filename))))

; save settings and quit
(define (save-and-quit config-file)
    (write-file config-file (append "(setq SETTINGS:todo-file \"" SETTINGS:todo-file "\")\n"))
    (exit 0))

;;; main

(print-help)
(maybe-load-settings config-file)
(input-loop)
(save-and-quit config-file)

; EOF
