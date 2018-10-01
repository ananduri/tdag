;; Emacs lisp for editing the TDAG DSL

;; require paredit

;; use electric return in paredit
(defvar electrify-return-match
    "[\]}\)\"]"
    "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

(defun electrify-return-if-match (arg)
    "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
    (interactive "P")
    (let ((case-fold-search nil))
      (if (looking-at electrify-return-match)
	  (save-excursion (newline-and-indent)))
      (newline arg)
      (indent-according-to-mode)))


(global-set-key (kbd "RET") 'electrify-return-if-match)


;; indent when pressing enter
;; one more way to improve:
;;  2. cache par-count and re-calculate with a diff
(defun return-and-indent (arg)
  (interactive "P")
  (let ((par-count 0)
        (curr (point)))
    (save-excursion
      (beginning-of-buffer)
      (while (not (= curr (point)))
        (if (looking-at "(")
            (incf par-count))        
        (if (looking-at ")")
            (decf par-count))
        (forward-char 1)))
    (electrify-return-if-match arg)
    (beginning-of-line)
    (do-n-times par-count (insert " "))))


(defvar par-count 0
  "Local var that stores the number of open parens before the point")

(define-minor-mode tdag-mode
  "paredit + indentation for TDAG DSL"
  :lighter " TDAG"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "RET") 'return-and-indent)
            map)  
  (enable-paredit-mode)
  (make-local-variable 'par-count))

(defmacro do-n-times (n cmd)
  `(let ((count 0))
     (while (not (= count ,n))
       ,cmd
       (incf count))))

;;(macroexpand '(do-n-times 2 (insert " ")))

;; (let ((count 0))
;;   (while (not (= count 2))
;;     (insert " ")
;;     (incf count)))  







;; indent in TDAG DSL
;; for testing purposes
(defun get-indentation-level ()
  "gets the indentation level by counting
   remaining open pairs of parens"
  (interactive)
  (save-excursion  
    (let ((par-count 0)
          (curr (point)))
      (beginning-of-buffer)
      (while (not (= curr (point)))
        (if (looking-at "(")
            (incf par-count))        
        (if (looking-at ")")
            (decf par-count))
        (forward-char 1))
      (message "level is %d" par-count)
      )))



;; syntax highlighting, not working yet.
;; currently it thinks this is a different mode
(require 'generic-x)
(define-generic-mode  
  'tdag-mode          ;; name of the mode
  '(";")              ;; comments delimiter
  '()                 ;; keywords
  '()                 ;; list of operators
  '("\\.dag$")        ;; file extensions that trigger this mode
  nil
  "dag highlighting mode")
