;;; c0-code.el --- Interaction with 'code', the C0 debugger

;; Author:     2010 Jakob Max Uecker
;; Maintainer: 
;; Created:    August 2010
;; Modified:   August 2010
;; Version:    0.1
;; Keywords:   c0 debugger

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;    This code interacts with the 'code' debugger. It expects
;;    the variable 'code-path' to be set to the 'code' executable

;;; Known Bugs:
;;    
;;
;;

;;; Versions:
;;
;;    0.1 - Initial release

;; User options
(defvar code-path nil
  "*Path pointing to the code executable")

;; Faces for highlighting the active portion of code
(defface code-normal-face
  '((((class color)
      (background dark))
     (:background "Yellow" :bold t :foreground "Black"))
    (((class color)
      (background light))
     (:background "Yellow" :bold t))
    (t
     ()))
  "*Face used for the next expression to be evaluated."
  :group 'code)

(defface code-error-face
  '((((class color)
      (background dark))
     (:background "Pink" :bold t :foreground "Black"))
    (((class color)
      (background light))
     (:background "Pink" :bold t))
    (t
     ()))
  "*Face used for highlighting erroneous expressions."
  :group 'code)

;; The keymap used for debugging
(defvar code-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'code-step)
    (define-key map (kbd "RET") 'code-step)
    (define-key map "n" 'code-next)
    (define-key map "v" 'code-locals)
    (define-key map "e" 'code-eval-exp)
    (define-key map "q" 'code-exit-debug)
    (define-key map "i" 'code-interrupt)
    (define-key map "?" 'code-help)
    (define-key map "h" 'code-help-long)
    map))

;;; Local variables follows
;;; These should not be set by the user

(defvar code-locals-buffer nil
  "Buffer which will display values of local variables")

(defvar code-locals-accum nil
  "Accumulator for local variable values for 'v' command")

(defvar code-output-accum nil
  "Accumulator for output, shown when prompt is encountered")

(defvar code-highlight-overlay nil
  "Overlay to highlight currently evaluated region")

(defvar code-point-old nil
  "Old point in current code buffer")

(defvar code-local-map-old nil
  "Old local keymap in current code buffer")

(defvar code-proc nil
  "Process running code")

(defvar code-main-directory nil
  "Directory of the C0 file with main function")

;; Column positions between cc0 and emacs are off by 1
(defun code-get-pos (line column)
  "Get buffer position from line.column position"
  (+
   (line-beginning-position (- (+ line 1) (line-number-at-pos)))
   (- column 1)))

;;; Functions for highlighting

(defun code-highlight
  (line-begin column-begin line-end column-end)
  "Move highlight overlay to specified region, point to beginning of region"
  (let ((pos-begin (code-get-pos line-begin column-begin))
	(pos-end (code-get-pos line-end column-end)))
    (if (not (null code-highlight-overlay))
	(move-overlay code-highlight-overlay pos-begin pos-end)
      (setq code-highlight-overlay (make-overlay pos-begin pos-end)))
    (goto-char pos-begin)))

(defun code-highlight-normal (line-begin column-begin line-end column-end)
  "Set normal highlight"
  (code-highlight line-begin column-begin line-end column-end)
  (overlay-put code-highlight-overlay 'face 'code-normal-face))

(defun code-highlight-error (line-begin column-begin line-end column-end)
  "Set highlight to indicate error"
  (code-highlight line-begin column-begin line-end column-end)
  (overlay-put code-highlight-overlay 'face 'code-error-face))

(defun code-delete-highlight ()
  "*Remove highlight overlay"
  (if (not (null code-highlight-overlay))
      (progn
	(delete-overlay code-highlight-overlay)
	(setq code-highlight-overlay nil))))

(defun code-enter-buffer ()
  "Set current buffer to code mode"
  (setq buffer-read-only t)
  (setq code-local-map-old (current-local-map))
  (use-local-map code-map)
  (setq code-point-old (point)))

(defun code-leave-buffer ()
  "Restore current buffer to normal mode"
  (code-delete-highlight)
  (use-local-map code-local-map-old)
  (goto-char code-point-old)
  (setq buffer-read-only nil))

(defun code-switch-to-file (filename)
  "Switch to stepping in filename"
  (code-leave-buffer)			; leave current buffer
  (find-file-existing filename)		; visit other file
  (code-enter-buffer)			; enter into debug mode
  )

(defun code-display-output-accum ()
  "Display accumulated output, if not empty"
  (if (not (null code-output-accum))
      (progn
	(with-current-buffer code-locals-buffer
	  (goto-char (point-max))
	  (insert code-output-accum))
	(setq code-output-accum nil))))

;;; Functions for parsing of debugger output

(defun code-canon-filename (filename)
  "Canonicalize the given filename, relative to code main directory"
  (if (file-name-absolute-p filename)
      filename
    (expand-file-name (concat code-main-directory filename))))

(defun code-parse (string)
  (let ((newline-pos (string-match "\n" string)))
    (if (null newline-pos)
	;; no trailing newline
	(code-parse-line string string)
      ;; trailing newline
      (if (not (code-parse-line (substring string 0 newline-pos) string))
	  (code-parse (substring string (+ 1 newline-pos)))))))

(defconst code-position-regexp
  "\\([0-9]*\\)[.]\\([0-9]*\\)-\\([0-9]*\\)[.]\\([0-9]*\\)"
  "Regular expression matched against code position.  Must define 4 numbers.")

(defconst code-location-regexp
  (concat "^\\([^:]*\\):" code-position-regexp)
  "Regular expression matched against debugger output")

(defconst code-interactive-regexp
  (concat "^\\(<debugger>\\):" code-position-regexp)
  "Regular expression matched against error in interactive parsing")

(defconst error-msg-regexp
  "\\(:error:.*\\|: @.*\\|: assert failed\\)"
  "Regular expression matched against error output")

(defun code-parse-line (string whole-string)
  "Parse one line of output from code program, returns non-NIL to abort rest"
  (cond ((string-match "^(code)" string)
	 ;; prompt - display values of local variables
         ;; and accumulated output since last prompt
	 (if (not (null code-locals-accum))
	     (progn
	       ;; (message "%d" (length code-locals-accum))
	       (with-current-buffer code-locals-buffer
		 (delete-region (point-min) (point-max))
		 (insert code-locals-accum)
		 (goto-char (point-max)))
	       (setq code-locals-accum nil))
             ;; Bugfix - entering a new function doesn't clear locals
             ;; - rjs 8/24/2012
             (progn
	       (with-current-buffer code-locals-buffer
		 (delete-region (point-min) (point-max))
		 (insert "(no local variables)")
		 (goto-char (point-max)))))
	 (if (not (null code-output-accum))
	     (progn
	       (message "%s" code-output-accum)
	       (setq code-output-accum nil)))
	 ())
	((string-match "^main function" string)
	 ;; main function returned
	 (code-exit-debug)
	 (message "%s" string)
	 ())
	((string-match (concat "^" error-msg-regexp) string)
	 ;; :error:<errormsg>
	 (let ((errormsg (match-string 1 string)))
	   (code-exit-debug)
	   (with-current-buffer code-locals-buffer
	     (delete-region (point-min) (point-max))
	     (insert whole-string))
	   (message "%s" errormsg)
	   ;; abort more parsing (return t)
	   t))
	((string-match (concat code-interactive-regexp error-msg-regexp) string)
	 ;; error message during interactive parsing or type-checking
	 ;; applies during "e <exp>" evaluation
	 ;; ignore error location, just show error mess
	 (let* ((errormsg (match-string 6 string)))
	   (message "%s" errormsg)
	   ;; continue parsing
	   ()))
	((string-match "^<debugger>:\\(.*\\)" string)
	 ;; remaining runtime messages from debugger during "e <exp" evaluation
	 ;; must come after the located error message above and before
	 ;; the general runtime error below
	 (let* ((errormsg (match-string 1 string)))
	   (message "%s" errormsg)
	   ;; continue parsing
	   ()))
	((string-match (concat code-location-regexp error-msg-regexp) string)
	 ;; error message during parsing or type-checking
	 ;; must come before the next clause
	 (let* ((canon-filename (code-canon-filename (match-string 1 string)))
		(line0 (string-to-number (match-string 2 string)))
		(col0 (string-to-number (match-string 3 string)))
		(line1 (string-to-number (match-string 4 string)))
		(col1 (string-to-number (match-string 5 string)))
		(errormsg (match-string 6 string)))
	   (if (not (string-equal canon-filename (buffer-file-name)))
	       (code-switch-to-file canon-filename))
	   (code-exit-debug)
	   (code-highlight-error line0 col0 line1 col1)
	   (with-current-buffer code-locals-buffer
	     (delete-region (point-min) (point-max))
	     (insert whole-string))
	   (message "%s" errormsg)
	   ;; abort more parsing (return t)
	   t))
	((string-match code-location-regexp string)
	 ;; location of code currently executed
	 (let* ((canon-filename (code-canon-filename (match-string 1 string)))
		(line0 (string-to-number (match-string 2 string)))
		(col0 (string-to-number (match-string 3 string)))
		(line1 (string-to-number (match-string 4 string)))
		(col1 (string-to-number (match-string 5 string))))
	   (if (not (string-equal canon-filename (buffer-file-name)))
	       (code-switch-to-file canon-filename))
	   (code-highlight-normal line0 col0 line1 col1)
	   ()))
	((string-match "^Exception: \\(.*\\)" string)
	 (code-exit-debug)
	 (with-current-buffer code-locals-buffer
	   (delete-region (point-min) (point-max))
	   (insert whole-string))
	 (message "%s" whole-string)
	 ;; abort more parsing (return t)
	 t)
	((string-match "^\\(_tmp_[0-9]*\\|_caller\\): " string)
	 ;; _tmp_n: value or _caller: value
	 ;; do not display values of temporary variables
	 ())
	((string-match "^[a-zA-Z0-9_]*: " string)
	 ;; varname: value, accumulate to be shown upon next prompt
	 ;; might be better solved with \\w*, but above is more specific
	 (setq code-locals-accum (concat code-locals-accum string "\n"))
	 ())
	;; suppress blank lines
	((string-equal "\n" string) ())
	((string-equal "" string) ())
	(t
	 ;; collect other output, usually from print statements
	 ;; in program being executed, or from "e <exp>" commands
	 (setq code-output-accum (concat code-output-accum string "\n"))
	 ())
	))

;;; Filter and Sentinel functions

;; Receives output from the debugger. Logs all output in
;; the debugger's associated buffer before passing it on
;; to the parsing function
(defun code-filter (proc string)
  "Filter function for code interaction"
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	(goto-char (process-mark proc))
	(insert string)
	(set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc)))))
  (code-parse string))
	 
;; Is called if the debugger process receives a signal
;; or exits
(defun code-sentinel (proc string)
  "Sentinel for code process"
  (cond
   ((string-match "^exited abnormally" string)
    (code-exit-debug)
    (code-display-output-accum)
    (message "%s" "program aborted"))
   (t (code-exit-debug)
      (code-display-output-accum)
      (message "%s" "unexpected termination of code"))))

;;; Functions for sending input to the debugger

(defun code-send-string (string)
  "Send STRING to code process"
  (process-send-string code-proc string))

(defun code-step ()
  "Step to next statement, potentially entering a function"
  (interactive)
  (code-send-string "s\n")
  (code-send-string "v\n"))

(defun code-next ()
  "Step to next statement, passing over functions unless they
include a breakpoint"
  (interactive)
  (code-send-string "n\n")
  (code-send-string "v\n"))

(defun code-eval-exp (exp)
  "Evaluate an expression in the current state"
  (interactive "sEvaluate expression: ")
  (code-send-string (concat "e " exp "\n"))
  (code-send-string "v\n"))

(defun code-locals ()
  "Show the value of local variables"
  (interactive)
  (code-send-string "v\n"))

(defun code-interrupt ()
  "Interrupt the debugger"
  (interactive)
  (interrupt-process "code"))

(defun code-help ()
  "Show the Emacs help for code"
  (interactive)
  (message "%s\n%s\n%s\n%s\n%s\n%s\n%s"
	   "return or s - step"
	   "n - next (skip function calls)"
	   "e <exp> - evaluate <exp>"
	   "q - quit"
	   "i - interrupt code"
	   "? - this short help"
	   "h - detailed help"))

(defun code-help-long ()
  "Show the longish help for code"
  (interactive)
  (find-file-other-frame (concat c0-root "c0-mode/README")))

;;; Enter and Exit functions

;; Start the debugger on the current buffer. The buffer must
;; be associated with ('visiting') a file.
;; After initial checks, the function
;; -makes the buffer read only
;; -saves the current keymap and point
;; -adds a hook that quits the debugger if the buffer is killed
;; -runs the debugger

(defun code ()
  "Enter debugging mode in current buffer."
  (interactive)
  (code-enter-debug))

(defun code-enter-debug ()
  "Enter debugging mode."
  (interactive)
  (if (get-process "code")
      (message "%s" "debugger already running")
    (if (null code-path)
	(message "%s" "debugger path not set")
      (if (and (buffer-modified-p) (yes-or-no-p "save buffer? "))
	  (save-buffer))
      (setq args (read-string "Call debugger with: code" 
			      (concat " -e " (file-relative-name (buffer-file-name)))))
      ;; start code process
      (setq code-proc
	    (start-process-shell-command
	     "code"
	     "*code*"
	     code-path
	     args))
      ;; kill-buffer-hook activated when switching to another file
      ;; the first time; disabled
      ;; (add-hook 'kill-buffer-hook 'code-kill-process)
      (setq code-output-accum nil)	; in case we are in a strange state
      (setq code-locals-accum nil)	; in case we are in a strange state
      (set-process-filter code-proc 'code-filter)
      (set-process-sentinel code-proc 'code-sentinel)
      ;; switch current buffer to debugging mode
      (code-enter-buffer)
      (setq code-main-directory (file-name-directory (buffer-file-name)))
      ;; create and display buffer for local variables
      (setq code-locals-buffer (get-buffer-create "*code-locals*"))
      (display-buffer code-locals-buffer) ; do not switch
      (save-window-excursion
	(switch-to-buffer-other-window code-locals-buffer)
	(delete-region (point-min) (point-max)))
      (message "Type '?' for help")
      )))

;; Hook to be run if the buffer is killed while debugging
;; Kills the debugger
(defun code-kill-process ()
  (if (get-process "code")
      (delete-process "code")))

;; Quit the debugger. Restores the buffers keymap and point
(defun code-exit-debug ()
  "Exit debugging mode"
  (interactive)
  (code-kill-process)
  ;; for now, keep buffers
  ;; (kill-buffer "*code*")
  ;; (kill-buffer code-locals-buffer)
  ;; (remove-hook 'kill-buffer-hook 'code-kill-process)
  (code-leave-buffer)
  (message "%s" "code exited"))

(provide 'c0-code)

;;; c0-code.el ends here
