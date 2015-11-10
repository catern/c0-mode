;; I let emacs guess the indent style of example c0 code and saved it as a style
(c-add-style "c0-allman"
             '("bsd" ; c0-allman inherits from this style
               (c-basic-offset . 2)
               (c-indent-comments-syntactically-p . t)
               (c-offsets-alist
                ; guessed style, which I modified a little
                (block-close . 0)
                (block-open . 0)
                (class-close . 0)
                (class-open . 0)
                (comment-intro . (lambda (x) (if (assq 'substatement c-syntactic-context) '- 'c-lineup-comment))) ; only indents comments if not right below an if, while, for, etc.; it would be better to only do this for contracts, and indent normal comments normally
                (defun-block-intro . +)
                (defun-close . 0)
                (defun-open . 0)
                (else-clause . 0)
                (func-decl-cont . 0)
                (inclass . +)
                (statement . 0)
                (statement-block-intro . +)
                (substatement . +)
                (substatement-open . 0)
                (topmost-intro . 0)
                (topmost-intro-cont . 0)
                ; non-guessed style defaults, should be ok
                (access-label . -)
                (annotation-top-cont . 0)
                (annotation-var-cont . +)
                (arglist-close . c-lineup-close-paren)
                (arglist-cont c-lineup-gcc-asm-reg 0)
                (arglist-cont-nonempty . c-lineup-arglist)
                (arglist-intro . +)
                (brace-entry-open . 0)
                (brace-list-close . 0)
                (brace-list-entry . 0)
                (brace-list-intro . +)
                (brace-list-open . 0)
                (c . c-lineup-C-comments)
                (case-label . 0)
                (catch-clause . 0)
                (composition-close . 0)
                (composition-open . 0)
                (cpp-define-intro c-lineup-cpp-define +)
                (cpp-macro . -1000)
                (cpp-macro-cont . +)
                (do-while-closure . 0)
                (extern-lang-close . 0)
                (extern-lang-open . 0)
                (friend . 0)
                (incomposition . +)
                (inexpr-class . +)
                (inexpr-statement . +)
                (inextern-lang . +)
                (inher-cont . c-lineup-multi-inher)
                (inher-intro . +)
                (inlambda . c-lineup-inexpr-block)
                (inline-close . 0)
                (inline-open . +)
                (inmodule . +)
                (innamespace . +)
                (knr-argdecl . 0)
                (knr-argdecl-intro . +)
                (label . 2)
                (lambda-intro-cont . +)
                (member-init-cont . c-lineup-multi-inher)
                (member-init-intro . +)
                (module-close . 0)
                (module-open . 0)
                (namespace-close . 0)
                (namespace-open . 0)
                (objc-method-args-cont . c-lineup-ObjC-method-args)
                (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
                (objc-method-intro .
                                   [0])
                (statement-case-intro . +)
                (statement-case-open . 0)
                (statement-cont . +)
                (stream-op . c-lineup-streamop)
                (string . -1000)
                (substatement-label . 2)
                (template-args-cont c-lineup-template-args +))))


;; make the default c0 style c0-allman
(add-to-list 'c-default-style '(c0-mode . "c0-allman"))
