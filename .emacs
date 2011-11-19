(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(flymake-allowed-file-name-masks (quote nil))
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/org/todo.org")))
 '(py-pychecker-command "check.py")
 '(py-pychecker-command-args (quote ("")))
 '(python-check-command "check.py"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(let ((default-directory "~/.emacs.d/site-lisp/"))
(normal-top-level-add-to-load-path '("."))
(normal-top-level-add-subdirs-to-load-path))

(when (display-graphic-p)
  (require 'color-theme)
  (color-theme-initialize)

  ;(load-file "~/.emacs.d/color-theme-almost-monokai.el")
  (require 'color-theme-almost-monokai)
  (color-theme-almost-monokai))

;(require 'linum)
(global-linum-mode 1)
(setq linum-mode-inhibit-modes-list '(eshell-mode
                                      shell-mode
                                      erc-mode
                                      jabber-roster-mode
                                      jabber-chat-mode
                                      gnus-group-mode
                                      gnus-summary-mode
                                      gnus-article-mode))

(defadvice linum-on (around linum-on-inhibit-for-modes)
  "Stop the load of linum-mode for some major modes."
    (unless (member major-mode linum-mode-inhibit-modes-list)
      ad-do-it))

(ad-activate 'linum-on)

;; Set autoindent to return
(define-key global-map (kbd "RET") 'newline-and-indent)

(if (display-graphic-p) (set-frame-size (selected-frame) 265 82))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(global-set-key [M-left] 'windmove-left)          ; move to left windnow
(global-set-key [M-right] 'windmove-right)        ; move to right window
(global-set-key [M-up] 'windmove-up)              ; move to upper window
(global-set-key [M-down] 'windmove-down)          ; move to downer window

(delete-selection-mode t)

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

(setenv "PATH" (concat "/Users/mkelly/local/node/bin:/usr/local/share/python:/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin", "/usr/local/share/python", "/Users/mkelly/local/node/bin")))


(defun shell ()
  (interactive)
  (ansi-term "/bin/zsh"))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'auto-complete)
(global-auto-complete-mode t)

(add-to-list 'load-path
              "~/.emacs.d/vendor/yasnippet-0.6.1c")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/vendor/yasnippet-0.6.1c/snippets")

(add-to-list 'load-path "~/.emacs.d/vendor/pymacs-0.24-beta2")

; Load python libs only when needed
(autoload 'python-mode "init_python" "" t)

; nxHTML
(load "~/.emacs.d/vendor/nxhtml/autostart.el")
(setq mumamo-background-colors nil)

;; Mumamo is making emacs 23.3 freak out:
(when (and (equal emacs-major-version 23)
           (equal emacs-minor-version 3))
  (eval-after-load "bytecomp"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-beginning-of-syntax-function))
  ;; tramp-compat.el clobbers this variable!
  (eval-after-load "tramp-compat"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-beginning-of-syntax-function)))

;; Attempts to fix PHP-mode in nXHTML
(autoload 'php-mode "my-php" "PHP Mode." t)
(add-to-list 'auto-mode-alist '("\\.php[34]?\\'\\|\\.phtml\\'" . php-mode))


;; Org-mode customizations
(setq org-agenda-files (list "~/org/todo.org"))
(setq org-link-abbrev-alist
       '(("bugzilla" . "https://bugzilla.mozilla.org/show_bug.cgi?id=")))

(setq org-todo-keywords
	'((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

(setq org-highest-priority ?A)
(setq org-lowest-priority ?E)
(setq org-default-priority ?C)

(setq org-agenda-files (list "~/org/work.org"))

(define-key global-map "\C-ca" 'org-agenda)

(setq org-support-shift-select t)


; Markdown-mode
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

; Tramp, mainly for editing read-only files by sudo-ing
; EX: C-c C-f /sudo::/path/to/file
(require 'tramp)

; js2-mode
(require 'js2-mode)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\(on\\|m\\)?$" . js2-mode))
(define-key js2-mode-map [(return)] 'newline-and-indent)
(setq js2-cleanup-whitespace t)

; Sessions
;(desktop-save-mode 1)

;; use only one desktop
;(setq desktop-path '("~/.emacs.d/"))
;(setq desktop-dirname "~/.emacs.d/")
;(setq desktop-base-file-name "emacs-desktop")
;(setq desktop-save t)

; Windows.el
;(require 'windows)
;(win:startup-with-window)
;(define-key ctl-x-map "C" 'see-you-again)

;(setq win:switch-prefix "\C-cw")
;(define-key global-map win:switch-prefix nil)
;(define-key global-map "\C-cw1" 'win-switch-to-window)

;(setq win:configuration-file "~/.emacs.d/emacs-windows")
;(if (file-exists-p win:configuration-file)
;    (win-load-all-configurations))

; Disable backup files
(setq make-backup-files nil)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed 0.5)
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; C-k should kill the newline as well.
(setq kill-whole-line t)

; Jinja mode
; (require 'jinja)
; (add-to-list 'auto-mode-alist '("\\.html\\'" . jinja-nxhtml-mumamo))

; Edit LESS files as css
(add-to-list 'auto-mode-alist '("\\.less\\'" . css-mode))

; Require newline at end of files
(setq require-final-newline t)

; Balance mode
(autoload 'balance-mode "balance")
(setq auto-mode-alist
      (append '(("\\.bal$" . balance-mode)) auto-mode-alist))
