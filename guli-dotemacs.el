
(setq user-full-name "Guo Peng Li"
      user-mail-address "liguopeng@liguopeng.net")

;; (add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'use-package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq delete-old-versions -1)
(setq version-control t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)

(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(setq make-backup-files t)

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode t)
  (scroll-bar-mode -1))

(setq winner-dont-bind-my-keys t)

(use-package winner
  :ensure winner
  :init (winner-mode 1))
(winner-mode t)

(global-set-key (kbd "C-x w u") 'winner-undo)
(global-set-key (kbd "C-x w r") 'winner-redo)

(setq sentence-end "([。！？]|……|[.?!][]\"')}]*($|[ \t]))[ \t\n]*")
(setq sentence-end-double-space nil)

(defun guli/indent-or-complete ()
  (interactive)
  (if (looking-at "\\>")
      (hippie-expand nil)
    (indent-for-tab-command))
  )

(setq default-tab-width 4)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;; not supported by on emacs 23
(setq-default line-spacing 2)

(fset 'yes-or-no-p 'y-or-n-p)

;; obsoleted in 24.1
;; (partial-completion-mode 1) 

(icomplete-mode 1)

(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

(show-paren-mode t)
(setq show-paren-style 'parentheses)

(prefer-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix) 
(setq default-buffer-file-coding-system 'utf-8-unix)

(ansi-color-for-comint-mode-on)

;; highlight the region
(transient-mark-mode t)

(defun lgp-mark-char()
  "Set mark at current point and move cursor to next char."
  (interactive)
  (push-mark (point))
  (forward-char))

(defun lgp-mark-word()
  "Set mark at current point and move cursor to the end of the word."
  (interactive)
  (push-mark (point))
  (forward-word))

(defun lgp-mark-sentence()
  "Set mark at current point and move cursor to the end of the sentence."
  (interactive)
  (push-mark (point))
  (forward-sentence))

(global-set-key [(control shift f)] 'lgp-mark-char)
(global-set-key [(meta shift f)] 'lgp-mark-word)
(global-set-key [(meta shift e)] 'lgp-mark-sentence)

(global-set-key [(f9)] 'list-bookmarks)
(global-set-key [(f2)] 'set-mark-command)

;; functions for temp bookmarks
;; C-. set a bookmark 
;; C-, jump to previous bookmark
(global-set-key (kbd "C-.") 'guli/point-to-register)
(global-set-key (kbd "C-,") 'guli/jump-to-register)

(defun guli/point-to-register()
  "Store cursorposition _fast_ in a register. 
Use my-jump-to-register to jump back to the stored 
position."
  (interactive)
  (setq zmacs-region-stays t)
  (point-to-register 8))

(defun guli/jump-to-register()
  "Switches between current cursorposition and position
that was stored with my-point-to-register."
  (interactive)
  (setq zmacs-region-stays t)
  (let ((tmp (point-marker)))
        (jump-to-register 8)
        (set-register 8 tmp)))

(hl-line-mode 1)
(setq global-hl-line-mode t)

(require 'ido)
(ido-mode t)

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'text-mode-hook 
         '(lambda()
            (setq indent-tabs-mode nil)))

;; (require 'tramp)
;; (setq tramp-default-method "plink"
;;       tramp-password-end-of-line "\r\n"
;; ;      tramp-default-user "root"      
;;    tramp-default-host "59.151.15.39")

;; (add-to-list
;;  'tramp-multi-connection-function-alist
;;  '("gateway-andover" tramp-multi-connect-rlogin "plink -ssh -A -l %u %h %n"))

(global-set-key [f1]    'help)
(global-set-key [f2]    'org-insert-todo-heading)
(global-set-key [f6]    'shell)
(global-set-key [f7]    'text-mode)
(global-set-key [f8]    'outline-mode)

(define-key global-map [(f5)] 'revert-buffer)
(global-set-key [f7] 'calendar)
(global-set-key [f8] 'other-window)
(global-set-key [f9] 'view-mode)
(global-set-key [f11] 'compile)
(global-set-key [f12] 'gdb)
(global-set-key (kbd "C-c C-o") 'occur)

(global-set-key [(meta f12)] 'speedbar)
(global-set-key [(f1)] 'delete-other-windows)

(require 'template)
(template-initialize)
(setq template-default-directories (cons "~/.emacs.d/templates" template-default-directories))

;; (require 'color-theme)
;; (color-theme-initialize)
;; (setq color-theme-is-global t)

;; (load-library "color-theme-library")

;; (color-theme-robin-hood)
;; great for html generation
;; (color-theme-pierson) 
;; (color-theme-gnome2)
;; (color-theme-gray30)
;; (color-theme-comidia)
;; (color-theme-gray1)
;; (color-theme-oswald)

;;(require 'zenburn)
;; (color-theme-zenburn)
;; (color-theme-gray30)

(setq scroll-margin 5
      scroll-conservatively 10000)

;; increase the sroll speed of large files
(setq lazy-lock-defer-on-scrolling t)

(defun guli/hold-line-scroll-up()
  "Scroll the page with the cursor in the same line"
  (interactive)
  (scroll-up 1))

(defun guli/hold-line-scroll-down()
  "Scroll the page with the cursor in the same line"
  (interactive)
  (scroll-down 1))

(global-set-key (kbd "M-n") 'guli/hold-line-scroll-up)
(global-set-key (kbd "M-p") 'guli/hold-line-scroll-down)

(defun guli/current-date()
  (interactive)
  (shell-command "date '+%Y-%m-%d'"))

(defun guli/insert-current-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d" (current-time))))

(defun guli/insert-current-time ()
  (interactive)
 (insert (format-time-string "%H:%M " (current-time))))

(define-key global-map [(meta f11)] 'guli/insert-current-date)
(define-key global-map [(meta f12)] 'guli/insert-current-time)

(defun guli/insert-date ()
  "Insert date-time at cursor."
  (interactive)
  ;; (insert (format-time-string "%Y/%m/%d %H:%M:%S" (current-time)))
  ;; (insert (format-time-string "%Y/%m/%d" (current-time)))
  (insert (format-time-string "%Y/%m/%d w%W %a" (current-time)))
  )

(global-set-key (kbd "C-c m d") 'guli/insert-date)

(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

(setq custom-file "~/.emacs.d/lisp/guli-custom.el")

(put 'narrow-to-region 'disabled nil)

(setq visible-bell nil)
(setq default-fill-column 78)
(setq inhibit-startup-message t)
(setq line-number-mode t)
(setq column-number-mode t)

(setq kill-whole-line t)

(setq require-final-newline t)

(setq default-major-mode 'text-mode)

(auto-image-file-mode)

(mouse-avoidance-mode 'animate)

(put 'upcase-region 'disabled nil)

(global-set-key (kbd "RET") 'newline-and-indent)

(defun guli/goto-char (n char)
  "Move forward to Nth occurence of CHAR.
Typing `guli/goto-char-key' again will move forwad to the next Nth
occurence of CHAR."
  
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char) char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))

(define-key global-map (kbd "C-z") 'guli/goto-char)

(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)

(use-package windmove
             :bind
             (("C-c <right>" . windmove-right)
              ("C-c <left>" . windmove-left)
              ("C-c <up>" . windmove-up)
              ("C-c <down>" . windmove-down)))

(defun guli/vsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (unless prefix
    (switch-to-next-buffer)))
(defun guli/hsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (unless prefix (switch-to-next-buffer)))
(bind-key "C-x 2" 'guli/vsplit-last-buffer)
(bind-key "C-x 3" 'guli/hsplit-last-buffer)

(defun guli/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'guli/smarter-move-beginning-of-line)

(add-to-list 'load-path "~/elisp/recursive-narrow")
(defun guli/recursive-narrow-dwim-org ()
    (if (derived-mode-p 'org-mode) 
         (cond ((or (org-at-block-p) (org-in-src-block-p)) (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
)
(use-package recursive-narrow
  :config
  (add-hook 'recursive-narrow-dwim-functions 'guli/recursive-narrow-dwim-org)
  :bind
  (("C-x n w" . recursive-widen)
   ("C-x n n" . recursive-narrow-or-widen-dwim)))

(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(windmove-default-keybindings)

(require 'window-numbering)
(window-numbering-mode 1)

(defun guli/kill-buffer-and-window()
  (interactive)
  (kill-buffer-and-window))

(defun guli/kill-buffer()
  (interactive)
  (ido-kill-buffer))

(global-set-key [C-f4] 'guli/kill-buffer-and-window)
(global-set-key [C-f3] 'guli/kill-buffer)

; (global-set-key [(control tab)] 'next-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (load-file "~/.emacs.d/emacs-for-python/epy-init.el")

;; (add-to-list 'load-path "~/.emacs.d/emacs-for-python/") ;; tell where to load the various files
;; (require 'epy-setup)      ;; It will setup other loads, it is required!
;; (require 'epy-python)     ;; If you want the python facilities [optional]
;; (require 'epy-completion) ;; If you want the autocompletion settings [optional]
;; (require 'epy-editing)    ;; For configurations related to editing [optional]
;; (require 'epy-bindings)   ;; For my suggested keybindings [optional]
;; (require 'epy-nose)       ;; For nose integration

; python auto indent
(add-hook 'python-mode-hook
      '(lambda()
         (local-set-key
          "\r"
          '(lambda()
         (interactive)
         (insert "\n")
         (python-indent-line)))))

; indent python code with spaces
(add-hook 'python-mode-hook
          '(lambda()
             (setq indent-tabs-mode nil)))

;; (setq epy-enable-ropemacs nil)

;; using c++-mode on .h files
(setq auto-mode-alist (cons '("\\.hpp$" . c++-mode) auto-mode-alist))

;; ;; load c++ config file (c++ snipe)
;; (load-file (concat lgp-path-my-config "cpp/config.el"))

;; (autoload 'gtags-mode "gtags" "" t)

(defun my-c++-mode-hook()
  ;; forward by work
  (local-set-key [(meta f)] 'c-forward-into-nomenclature)
  (local-set-key [(meta b)] 'c-backward-into-nomenclature)

  (setq tab-width 4 indent-tabs-mode nil)
  (local-set-key (kbd "^M") 'newline-and-indent)
  (c-set-style lgp-default-c++-style)
  ;; (define-key c-mode-base-map [(control shift`)] 'c-indent-command)

  ;; using hs-minor-mode
  (hs-minor-mode)
  (define-key c-mode-base-map [(control \\)] 'hs-toggle-hiding)
  (define-key c-mode-base-map [(meta \\)] 'hs-show-block)
  
  ;; using company-mode
  ;; (company-mode t)

  ;; abbrev mode
  (setq abbrev-mode t)

  ;; cedet
  ;; (ede-minor-mode t) 

  ;; (gtags-mode)

  ;; (semantic-load-enable-gaudy-code-helpers)
  ;; (semantic-load-enable-code-helpers)
  ;; (semantic-load-enable-minimum-features)

  ;; (define-key c-mode-base-map [(tab)] 'guli/indent-or-complete)
  ;; (local-set-key [(control return)] 'semantic-complete-analyze-inline)
  ;; (define-key c-mode-base-map [(meta ?/)] 'semantic-ia-complete-symbol-menu)
  ;; (local-set-key (kbd "M-/") 'semantic-complete-analyze-inline)
  ;; (local-set-key "." 'semantic-complete-self-insert)
  ;; (local-set-key ">" 'semantic-complete-self-insert) 
  
  ;; (c-toggle-auto-newline t) 
  ;; (c-hungry-delete-forward)
  (c-toggle-hungry-state t)
  )

(add-hook 'c++-mode-hook 'my-c++-mode-hook) 
(add-hook 'c-mode-common-hook 'my-c++-mode-hook)

(require 'xcscope)


(setq cscope-program "/usr/local/bin/cscope")

(define-key global-map [(control f3)]  'cscope-set-initial-directory)
(define-key global-map [(control meta f4)]  'lgp-switch-cscope-update-db-status)
(define-key global-map [(control f5)]  'cscope-find-this-symbol)
(define-key global-map [(control f6)]  'cscope-find-global-definition)
(define-key global-map [(control f7)]  'cscope-find-global-definition-no-prompting)
(define-key global-map [(control f8)]  'cscope-pop-mark)
(define-key global-map [(control f9)]  'cscope-next-symbol)
(define-key global-map [(control f10)] 'cscope-prev-symbol)
(define-key global-map [(control f11)] 'cscope-next-file)
(define-key global-map [(control f12)] 'cscope-prev-file)
(define-key global-map [(meta f9)]     'cscope-display-buffer)
(define-key global-map [(meta f10)]    'cscope-display-buffer-toggle)

(defun lgp-switch-cscope-update-db-status()
  (interactive)
  (if cscope-do-not-update-database
      (setq cscope-do-not-update-database nil)
    (setq cscope-do-not-update-database t)))

;; NOT update database automatically
(setq cscope-do-not-update-database t)

;; ;; doxymacs configure
(require 'doxymacs)
(add-hook 'c-mode-common-hook 'doxymacs-mode)

(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))

;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;; default C\C++ coding style 
;; (setq c-default-style '((c-mode . "stroustrup")))
;; (setq lgp-default-c++-style "symbian")
(setq lgp-default-c++-style "stroustrup")

;; (require 'symbian-c++-mode)

(require 'cc-mode)

;; (require 'company-mode)
;; (require 'company-bundled-completions)
;; (company-install-bundled-completions-rules)

;; (require 'erlang)
;; (setq auto-mode-alist (cons '("\\.erl$" . erlang-mode) auto-mode-alist))

;; (setq load-path (cons  "/usr/lib/erlang/lib/tools-2.6.13/emacs" load-path))
;; (setq erlang-root-dir "/usr/lib/erlang/")
;; (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))

;; (require 'erlang-start)

(require 'git-emacs)

;; (enable-visual-studio-bookmarks)

(global-font-lock-mode 1)
;; (setq font-lock-maximum-decoration t)

;; (set-default-font "DejaVu Sans Mono")
(when window-system
;;  (set-frame-font "Symbol-12")
  (set-fontset-font (frame-parameter nil 'font)
                    'han '("WenQuanYi Micro Hei" . "ISO10646-1"))
  (set-fontset-font (frame-parameter nil 'font)
                    'symbol '("WenQuanYi Micro Hei" . "ISO10646-1"))
  (set-fontset-font (frame-parameter nil 'font)
                    'cjk-misc '("WenQuanYi Micro Hei" . "ISO10646-1"))
  (set-fontset-font (frame-parameter nil 'font)
                    'bopomofo '("WenQuanYi Micro Hei" . "ISO10646-1")))

(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; (load "desktop")
;; (desktop-load-default)
;; (desktop-read);; (desktop-save-mode 1)

;; (require 'tabbar)
;; (tabbar-mode 1)
;; (setq tabbar-buffer-groups-function
;;        (lambda (buffer) (list "All buffers")))

;; (global-set-key [C-M-left] 'tabbar-backward-group)
;; (global-set-key [C-M-right] 'tabbar-forward-group)
;; (global-set-key [C-left] 'tabbar-backward-tab)
;; (global-set-key [C-right] 'tabbar-forward-tab)
;; (global-set-key [C-tab] 'tabbar-forward-tab)

;; ;; ignore some special buffers (don't display them in tabs)
;; (setq tabbar-buffer-groups-function 'tabbar-buffer-ignore-groups)

;; (defun tabbar-buffer-ignore-groups (buffer)
;;   "Return the *LIST OF GROUP NAMES* buffer belongs to."
;;   (with-current-buffer (get-buffer buffer)
;;     (cond
;;      ((eq major-mode 'dired-mode)
;;       '("Dired")                        ; directories
;;       )
;;      ((memq major-mode
;;             '(help-mode apropos-mode Info-mode Man-mode))
;;       '("Help")                         ; help informations
;;       )
;;      ((memq major-mode
;;             '(org-mode muse-mode))
;;       '("Notes")
;;       )
;;      ((memq major-mode
;;             '(conf-mode))
;;       '("Config")
;;       )
;;      ((not (string= "*" (substring (buffer-name) 0 1)))
;;       '("default")                      ; all except emacs buffers
;;       )
;; ;;      (t
;; ;;       '("default")
;; ;;        (list 
;; ;;        "default"  ;; no-grouping
;; ;;        (if (and (stringp mode-name) (string-match "[^ ]" mode-name))
;; ;;            mode-name
;; ;;          (symbol-name major-mode)))
;; ;;       )
;;      )))

;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(tabbar-selected-face ((t (:inherit tabbar-default-face :background "#102e4e" :foreground "green" :box (:line-width 2 :color "#102e4e" :style released-button)))))
;;  '(tabbar-unselected-face ((t (:inherit tabbar-default-face :foreground "#102e4e" :box (:line-width 2 :color "white" :style pressed-button))))))

(autoload 'folding-mode "folding" 
  "Minor mode that simulates a folding editor" t)

(require 'org-install)

;(setq org-fontify-done-headline t)
(setq org-hide-emphasis-markers t)
;(setq org-hide-leading-stars t)
;(setq org-reverse-note-order t)
;(setq org-tags-column -120)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(global-set-key "\C-cl" 'org-store-link)   ; store line
(global-set-key "\C-ca" 'org-agenda)       ; agenda view
(global-set-key "\C-cb" 'org-iswitchb)     ; switch to org buffer

(add-hook 'org-mode-hook 'turn-on-font-lock)

(define-key mode-specific-map [\C-ca] 'org-agenda)

;; show todo-list defined in current file
(defun guli/task-list()
  (interactive)
  (occur "TODO"))

(setq org-directory "~/private/org/")

(setq org-default-notes-file "~/private/org/notes.org")

;; show which events should be listed in agenda view
(setq org-agenda-files
      (list "~/private/org/todo/todo-work.org"
            "~/private/org/todo/todo-personal.org"
            ))

(eval-after-load "org"
  ;; '(progn
  '(progn
     (define-prefix-command 'org-todo-state-map)
     (define-key org-mode-map "\C-cx" 'org-todo-state-map)
     (define-key org-todo-state-map "x"
       #'(lambda nil (interactive) (org-todo "CANCELLED")))
     (define-key org-todo-state-map "d"
       #'(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "f"
       #'(lambda nil (interactive) (org-todo "DEFERRED")))
     (define-key org-todo-state-map "l"
       #'(lambda nil (interactive) (org-todo "DELEGATED")))
     (define-key org-todo-state-map "s"
       #'(lambda nil (interactive) (org-todo "STARTED")))
     (define-key org-todo-state-map "w"
       #'(lambda nil (interactive) (org-todo "WAITING")))
     ))

(setq org-clock-persist t)
(org-clock-persistence-insinuate)

(setq org-emphasis-alist (quote (("*" org-bold "<b>" "</b>")
                            ("/" italic "<i>" "</i>")
                            ("&" highlight "<font color=\"red\">" "</font>")
                            ("_" underline "<u>" "</u>")
                            ("=" org-code "<code>" "</code>" verbatim)
                            ("~" org-verbatim "" "" verbatim)
                            ("+" format-font-lock-strikethru-face "<del>" "</del>"))))

(setq org-fast-tag-selection-single-key (quote expert))

;; when task done
(setq org-log-done 'time)
;(setq org-log-done 'note)

(setq org-agenda-custom-commands
      '(("w" "Weekly Plan"
         ( (agenda)
           (todo "TODO" "STARTED")
           )
         )
        ("u" todo "WORK&URGENT" nil)
        ("c" todo "WORK&@PHONE" nil)
        ("h" todo "PERSONAL-@ERRANDS" nil)

        ("p" "Personal Plan" tags-todo ""
         ((org-agenda-files (file-expand-wildcards "~/private/org/todo/todo*.org"))
          (agenda)
          (tags-todo "")
          )
         )

        ("m" tags "WORK" nil)
        ("a" "My agenda"
         ((org-agenda-list)
          (tags-todo "URGENT")
          (tags "PROJECT-MAYBE-DONE")))

        ("H" "Home NA Lists"
         ( (agenda)
           (tags-todo "HOME")
           (tags-todo "COMPUTER")
           )
         )

        ;; ... put your other custom commands here
        ))

(setq org-publish-project-alist
      '(("org"
         :base-directory "~/private/org/"
         :publishing-directory "~/private/org/html"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :table-of-contents nil
         :inline-images t
         :recursive t
;;       :style "<link rel=\"stylesheet\"
;;                      href=\"../other/mystyle.css\"
;;                      type=\"text/css\">"
         :style "<link rel=\"stylesheet\" href=\"org.css\" type=\"text/css\">"
         )
        ("css"
         :base-directory "~/private/org/html/css"
         :base-extension "css")
        ("img"
         :base-directory "~/private/org/html/img"
         :base-extension "jpg|png|gif")
        ))

(require 'org-capture)
(global-set-key "\C-cr" 'org-capture)

(setq org-capture-templates
      '(
        ;; todo list, GTD
        ("t" "Todo (work)" entry (file+datetree "~/private/org/todo/todo-work.org")  "* TODO %^{topic} :work:%^g\nAdded: %U\n\n%?\n" :empty-lines 1)
        ("p" "Todo (Personal)" entry (file+datetree "~/private/org/todo/todo-personal.org")  "* TODO %^{topic} :personal:%^g\nAdded: %U\n\n%?\n" :empty-lines 1)

        ;; Reviews and Journanl
        ("r" "Daily Review" entry (file+datetree "~/private/org/notes/review.org") "Daily Report :review:"
         "* daily review %U :Daily: \n%[~/.emacs.d/conf/org/dailyreport.txt]\n"  :prepend t :empty-lines 1)
        ("j" "Journal" entry (file+datetree "~/private/org/notes/journal.org")  "* %U - %^{Title} :journal:\n %i\n %a" :empty-lines 1)
        ("l" "Log Time" entry (file+datetree "~/private/org/notes/timelog.org")  "* %U - %^{Title} :time:")

        ;; notes for study, management, ideas
        ("n" "Notes" entry (file+headline "~/private/org/notes/notes.org" "Notes")  "* %^{topic} %u %^g\n%?\n" :prepend t :empty-lines 1)
        ("m" "Management" entry (file+headline "~/private/org/notes/notes.management.org" "Management")  "* %^{topic} %u :manage:%^g\n%?\n" :prepend t :empty-lines 1)
        ))

(setq org-fontify-emphasized-text t)

(defun org-dblock-write:image (params)
  (let ((file (plist-get params :file)))
    (clear-image-cache file)
    (insert image (create-image file) )))

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "STARTED(s!)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(x)" "DEFERRED(f)")
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "|" "CANCELLED(x)")
        (sequence "QUOTE(q!)" "QUOTED(Q!)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")))

(setq org-todo-keyword-faces
      '(("TODO"      . org-warning)
        ("DEFERRED"  . shadow)
        ("CANCELLED"  . (:foreground "green"))))

(add-hook 'org-finalize-agenda-hook
          (lambda ()
            (save-excursion
              (color-org-header "personal:"  "green")
              (color-org-header "birthdays:" "gold")
              (color-org-header "work:"      "orange"))))

(setq general-holidays
      '((holiday-fixed 1 1 "元旦")
        (holiday-fixed 2 14 "情人节")
        (holiday-fixed 3 14 "白色情人节")
        (holiday-fixed 4 1 "愚人节")
        (holiday-fixed 5 1 "劳动节")
        (holiday-float 5 0 2 "母亲节")
        (holiday-fixed 6 1 "儿童节")
        (holiday-float 6 0 3 "父亲节")
        (holiday-fixed 7 1 "建党节")
        (holiday-fixed 8 1 "建军节")
        (holiday-fixed 9 10 "教师节")
        (holiday-fixed 10 1 "国庆节")
        (holiday-fixed 12 25 "圣诞节")
        ;; 农历节日
        (holiday-chinese 1 1 "春节")
        (holiday-chinese 1 2 "春节")
        (holiday-chinese 1 3 "春节")
        (holiday-chinese-qingming)
        (holiday-chinese 5 5 "端午节")
        (holiday-chinese 8 15 "中秋节")
        ))

(require 'org)

;; display english week name in timestamp
;; put this at the end of org-init.el
(setq system-time-locale "C")
(setq system-time-locale "en_US.utf8")
(setq org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%m/%d/%y>" . "<%m/%d/%y %a %H:%M>"))

(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t) 
(setq auto-mode-alist (cons '("\\.text" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

(defun create-shell ()
  "creates a shell with a given name"
  (interactive);; "Prompt\n shell name:")
  (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*shell-" shell-name "*"))))

(defun clear-shell ()                                                                                          
  (interactive)
  (let ((comint-buffer-maximum-size 0))                                                                        
    (comint-truncate-buffer)))

;; kill "Completions" buffer
(add-hook 'minibuffer-exit-hook 
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))

(defun cygwin-shell ()
  "Run cygwin bash in shell mode."
  (interactive)
  (let ((explicit-shell-file-name "C:/bin/cygwin64/bin/bash"))
    (call-interactively 'shell)))

(when window-system
  (setq explicit-shell-file-name "C:/bin/cygwin64/bin/bash")
  (setq explicit-sh-args '("-login" "-i"))
  )

(use-package erc
  :ensure erc
  :config
  (setq erc-autojoin-channels-alist '(("freenode.net"
                   "#org-mode"
                   "#hacklabto"
                   "#emacs"))
    erc-server "irc.freenode.net"
    erc-nick "guli"))

(use-package ace-jump-mode
  :ensure ace-jump-mode)
;; I use the jj key-chord for this; see the definitions for key-chord

(use-package ace-window
  :ensure ace-window
  :config (setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  :bind ("C-x o" . ace-window))

(use-package ace-jump-zap
  :ensure ace-jump-zap
  :bind
  (("M-z" . ace-jump-zap-up-to-char-dwim)
   ("C-M-z" . ace-jump-zap-to-char-dwim)))

(use-package smartparens
  :ensure smartparens
  :diminish smartparens
  :config
  (progn
    (require 'smartparens-config)
    (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
    (add-hook 'emacs-lisp-mode-hook 'show-smartparens-mode)

;;;;;;;;;;;;;;;;;;;;;;;;
    ;; keybinding management

    (define-key sp-keymap (kbd "C-c s r n") 'sp-narrow-to-sexp)
    (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
    (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
    (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
    (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
    (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
    (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

    (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
    (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
    (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
    (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

    (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
    (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

    (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
    (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

    (define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
    (define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

    (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
    (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
    (define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
    (define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

    (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
    (define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
    (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
    (define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

    (define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
    (define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
    (define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

    (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
    (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

    (define-key sp-keymap (kbd "C-c s t") 'sp-prefix-tag-object)
    (define-key sp-keymap (kbd "C-c s p") 'sp-prefix-pair-object)
    (define-key sp-keymap (kbd "C-c s c") 'sp-convolute-sexp)
    (define-key sp-keymap (kbd "C-c s a") 'sp-absorb-sexp)
    (define-key sp-keymap (kbd "C-c s e") 'sp-emit-sexp)
    (define-key sp-keymap (kbd "C-c s p") 'sp-add-to-previous-sexp)
    (define-key sp-keymap (kbd "C-c s n") 'sp-add-to-next-sexp)
    (define-key sp-keymap (kbd "C-c s j") 'sp-join-sexp)
    (define-key sp-keymap (kbd "C-c s s") 'sp-split-sexp)

;;;;;;;;;;;;;;;;;;
    ;; pair management

    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    (sp-local-pair 'web-mode "<" nil :when '(guli/sp-web-mode-is-code-context))

;;; markdown-mode
    (sp-with-modes '(markdown-mode gfm-mode rst-mode)
      (sp-local-pair "*" "*" :bind "C-*")
      (sp-local-tag "2" "**" "**")
      (sp-local-tag "s" "```scheme" "```")
      (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;;; tex-mode latex-mode
    (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
      (sp-local-tag "i" "1d5f8e69396c521f645375107197ea4dfbc7b792quot;<" "1d5f8e69396c521f645375107197ea4dfbc7b792quot;>"))

;;; html-mode
    (sp-with-modes '(html-mode sgml-mode web-mode)
      (sp-local-pair "<" ">"))

;;; lisp modes
    (sp-with-modes sp--lisp-modes
      (sp-local-pair "(" nil :bind "C-("))))

;; show agenda
(add-hook 'after-init-hook (lambda () (org-agenda nil "a")))

(setenv "PATH" (concat "\"c:/program files/postgresql/9.3/bin;\"" (getenv "PATH")))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

(if (equal system-type `darwin)    
    (setenv "PATH" (concat (getenv "PATH")
                           ":/usr/local/Cellar/cscope/15.8a/bin")))
