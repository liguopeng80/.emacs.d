;;; init.guli.el --- Where all the magic begins
;;
;; This file loads Org-mode and then loads the rest of our Emacs initialization from Emacs lisp
;; embedded in literate Org-mode files.

;; Load up Org Mode and (now included) Org Babel for elisp embedded in Org Mode files

(setq emacs-load-start-time (current-time))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))
(setq *win32* (eq system-type 'windows-nt) )
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(setq *linux-x* (and window-system *linux*) )
(setq *xemacs* (featurep 'xemacs) )
(setq *emacs23* (and (not *xemacs*) (or (>= emacs-major-version 23))) )
(setq *emacs24* (and (not *xemacs*) (or (>= emacs-major-version 24))) )

(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

;; load org files
(let* ((org-dir (expand-file-name
                 "lisp" (expand-file-name
                         "org" (expand-file-name
                                "src" dotfiles-dir))))
       (org-contrib-dir (expand-file-name
                         "lisp" (expand-file-name
                                 "contrib" (expand-file-name
                                            ".." org-dir))))
       (load-path (append (list org-dir org-contrib-dir)
                          (or load-path nil))))
  ;; load up Org-mode and Org-babel
  (require 'org-install)
  (require 'ob-tangle)
  )

;; load up all literate org-mode files in this directory, for example:
(setq user-customize-org-file
     (concat "~/.emacs.d/" (or user-login-name "") "-dotemacs.org"))
(org-babel-load-file user-customize-org-file)
;; (mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))

(setq-default initial-scratch-message
              (concat ";; Happy hacking " (or user-login-name "") "!\n\n"
                      ";; 积极主动；以使为终；要事第一；\n;; 双赢思维；知彼知己；综合踪效\n\n"
                      ";; 士不可以不弘毅，任重而道远。\n"
                      ";; 弘，宽广也。毅，强忍也。非弘不能胜其重，非毅无以致其远。\n"
                      ";; 仁以为己任，不亦重乎？死而后已，不亦远乎？\n\n"
                      ))

;;; init.el ends here
