(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-priority-faces
   (quote
    ((65 . "DeepPink")
     (66 . "firebrick")
     (67 . "white"))))
 '(package-selected-packages
   (quote
    (rust-mode yasnippet yaml-mode xcscope writeroom-mode wgrep w3m use-package unfill treemacs textile-mode tagedit switch-window string-edit sr-speedbar solidity-mode smartparens session scss-mode scratch sass-mode rvm robe rinari regex-tool rainbow-delimiters quack project-local-variables pretty-mode pomodoro pointback paredit page-break-lines org-fstree org mwe-log-commands multiple-cursors multi-term move-text mic-paren maxframe markdown-mode magit lua-mode less-css-mode legalese json-mode js2-mode iedit idomenu ibuffer-vc htmlize hl-sexp haskell-mode gitignore-mode gitconfig-mode git-timemachine git-gutter ggtags fringe-helper flyspell-lazy flymake-sass flymake-ruby flymake-python-pyflakes flymake-lua flymake-jslint flymake-css flymake-coffee fancy-narrow expand-region exec-path-from-shell erlang emmet-mode elnode dsvn dropdown-list dired-details dired+ csharp-mode crontab-mode cpputils-cmake company-c-headers company-anaconda color-theme coffee-mode buffer-move bbdb auto-compile ag ace-jump-zap)))
 '(safe-local-variable-values
   (quote
    ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook
            (quote write-contents-functions)
            (lambda nil
              (delete-trailing-whitespace)
              nil))
           (require
            (quote whitespace))
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face trailing lines-tail)
     (require-final-newline . t))))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-highlight-prompt ((t (:foreground "light sky blue"))))
 '(font-lock-comment-face ((t (:foreground "pale green" :slant italic))))
 '(org-level-1 ((t (:foreground "forest green"))))
 '(window-numbering-face ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold))) t))
