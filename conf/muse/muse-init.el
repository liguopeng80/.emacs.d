;;; muse-init.el --- Initialize muse-mode.

;;;; Add below to ~/.emacs config file
;(load-file "~/notes/conf/muse-init.el")

;; muse
(require 'htmlize)
(require 'muse)
(require 'muse-colors)
(require 'muse-mode)     ; load authoring mode
(require 'muse-html)     ; load publishing styles I use
(require 'muse-wiki)
(require 'muse-xml)
(require 'muse-project)  ; publish files in projects

;; 允许不使用 muse 扩展名
(add-to-list 'auto-mode-alist
			 '("#title " . muse-mode))

;; (add-hook 'text-mode-hook
;;           (lambda ()
;;             (unless (eq major-mode 'muse-mode)
;;               (when (equal (file-name-extension buffer-file-truename) "txt")
;;                 (save-excursion
;;                   (goto-line 5)
;;                   (if (re-search-backward "\* [A-Z][a-z]+.*" 1 t)
;;                       (muse-mode)))))))

;; 设置输出时的中文编码
(setq muse-html-meta-content-type "text/html; charset=utf-8")
;(setq muse-html-meta-charset "gb2312")
(setq muse-html-content-coding "utf-8")
(setq muse-html-charset-default "utf-8")
(setq muse-html-coding-default "utf-8")

;; 增加自定义的发布类型
(unless (assoc "my-blosxom" muse-publishing-styles)
  (muse-derive-style "notes-html" "html"
                     :header "~/conf/muse/header.html"
                     :footer "~/conf/muse/footer.html"
					 ))

;; 设置项目属性
(setq muse-project-alist
	  '(("MyNotes" ("~/notes" :default "index")
		 (:base "notes-html" :path "~/notes/publish") ) ; MyNotes
		))

;; 使用默认的 TAB 键
(define-key muse-mode-map [tab] 'indent-for-tab-command)


;; 代码高亮
;; (require 'muse-colors)
;; (add-to-list 'muse-colors-tags
;;              '("src" t t t muse-colors-example-tag))

;; ;; add a <source> tag, which use source-highlight
;; (defun muse-publish-source-tag (beg end attrs)
;;   (let ((lang (cdr (assoc "lang" attrs)))
;;         (style (muse-style-element :base))
;;         hcodes)
;;     (if (string-equal style "pdf") (setq style "latex"))
;;     (if lang
;;         (progn
;;           (muse-publish-command-tag
;;            beg end
;;            (cons (cons "interp" 
;; 					   (format "echo \"<literal><pre class=\"example\"> 
;; $(source-highlight -s %s -f %s) 
;; </pre></literal>\"" lang style))
;; 				 attrs))
;;           (muse-publish-mark-read-only beg (point)))
;;       (error "No lang attribute specified in <source> tag"))))

;; (add-to-list
;;  'muse-publish-markup-tags '("source" t t nil muse-publish-source-tag))

;; 修改 outline-minor-mode 的绑定前缀

