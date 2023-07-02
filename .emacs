(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; (package-refresh-contents)

(set-frame-font "Menlo-16")
(when (display-graphic-p)
  (setq fonts
        (cond ((eq system-type 'darwin)     '("Monaco" "STHeiti"))
              ((eq system-type 'gnu/linux)  '("Menlo" "WenQuanYi Zen Hei"))
              ((eq system-type 'windows-nt) '("Consolas" "Microsoft Yahei"))))

  (setq face-font-rescale-alist '(("STHeiti" . 1.2) ("Microsoft Yahei" . 1.2) ("WenQuanYi Zen Hei" . 1.2)))
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" (car fonts) 16))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family (car (cdr fonts))))))

(ido-mode 1)

(add-to-list 'load-path "~/.emacs.d/load")
(setq exec-path (cons "/usr/local/bin" exec-path))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(add-hook 'after-init-hook 'global-company-mode)

(require 'cc-mode)
(add-hook 'cc-mode-hook 'eglot-ensure)
(c-set-offset 'inline-open 0)
(c-set-offset 'substatement-open 0)
(c-set-offset 'brace-list-open 0)
(setq c-basic-offset 4)

(defun my-c-mode-common-hook()
  (c-toggle-auto-state 1)
  (c-toggle-auto-hungry-state 1)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; exec-path-from-shell
(load "exec-path-from-shell")
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(set-scroll-bar-mode nil)
(if window-system
    (setq default-frame-alist
          (append
           '( (top . 80)
              (left . 100)
              (width . 110)
              (height . 35))
           default-frame-alist))
  )
(mouse-avoidance-mode 'animate)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;;(setq auto-save-default nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 5)
(setq delete-old-versions t)
(setq display-time-day-and-date t)
(display-time)

(add-to-list 'load-path "~/.emacs.d/load/color-theme")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))
(sublime-text-2)


;;yasnippet
(add-to-list 'load-path
             "~/.emacs.d/load/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(global-set-key [f1] 'multi-term)
(global-set-key [f2] 'compile)
(global-set-key [f12] 'ecb-activate)
(global-set-key [C-f12] 'ecb-deactivate)
(global-set-key [(meta return)] 'semantic-ia-complete-symbol-menu)
(global-set-key [C-.] 'cscope-find-global-definition)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 4)
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(display-time-mode t)
 '(ecb-auto-activate nil)
 '(ecb-options-version "2.32")
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.18)
 '(global-linum-mode t)
 '(package-selected-packages
   '(rust-playground javap-mode rust-mode eglot company cargo-mode))
 '(tool-bar-mode nil))

;;Python Config
(require 'python)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))

(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:ac-setup)


;;Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;;haml
(require 'haml-mode)
(add-hook 'haml-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (define-key haml-mode-map "\C-m" 'newline-and-indent)))


;;coffee
(require 'coffee-mode)

(setq whitespace-action '(auto-cleanup))
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))


;;Go
(add-to-list 'exec-path "/usr/local/go/bin/")
(require 'go-mode-autoloads)
;;(add-hook 'before-save-hook 'gofmt-before-save)

;;Yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; term
(require 'multi-term)
(setq multi-term-program "/usr/local/bin/fish")

;; protobuf
(require 'protobuf-mode)
(add-to-list 'auto-mode-alist
             '("\\.proto$" . protobuf-mode))

(defconst my-protobuf-style
  '((c-basic-offset . 4)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "my-style" my-protobuf-style t)))

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 4)
(setq web-mode-code-indent-offset 4)

(setq web-mode-content-types-alist
      '(("jsx" . ".*\\.js\\'")))

;; cql-mode
(require 'cql-mode)

;; cql-mode
(require 'sql-indent)
(add-hook 'sql-mode-hook 'sqlind-minor-mode)

;; Update indentation rules, select, insert, delete and update keywords
;; are aligned with the clause start

(defvar my-sql-indentation-offsets-alist
  `((select-clause 0)
    (insert-clause 0)
    (delete-clause 0)
    (update-clause 0)
    ,@sqlind-default-indentation-offsets-alist))

(add-hook 'sqlind-minor-mode-hook
          (lambda ()
            (setq sqlind-indentation-offsets-alist
                  my-sql-indentation-offsets-alist)))


;; hcl-mode
(require 'hcl-mode)

;; rust-mode
(require 'cargo-mode)
(require 'rust-mode)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))
(add-hook 'eglot--managed-mode-hook (lambda () (eldoc-mode -1)))

;; lua-mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


;; format-all
(require 'format-all)
(add-hook 'before-save-hook 'format-all-buffer)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
