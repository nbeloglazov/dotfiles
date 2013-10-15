;disable backup
(setq backup-inhibited t)
(setq inhibit-startup-message t)
;disable auto save
(setq auto-save-default nil)
(global-linum-mode 1)
(menu-bar-mode -1)

(setq-default show-trailing-whitespace t)
(setq-default default-indicate-empty-lines t)
(setq-default indent-tabs-mode nil)
(setq vc-follow-symlinks t)


; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; I prefer using the "clipboard" selection (the one the
;; typically is used by c-c/c-v) before the primary selection
;; (that uses mouse-select/middle-button-click)
(setq x-select-enable-clipboard t)

;; If emacs is run in a terminal, the clipboard- functions have no
;; effect. Instead, we use of xsel, see
;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
;; program for getting and setting the contents of the X selection"
(unless window-system
 (when (getenv "DISPLAY")
  ;; Callback for when user cuts
  (defun xsel-cut-function (text &optional push)
    ;; Insert text to temp-buffer, and "send" content to xsel stdin
    (with-temp-buffer
      (insert text)
      ;; I prefer using the "clipboard" selection (the one the
      ;; typically is used by c-c/c-v) before the primary selection
      ;; (that uses mouse-select/middle-button-click)
      (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
  ;; Call back for when user pastes
  (defun xsel-paste-function()
    ;; Find out what is current selection by xsel. If it is different
    ;; from the top of the kill-ring (car kill-ring), then return
    ;; it. Else, nil is returned, so whatever is in the top of the
    ;; kill-ring will be used.
    (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
      (unless (string= (car kill-ring) xsel-output)
        xsel-output )))
  ;; Attach callbacks to hooks
  (setq interprogram-cut-function 'xsel-cut-function)
  (setq interprogram-paste-function 'xsel-paste-function)
  ;; Idea from
  ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
  ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
 ))

(defun add-path (name)
  (add-to-list 'load-path (concat "~/.emacs.d/" name)))
(defun add-require (name)
  (add-path (symbol-name name))
  (require name))


;;; ido
(require 'ido)
(ido-mode t)
(add-require 'ido-better-flex)
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "TAB") 'ido-next-match)))

(add-path "")

;;; rinari - IDE for ruby/rails
(add-path "rinari/util/jump")
(require 'jump)
(add-path "rinari/util/inf-ruby")
(require 'inf-ruby)
(add-require 'rinari)

;;; paredit
(require 'paredit)

;;; common deps
(add-path "s.el")
(require 's)
(add-path "dash.el")
(require 'dash)


;;; Clojure
(add-require 'clojure-mode)
;;; nREPL
(add-path "nrepl.el")
(require 'nrepl)
(setq nrepl-popup-on-error nil)
(setq nrepl-history-file "~/.repl-history")
(add-hook 'nrepl-interaction-mode-hook
  'nrepl-turn-on-eldoc-mode)
(require 'clojure-test-mode)



(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;;; Rainbow delimiters
(add-require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)


;;; Toggle buffers for *nrepl* and *nrepl-error*
(defvar nrepl-hidden-by-buffers '())
(defvar nrepl-deselected-by-buffers '())

(defun buffer-in-next-window ()
  (if (one-window-p) nil
    (buffer-name (window-buffer (next-window)))))

(defun show-buffer-and-memorize-old (buffer selected)
  (let* ((will-be-hidden-buffer (buffer-in-next-window)))
    (add-to-list 'nrepl-hidden-by-buffers (list buffer will-be-hidden-buffer))
    (if selected
      (progn (add-to-list 'nrepl-deselected-by-buffers (list buffer (selected-window)))
             (pop-to-buffer buffer nil t))
      (display-buffer buffer))))

(defun hide-buffer-and-restore-old (buffer)
  (let* ((old (cadr (assoc buffer nrepl-hidden-by-buffers)))
         (old-selected-window (cadr (assoc buffer nrepl-deselected-by-buffers))))
     (assq-delete-all buffer nrepl-hidden-by-buffers)
     (assq-delete-all buffer nrepl-deselected-by-buffers)
     (if old
       (replace-buffer-in-windows buffer)
       (delete-windows-on buffer))
     (when old-selected-window
       (select-window old-selected-window))))

(defun toggle-buffer (buffer &optional selected)
  (interactive)
  (if (get-buffer buffer)
    (if (get-buffer-window buffer)
      (hide-buffer-and-restore-old buffer)
      (show-buffer-and-memorize-old buffer selected))
    (message "Buffer %s doesn't exist" buffer)))

(defun toggle-nrepl-buffer ()
  (interactive)
  (toggle-buffer "*nrepl*" t))

(defun toggle-nrepl-error-buffer ()
  (interactive)
  (toggle-buffer "*nrepl-error*"))

(defun define-toggle-keys ()
  (local-set-key (kbd "<f1>") 'toggle-nrepl-buffer)
  (local-set-key (kbd "<f2>") 'toggle-nrepl-error-buffer))

(add-hook 'clojure-mode-hook 'define-toggle-keys)
(add-hook 'nrepl-mode-hook 'define-toggle-keys)




;;; Projectile
(add-require 'projectile)
(setq projectile-require-project-root nil)
(projectile-global-mode)
(global-set-key  (kbd "M-p") 'projectile-find-file)

(global-whitespace-mode)
(setq whitespace-style  '(newline space-mark tab-mark face trailing spaces))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minibuffer-prompt ((t (:background "#242424" :foreground "#8ac6f2" :box (:line-width -1 :color "red" :style released-button) :weight bold))))
 '(whitespace-space ((t (:foreground "color-240")))))
(define-key global-map (kbd "RET") 'newline-and-indent)


; If auctex.el is loaded then auctex is installed and load other stuff
(when (load "auctex.el" t t t)
;  (load "preview-latex.el" nil t t) doesnt work for me. May be it doesn't work in no-window mode at all.
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-PDF-mode t)
  (setq-default TeX-master nil)
  (eval-after-load "tex"
    '(setcdr (assoc "LaTeX" TeX-command-list)
             '("%`%l%(mode) -shell-escape%' %t"
               TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX"))))


;;; Lua
(add-require 'lua-mode)

;;; Scala
(add-require 'scala-mode2)

(load-theme 'wombat)
