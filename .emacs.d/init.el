; init the package manager
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)
)

; gets rid of the blinking cursor because I hate it
(blink-cursor-mode 0)

; commands for making c/c++ editing better
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)
;(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

; major minor mode hooks
; 'prog-mode-hook runs during any programming mode
(add-hook 'prog-mode-hook #'electric-pair-mode)

; add {} to the list of characters electric-pair handles
(add-hook 'prog-mode-hook
		  (lambda ()
			(define-key prog-mode-map "{" 'electric-pair)))

;; windows open when you close are opened on next open
(desktop-save-mode t)
;; prevents emacs from becoming slow from having a lot of files to load from desktop-save-mode
(setq desktop-restore-eager 10)

;; helm and async setup
(add-to-list 'load-path "/home/joel/.emacs.d/helm")
(add-to-list 'load-path "/home/joel/.emacs.d/emacs-async")
(require 'helm-config)

(when (require 'dired-aux)
  (require 'dired-async))

;; helm implement
(global-set-key [?\M-x] 'helm-M-x)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(global-set-key (kbd "M-y") 'helm-show-kill-ring); show kill ring command
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(global-set-key (kbd "C-x C-f") 'helm-find-files) ; replace for find file
;; use a better grep
(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))
(helm-mode 1)
(semantic-mode 1)
;; set fuzzy match for semantic definition finding
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)
;; get man page under point
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
(setq helm-locate-fuzzy-match t)
;; set occur command to a better key combo
(global-set-key (kbd "C-c h o") 'helm-occur)
(setq helm-apropos-function-list t)
(setq helm-lisp-fuzzy-completion t)
;; show all of the mark ring
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
;; set register key to be better
(global-set-key (kbd "C-c h x") 'helm-register)
;; set helm eval to be more consistent in key chords
(global-set-key (kbd "C-c h M-:") 'helm-eval-expression-with-eldoc)
;; retrieve old eshell commands
(require 'helm-eshell)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))
(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
;; helm lookup keybind
(require 'helm-descbinds)
(helm-descbinds-mode)
;; ggtags setup
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))
(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
(setq-local imenu-create-index-function #'moo-jump-local)
;; company code completion
(require 'company)
;(require 'helm-company)
(add-hook 'after-init-hook 'global-company-mode)
;; semantic code completion
(require 'cc-mode)
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)
;; function-args
(require 'function-args)
(fa-config-default)
(define-key c-mode-map  [(contrl tab)] 'moo-complete)
(define-key c++-mode-map  [(control tab)] 'moo-complete)
(define-key c-mode-map (kbd "M-o")  'fa-show)
(define-key c++-mode-map (kbd "M-o")  'fa-show)
