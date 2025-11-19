;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-henna)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(add-hook 'focus-out-hook 'save-buffer)
;; (setq pdf-info-epdfinfo-program
;;       (car (file-expand-wildcards "/nix/store/43hd59byk82cpxlkbm91j027j4rqnrp8-emacs-pdf-tools-20240429.407/share/emacs/site-lisp/elpa/pdf-tools-*/epdfinfo")))
;; Add pdf-tools Elisp to load-path



;; (after! tex
;;   (setq TeX-view-program-selection nil)
;;   (setq +latex-viewers '(pdf-tools skim))
;;   (load! "$HOME/.emacs.d/modules/lang/latex/+viewers"))

(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; Asks master file when first time opened a .tex.file. Lets auctex know 'For all files in the same directory, remember and reuse a single master file'
(setq-default TeX-master 'shared)

;; Auto enables LaTeX-mode for tex file
(add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))

;; Enable automatic correlation
;; (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
;; (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
;; (setq TeX-source-correlate-method 'synctex)

;; (after! tex
;;   (setq TeX-source-correlate-mode t)  ;; Enable synctex correlation
;;   (setq TeX-source-correlate-start-server t) ;; Start server for inverse search
;;   (setq TeX-view-program-selection
;;         '((output-pdf "PDF Tools")))
;;   (setq TeX-view-program-list
;;         '(("PDF Tools" TeX-pdf-tools-sync-view)))
;;   (add-hook 'TeX-after-compilation-finished-functions
;;             #'TeX-revert-document-buffer))
(after! tex
  ;; Enable source correlation (synctex)
  (setq TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server t
        TeX-engine 'default
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))

  ;; Automatically open and sync PDF after successful compilation
  (defun my/TeX-after-compile-finish (file)
    "Automatically view the PDF after successful LaTeX compilation."
    (when (and (TeX-active-process)
               (not (TeX-process-get-variable (TeX-active-process) 'TeX-error)))
      (TeX-view)))  ;; Open the viewer and sync position

  ;; Add the hook
  (add-hook 'TeX-after-compilation-finished-functions #'my/TeX-after-compile-finish)

  ;; Refresh PDF buffer automatically
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

;; Fix misaligned initial PDF view after SPC m v
(after! pdf-view
  (defun my/pdf-view-recenter-after-open ()
    "Recenter the PDF after opening to fix right offset."
    (when (derived-mode-p 'pdf-view-mode)
      (pdf-view-fit-page-to-window)
      (run-with-timer 0.3 nil #'pdf-view-fit-page-to-window)))
  (add-hook 'pdf-view-mode-hook #'my/pdf-view-recenter-after-open))

;; Move by visual lines instead of logical lines
(map! :n "j" #'evil-next-visual-line
      :n "k" #'evil-previous-visual-line
      :v "j" #'evil-next-visual-line
      :v "k" #'evil-previous-visual-line)

;; (setq-hook! 'LaTeX-mode-hook
;;   +lsp-company-backends
;;   '(:separate
;;     company-capf
;;     company-yasnippet
;;     company-reftex-labels
;;     company-reftex-citations
;;     company-files
;;     company-dabbrev
;;     company-dabbrev-code))


(use-package rainbow-delimiters
  :hook ((LaTeX-mode . rainbow-delimiters-mode)
         (latex-mode . rainbow-delimiters-mode)))

;; (use-package golden-ratio
;;   :ensure t
;;   :hook (after-init . golden-ratio-mode))
;; (after! golden-ratio
;;   ;; Resize after window moves with Evil
;;   (dolist (cmd '(evil-window-left
;;                  evil-window-right
;;                  evil-window-up
;;                  evil-window-down))
;;     (add-to-list 'golden-ratio-extra-commands cmd)))
(use-package! yasnippet
  :hook (latex-mode . yas-minor-mode))
(setq yas-snippet-dirs '("~/.config/doom/snippets"))

(add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)

(setq lsp-tex-server 'digestif)
(setq lsp-texlab-completion-snippets t) ;; enable snippet completions

;; (use-package! kind-icon
;;   :ensure t
;;   :after corfu
;;   ;:custom
;;   ; (kind-icon-blend-background t)
;;   ; (kind-icon-default-face 'corfu-default) ; only needed with blend-background
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
(use-package! corfu
  ;; :ensure t
  :hook (((prog-mode text-mode tex-mode ielm-mode) . corfu-mode)
         ;; ((shell-mode eshell-mode) . my/corfu-shell-settings)
         (minibuffer-setup . my/corfu-enable-always-in-minibuffer))
  :bind (:map corfu-map
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous)
         ("RET" . nil)
         ("M-RET" . corfu-insert)
         ("M-." . corfu-show-location)
         ("M-h" . nil)
         ([remap next-line] . nil)
         ([remap previous-line] . nil)
         ("M-." . corfu-info-location)
         ("C-h" . corfu-info-documentation))
  :config
  (setq corfu-auto-prefix 4
        corfu-auto-delay 0.07
        corfu-count 8
        corfu-auto  t
        corfu-cycle t
        corfu-quit-no-match 'separator
        corfu-preselect 'prompt
        corfu-scroll-margin 5)

  ;; Extensions
  (use-package! corfu-info
    :bind (:map corfu-map ("M-g" . nil)))
  (use-package! corfu-history :defer 3 :config (corfu-history-mode 1))
  (use-package! corfu-popupinfo
    :bind ( :map corfu-map
            ([remap corfu-info-documentation] . corfu-popupinfo-toggle))
    :config
    (setq corfu-popupinfo-hide nil
          corfu-popupinfo-delay '(2 . 0.2))
    (corfu-popupinfo-mode 1))
  (use-package! corfu-quick
    :bind (:map corfu-map ("'" . corfu-quick-complete))
    :config (setq corfu-quick1 "asdfghjkl;"))

  ;; Corfu in the minibuffer
  (defvar my-corfu-minibuffer-exclude-modes (list read-passwd-map)
    "Minibuffer-local keymaps for which Corfu should be disabled.")
  (defvar my-corfu-minibuffer-exclude-commands
    '(org-ql-find)
    "Minibuffer commands for which Corfu should be disabled.")
  (defun my/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active."
    (unless (or (bound-and-true-p vertico--input)
                (memq this-command my-corfu-minibuffer-exclude-commands)
                (memq (current-local-map)
                      my-corfu-minibuffer-exclude-modes))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (use-package! consult
    :bind (:map corfu-map
           ("M-m" . corfu-move-to-minibuffer)
           ("C-<tab>" . corfu-move-to-minibuffer))
    :config
    (defun corfu-move-to-minibuffer ()
      (interactive)
      (pcase completion-in-region--data
        (`(,beg ,end ,table ,pred ,extras)
         (let ((completion-extra-properties extras)
               completion-cycle-threshold completion-cycling)
           (consult-completion-in-region beg end table pred)))))
    (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

  ;; Corfu in the shell
  (defun my/corfu-shell-settings ()
    (setq-local corfu-quit-no-match t
                corfu-auto nil)
    (setq-local corfu-map (copy-keymap corfu-map)
                completion-cycle-threshold nil)
    (define-key corfu-map "\r" #'corfu-insert-and-send)
    (corfu-mode))
  (defun corfu-insert-and-send ()
    (interactive)
    ;; 1. First insert the completed candidate
    (corfu-insert)
    ;; 2. Send the entire prompt input to the shell
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((derived-mode-p 'comint-mode)
      (comint-send-input))))

  ;; Faster corfu
  ;; Disabled -- interferes with dynamic completion tables
  (use-package! orderless
    :disabled
    :after orderless
    :hook (corfu-mode . my/corfu-comp-style)
    :config
    (defun my/corfu-comp-style ()
      "Set/unset a fast completion style for corfu"
      (if corfu-mode
          (setq-local completion-styles '(orderless-fast))
        (kill-local-variable 'completion-styles)))))

(use-package! kind-icon
  :disabled
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package! nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (setq nerd-icons-corfu-mapping
        '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
          (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
          ;; ...
          (t :style "cod" :icon "code" :face font-lock-warning-face)))
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


;; (provide 'setup-corfu)
;;; setup-corfu.el ends here
