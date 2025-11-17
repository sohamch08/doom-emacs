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
(setq yas-snippet-dirs '("~/.config/doom/snippets"))

;; (setq lsp-tex-server 'digestif)
(setq-hook! 'LaTeX-mode-hook
  +lsp-company-backends
  '(:separate
    company-capf
    company-yasnippet
    company-reftex-labels
    company-reftex-citations
    company-files
    company-dabbrev
    company-dabbrev-code))
