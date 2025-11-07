;;; anysphere-theme.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: November 7, 2025
;; Author: nickwanninger <github.com/nickwanninger>
;; Maintainer:
;; Source:
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup anysphere-theme nil
  "Options for the `anysphere' theme."
  :group 'doom-themes)

(defcustom anysphere-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'anysphere-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme anysphere
  "A theme based off of the Anysphere VSCode theme"

  ;; name        gui       256       16
  ((bg         '("#1a1a1a" nil       nil))
   (bg-alt     '("#141414" nil       nil))
   (base0      '("#0d0d0d" "black"   "black"))
   (base1      '("#141414" "#141414"))
   (base2      '("#1a1a1a" "#1a1a1a"))
   (base3      '("#2A2A2A" "#2A2A2A" "brightblack"))
   (base4      '("#404040" "#404040" "brightblack"))
   (base5      '("#505050" "#505050" "brightblack"))
   (base6      '("#636262" "#636262" "brightblack"))
   (base7      '("#CCCCCC" "#CCCCCC" "brightblack"))
   (base8      '("#FFFFFF" "#FFFFFF" "white"))
   (fg         '("#D8DEE9" "#D8DEE9" "white"))
   (fg-alt     '("#CCCCCC99" "#CCCCCC99"))

   ;; Anysphere primary colors
   (red        '("#BF616A" "#BF616A" "red"))
   (orange     '("#EBCB8B" "#EBCB8B" "orange"))
   (yellow     '("#EBCB8B" "#EBCB8B" "yellow"))
   (green      '("#A3BE8C" "#A3BE8C" "green"))
   (blue       '("#81A1C1" "#81A1C1" "brightblue"))
   (teal       '("#88C0D0" "#88C0D0" "brightblue"))
   (magenta    '("#B48EAD" "#B48EAD" "magenta"))
   (cyan       '("#88C0D0" "#88C0D0" "cyan"))
   (dark-cyan  (doom-darken cyan 0.4))

   ;; Anysphere accent colors for semantic highlighting
   (grey       '("#505050" "#505050" "brightblack"))
   (light-green '("#A3BE8C" "#A3BE8C" "lightgreen"))
   (violet     '("#B48EAD" "#B48EAD" "brightmagenta"))
   (dark-blue  '("#81A1C1" "#81A1C1" "darkblue"))
   (pink       '("#B48EAD" "#B48EAD" "pink"))

   ;; face categories - mapped to Anysphere semantic tokens
   (highlight      teal)
   (vertical-bar   `("#141414" ,@base0))
   (selection      `(,(car (doom-lighten bg 0.1)) ,@(cdr base4)))
   (builtin        teal)
   (comments       grey)
   (doc-comments   (doom-lighten grey 0.14))
   (constants      '("#83d6c5" "#83d6c5"))
   (functions      '("#efb080" "#efb080"))
   (keywords       teal)
   (methods        '("#efb080" "#efb080"))
   (operators      orange)
   (type           blue)
   (strings        '("#e394dc" "#e394dc"))
   (variables      '("#d6d6dd" "#d6d6dd"))
   (numbers        orange)
   (region         selection)
   (error          red)
   (warning        orange)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     `(,(doom-darken (car bg-alt) 0.3) ,@(cdr base3)))
   (modeline-bg-alt `(,(car bg) ,@(cdr base1)))
   (modeline-fg     base8)
   (modeline-fg-alt comments)
   (-modeline-pad
    (when anysphere-padded-modeline
      (if (integerp anysphere-padded-modeline)
          anysphere-padded-modeline
        4))))

  ;; --- faces ------------------------------
  (((font-lock-keyword-face &override) :weight 'bold)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground orange :weight 'bold)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   ;;;; doom-modeline
   (doom-modeline-buffer-path       :foreground dark-blue :bold bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)

   ;;;; tree-sitter faces
   (treesit-font-lock-macro-face       :foreground green)
   (treesit-font-lock-preprocessor-face :foreground green)

   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground yellow)
   (rainbow-delimiters-depth-2-face :foreground blue)
   (rainbow-delimiters-depth-3-face :foreground orange)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground magenta)
   (rainbow-delimiters-depth-6-face :foreground violet)
   (rainbow-delimiters-depth-7-face :foreground teal)))

  ;; --- variables --------------------------
  ;; ()
  

;;; anysphere-theme.el ends here
