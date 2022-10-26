(macro highlight! [name attributes colors]
  "Sets a highlight group globally using the vim.api.nvim_set_hl API.
  Accepts the following arguments:
  name -> a symbol.
  attributes -> a list of boolean attributes:
    - bold
    - italic
    - reverse
    - inverse
    - standout
    - underline
    - underlineline
    - undercurl
    - underdot
    - underdash
    - strikethrough
    - default
  colors -> a table of colors: fg bg ctermfg ctermbg
  Example of use:
  ```fennel
  (highlight! Error [:bold] {:fg \"#ff0000\"})
  ```
  That compiles to:
  ```fennel
  (vim.api.nvim_set_hl 0 \"Error\" {:fg \"#ff0000\"
                                    :bold true})
  ```"
  (let [name (tostring name)
        definition (collect [_ attr (ipairs attributes)
                             :into colors]
                     (tostring attr) true)]
    `(vim.api.nvim_set_hl 0 ,name ,definition)))

(vim.cmd "set background=dark")



(lambda select-colorscheme [theme]
  (let [{: setup} (require :base16-colorscheme)] (setup theme))

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; editor
  (highlight! ColorColumn [] {:fg :NONE :bg theme.base01})
  (highlight! Cursor [] {:fg theme.base00 :bg theme.base04})
  (highlight! CursorLine [] {:fg :NONE :bg theme.base01})
  (highlight! CursorColumn [] {:fg :NONE :bg theme.base01})
  (highlight! CursorLineNr [] {:fg theme.base04 :bg :NONE})
  (highlight! Error [] {:fg theme.base04 :bg theme.base0B})
  (highlight! LineNr [] {:fg theme.base03 :bg :NONE})

  ;; diagnostic
  (highlight! DiagnosticWarn [] {:fg theme.base08 :bg :NONE})
  (highlight! DiagnosticError [] {:fg theme.base0A :bg :NONE})
  (highlight! DiagnosticInfo [] {:fg theme.base04 :bg :NONE})
  (highlight! DiagnosticHint [] {:fg theme.base04 :bg :NONE})
  (highlight! DiagnosticUnderlineWarn [:undercurl] {:fg theme.base08 :bg :NONE})
  (highlight! DiagnosticUnderlineError [:undercurl] {:fg theme.base0A :bg :NONE})
  (highlight! DiagnosticUnderlineInfo [:undercurl] {:fg theme.base04 :bg :NONE})
  (highlight! DiagnosticUnderlineHint [:undercurl] {:fg theme.base04 :bg :NONE})

  ;; A nicer NvimTree theme
  (highlight! NvimTreeImageFile [] {:fg theme.base0C})
  (highlight! NvimTreeFolderIcon [] {:fg theme.base0C})
  (highlight! NvimTreeWinSeparator [] {:fg theme.base00 :bg theme.base00})
  (highlight! NvimTreeFolderName [] {:fg theme.base09})
  (highlight! NvimTreeIndentMarker [] {:fg theme.base02})
  (highlight! NvimTreeEmptyFolderName [] {:fg theme.base0F})
  (highlight! NvimTreeOpenedFolderName [] {:fg theme.base0F})
  ;; (highlight! NvimTreeNormal [] {:fg theme.base04 :bg :NONE})

  ;; (highlight! Normal [] {:fg :NONE :bg :NONE})
  ;; (highlight! InactiveWindow [] {:fg :NONE :bg :NONE})

  ;; Normal Config
  (vim.cmd "hi MatchParen gui=underline guibg=#393939")
  (highlight! NonText [] {:fg theme.base02})
  (highlight! Pmenu [] {:fg theme.base04 :bg theme.base01})
  (highlight! PmenuSbar [] {:fg theme.base04 :bg theme.base01})
  (highlight! PmenuSel [] {:fg theme.base08 :bg theme.base02})
  (highlight! PmenuThumb [] {:fg theme.base08 :bg theme.base02})
  (highlight! SpecialKey [] {:fg theme.base03})
  (highlight! Visual [] {:fg :NONE :bg theme.base02})
  (highlight! VisualNOS [] {:fg :NONE :bg theme.base02})

  ;; window
  (highlight! Title [] {:fg theme.base04})
  (highlight! VertSplit [] {:fg theme.base02 :bg theme.base00})

  ;; (highlight! Normal [] {:fg theme.base04 :bg theme.base00})
  (highlight! StatusLine [] {:fg theme.base04 :bg theme.base00})
  (highlight! StatusPosition [] {:fg theme.base04 :bg theme.base00})
  (highlight! StatusNormal [] {:fg theme.base00 :bg theme.base0D})
  (highlight! StatusReplace [] {:fg theme.base00 :bg theme.base08})
  (highlight! StatusInsert [] {:fg theme.base00 :bg theme.base0B})
  (highlight! StatusVisual [] {:fg theme.base00 :bg theme.base0E})
  (highlight! StatusTerminal [] {:fg theme.base00 :bg theme.base0F})
  (vim.cmd "hi StatusLineDiagnosticWarn gui=bold guifg=#be95ff guibg=#161616")
  (vim.cmd "hi StatusLineDiagnosticError gui=bold guifg=#ff7eb6 guibg=#161616"))

;; Definitions for the IBM carbon colorscheme are stolen from Nyoom.vim
(local carbon-dark
     {:base00 "#161616" :base01 "#262626" :base02 "#393939" :base03 "#525252"
      :base04 "#dde1e6" :base05 "#f2f4f8" :base06 "#ffffff" :base07 "#08bdba"
      :base08 "#3ddbd9" :base09 "#ff7eb6" :base0A "#ee5396" :base0B "#33b1ff"
      :base0C "#78a9ff" :base0D "#42be65" :base0E "#be95ff" :base0F "#82cfff"})

(local carbon-light
     {:base00 "#ECEFF4" :base01 "#E5E9F0" :base02 "#D8DEE9" :base03 "#4C566A"
      :base04 "#434C5E" :base05 "#3B4252" :base06 "#2E3440" :base07 "#8FBCBB"
      :base08 "#88C0D0" :base09 "#81A1C1" :base0A "#5E81AC" :base0B "#BF616A"
      :base0C "#D08770" :base0D "#EBCB8B" :base0E "#A3BE8C" :base0F "#B48EAD"})

(local snazzy-black
     {:base00 "#000000" :base01 "#34353e" :base02 "#43454f" :base03 "#78787e"
      :base04 "#a5a5a9" :base05 "#e2e4e5" :base06 "#eff0eb" :base07 "#f1f1f0"
      :base08 "#ff5c57" :base09 "#ff9f43" :base0A "#f3f99d" :base0B "#5af78e"
      :base0C "#9aedfe" :base0D "#57c7ff" :base0E "#ff6ac1" :base0F "#b2643c"})
   
(local snazzy
     {:base00 "#282a36" :base01 "#34353e" :base02 "#43454f" :base03 "#78787e"
      :base04 "#a5a5a9" :base05 "#e2e4e5" :base06 "#eff0eb" :base07 "#f1f1f0"
      :base08 "#ff5c57" :base09 "#ff9f43" :base0A "#f3f99d" :base0B "#5af78e"
      :base0C "#9aedfe" :base0D "#57c7ff" :base0E "#ff6ac1" :base0F "#b2643c"})

(local contrast-light
     {:base00 "#FFFFFF" :base01 "#000000" :base02 "#000000" :base03 "#000000"
      :base04 "#000000" :base05 "#000000" :base06 "#000000" :base07 "#000000"
      :base08 "#000000" :base09 "#000000" :base0A "#000000" :base0B "#000000"
      :base0C "#000000" :base0D "#000000" :base0E "#000000" :base0F "#000000"})
{: select-colorscheme
 : snazzy
 : snazzy-black
 : carbon-dark
 : carbon-light
 : contrast-light}
