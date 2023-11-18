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

  (when (not (?. theme :cursor))
    (tset theme :cursor theme.base01))

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; editor
  (highlight! ColorColumn [] {:fg :NONE :bg theme.base01})
  (highlight! Cursor [] {:fg theme.base00 :bg theme.base04})
  (highlight! CursorLine [] {:fg :NONE :bg theme.cursor})
  (highlight! CursorColumn [] {:fg :NONE :bg theme.cursor})
  (highlight! CursorLineNr [] {:fg theme.base04 :bg :NONE})
  (highlight! Error [] {:fg theme.base04 :bg theme.base0B})
  (highlight! LineNr [] {:fg theme.base03 :bg :NONE})

  ;; diagnostic
  (highlight! DiagnosticWarn [] {:fg theme.base08 :bg :NONE})
  (highlight! DiagnosticError [] {:fg theme.base09 :bg :NONE})
  (highlight! DiagnosticInfo [] {:fg theme.base04 :bg :NONE})
  (highlight! DiagnosticHint [] {:fg theme.base04 :bg :NONE})
  (highlight! DiagnosticUnderlineWarn [:undercurl] {:fg theme.base09 :bg :NONE})
  (highlight! DiagnosticUnderlineError [:undercurl] {:fg theme.base0A :bg :NONE})
  (highlight! DiagnosticUnderlineInfo [:undercurl] {:fg theme.base04 :bg :NONE})
  (highlight! DiagnosticUnderlineHint [:undercurl] {:fg theme.base04 :bg :NONE})

  (highlight! TSComment [] {:fg theme.base03 :bg :NONE})
  ; hi.TSComment            = { guifg = M.colors.base03, guibg = nil, gui = 'italic', guisp = nil}


  ;; A nicer NvimTree theme
  (highlight! NvimTreeImageFile [] {:fg theme.base0C})
  (highlight! NvimTreeFolderIcon [] {:fg theme.base0C})
  (highlight! NvimTreeWinSeparator [] {:fg theme.base00 :bg theme.base00})
  (highlight! NvimTreeFolderName [] {:fg theme.base09})
  (highlight! NvimTreeIndentMarker [] {:fg theme.base02})
  (highlight! NvimTreeEmptyFolderName [] {:fg theme.base0F})
  (highlight! NvimTreeOpenedFolderName [] {:fg theme.base0F})


  ;; (highlight! BufferVisible [] {:bg theme.base00})
  ;; (highlight! BufferTabpages [] {:bg theme.base00})
  ;; (highlight! BufferTabpageFill [] {:bg theme.base00})
  ;; (highlight! BufferInactive [] {:bg theme.base00})
  ;; (highlight! BufferInactiveIndex [] {:bg theme.base00})
  ;; (highlight! BufferInactiveMod [] {:bg theme.base00})
  ;; (highlight! BufferInactiveTarget [] {:bg theme.base00})

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
  (vim.cmd "hi StatusLineDiagnosticError gui=bold guifg=#ff7eb6 guibg=#161616")


  (highlight! NeogitDiffContext   [] {:bg "#111111" :fg "#FFFFFF"})
  (highlight! NeogitDiffAdd       [] {:bg "#003300" :fg "#00FF00"})
  (highlight! NeogitDiffDelete    [] {:bg "#330000" :fg "#FF0000"})


  (highlight! NeogitDiffContextHighlight   [] {:bg "#111111" :fg "#FFFFFF"})
  (highlight! NeogitDiffAddHighlight       [] {:bg "#003300" :fg "#00FF00"})
  (highlight! NeogitDiffDeleteHighlight    [] {:bg "#330000" :fg "#FF0000"}))



;
; Definitions for the IBM carbon colorscheme are stolen from Nyoom.vim
(local carbon-dark
     {:base00 "#161616" :base01 "#262626" :base02 "#393939" :base03 "#525252"
      :base04 "#dde1e6" :base05 "#f2f4f8" :base06 "#ffffff" :base07 "#08bdba"
      :base08 "#3ddbd9" :base09 "#ff7eb6" :base0A "#ee5396" :base0B "#33b1ff"
      :base0C "#78a9ff" :base0D "#42be65" :base0E "#be95ff" :base0F "#82cfff"})

(local carbon-black
     {:base00 "#000000" :base01 "#262626" :base02 "#393939" :base03 "#525252"
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



(local material-darker
     {:base00 "#212121" :base01 "#303030" :base02 "#353535" :base03 "#4A4A4A"
      :base04 "#B2CCD6" :base05 "#EEFFFF" :base06 "#EEFFFF" :base07 "#FFFFFF"
      :base08 "#F07178" :base09 "#F78C6C" :base0A "#FFCB6B" :base0B "#C3E88D"
      :base0C "#89DDFF" :base0D "#82AAFF" :base0E "#C792EA" :base0F "#FF5370"})


(local material-black
     {:base00 "#000000" :base01 "#303030" :base02 "#353535" :base03 "#4A4A4A"
      :base04 "#B2CCD6" :base05 "#EEFFFF" :base06 "#EEFFFF" :base07 "#FFFFFF"
      :base08 "#F07178" :base09 "#F78C6C" :base0A "#FFCB6B" :base0B "#C3E88D"
      :base0C "#89DDFF" :base0D "#82AAFF" :base0E "#C792EA" :base0F "#FF5370"})


(local tomorrow-light
     {:base00 "#ffffff" :base01 "#e0e0e0" :base02 "#d6d6d6" :base03 "#8e908c"
      :base04 "#969896" :base05 "#4d4d4c" :base06 "#282a2e" :base07 "#1d1f21"
      :base08 "#c82829" :base09 "#f5871f" :base0A "#eab700" :base0B "#718c00"
      :base0C "#3e999f" :base0D "#4271ae" :base0E "#8959a8" :base0F "#a3685a"})


(local helios-dark
  {:base00 "#1d2021"
   :base01 "#383c3e"
   :base02 "#53585b"
   :base03 "#6f7579"
   :base04 "#cdcdcd"
   :base05 "#d5d5d5"
   :base06 "#dddddd"
   :base07 "#e5e5e5"
   :base08 "#d72638"
   :base09 "#eb8413"
   :base0A "#f19d1a"
   :base0B "#88b92d"
   :base0C "#1ba595"
   :base0D "#1e8bac"
   :base0E "#be4264"
   :base0F "#c85e0d"})
   
(local bro-dark
  {:base00 "#1f1f1f"
   :base01 "#f81118"
   :base02 "#2dc55e"
   :base03 "#ecba0f"
   :base04 "#2a84d2"
   :base05 "#4e5ab7"
   :base06 "#1081d6"
   :base07 "#d6dbe5"
   :base08 "#d6dbe5"
   :base09 "#de352e"
   :base0A "#1dd361"
   :base0B "#f3bd09"
   :base0C "#1081d6"
   :base0D "#5350b9"
   :base0E "#0f7ddb"
   :base0F "#ffffff"})
(local one-light
  {:base00 "#ffffff"
   :base01 "#f0f0f1"
   :base02 "#e5e5e6"
   :base03 "#a0a1a7"
   :base04 "#696c77"
   :base05 "#383a42"
   :base06 "#202227"
   :base07 "#090a0b"
   :base08 "#ca1243"
   :base09 "#d75f00"
   :base0A "#c18401"
   :base0B "#50a14f"
   :base0C "#0184bc"
   :base0D "#4078f2"
   :base0E "#a626a4"
   :base0F "#986801"})



(local nord-dark
  {:base00 "#2E3440"
   :base01 "#3B4252"
   :base02 "#434C5E"
   :base03 "#4C566A"
   :base04 "#D8DEE9"
   :base05 "#E5E9F0"
   :base06 "#ECEFF4"
   :base07 "#8FBCBB"
   :base08 "#88C0D0"
   :base09 "#81A1C1"
   :base0A "#5E81AC"
   :base0B "#BF616A"
   :base0C "#D08770"
   :base0D "#EBCB8B"
   :base0E "#A3BE8C"
   :base0F "#B48EAD"})
   

(local nord-black
  {:base00 "#000000"
   :base01 "#3B4252"
   :base02 "#434C5E"
   :base03 "#4C566A"
   :base04 "#D8DEE9"
   :base05 "#E5E9F0"
   :base06 "#ECEFF4"
   :base07 "#8FBCBB"
   :base08 "#88C0D0"
   :base09 "#81A1C1"
   :base0A "#5E81AC"
   :base0B "#BF616A"
   :base0C "#D08770"
   :base0D "#EBCB8B"
   :base0E "#A3BE8C"
   :base0F "#B48EAD"})


(local gruvbox-light-hard
  {:base00 "#f9f5d7"
   :base01 "#ebdbb2"
   :base02 "#d5c4a1"
   :base03 "#bdae93"
   :base04 "#665c54"
   :base05 "#504945"
   :base06 "#3c3836"
   :base07 "#282828"
   :base08 "#9d0006"
   :base09 "#af3a03"
   :base0A "#b57614"
   :base0B "#79740e"
   :base0C "#427b58"
   :base0D "#076678"
   :base0E "#8f3f71"
   :base0F "#d65d0e"})
   
(local gruvbox-dark-hard
  {:base00 "#1d2021"
   :base01 "#3c3836"
   :base02 "#504945"
   :base03 "#665c54"
   :base04 "#bdae93"
   :base05 "#d5c4a1"
   :base06 "#ebdbb2"
   :base07 "#fbf1c7"
   :base08 "#fb4934"
   :base09 "#fe8019"
   :base0A "#fabd2f"
   :base0B "#b8bb26"
   :base0C "#8ec07c"
   :base0D "#83a598"
   :base0E "#d3869b"
   :base0F "#d65d0e"})



(local atlas
  {;; :base00 "#002635"
   :base00 "#000000"
   :base01 "#00384d"
   :base02 "#517F8D"
   :base03 "#6C8B91"
   :base04 "#869696"
   :base05 "#a1a19a"
   :base06 "#e6e6dc"
   :base07 "#fafaf8"
   :base08 "#ff5a67"
   :base09 "#f08e48"
   :base0A "#ffcc1b"
   :base0B "#7fc06e"
   :base0D "#5dd7b9"
   :base0C "#14747e"
   :base0E "#9a70a4"
   :base0F "#c43060"})
   

{: select-colorscheme
 : snazzy
 : snazzy-black
 : carbon-dark
 : carbon-black
 : carbon-light
 : material-darker
 : material-black
 : contrast-light
 : tomorrow-light
 : helios-dark
 : one-light
 : nord-dark
 : nord-black
 : gruvbox-light-hard
 : gruvbox-dark-hard
 : bro-dark
 : atlas}


