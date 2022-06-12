(require :core)

;; The name is up to you.
(module nvim-config
  {;; You can use Lua's regular require or Aniseed's autoload.
   require {: coq
            : gitsigns
            : nvim-tree}
    
   

            ;; Fennel destructuring syntax works but defeats the point of autoload.
            ;; Because a lookup is instantly invoked which triggers autoload at
            ;; module load time instead of when you need it in your code.
            ; {: some-fn} some-module}
    autoload {a aniseed.core}})


;; Setup NeovimTree as a replacement for nerd tree
(let [{: setup} (require :nvim-tree)]
  (setup {:view {:side :left :width 25 :hide_root_folder true}
          :disable_netrw true
          :hijack_netrw true
          :hijack_cursor true
          :update_cwd true
          :git {:enable false :ignore true}
          :hijack_directories {:enable true :auto_open true}
          :actions {:open_file {:resize_window true}}
          :renderer {:indent_markers {:enable false}}}))

;; setup gitsigns with no custom configuration
(let [{: setup} (require :gitsigns)]
  (setup))


;; Configure the language server configuration with bindings per supported buffer


;;*****************************************************
;; Configure Treesitter
;;*****************************************************
(local ts (require :nvim-treesitter))
(local tsq (require :nvim-treesitter.query))
(local tsp (require :nvim-treesitter.parsers))

(let [{: setup} (require :nvim-treesitter.configs)]
  ;; Usual setup for treesitter
  (setup {:ensure_maintained "maintained"
          :sync_install false
          :highlight { :enable true
                       :additional_vim_regex_highlighting false
                       :indent {:enable true}}}))

(defn nnoremap [from to opts]
  (let [map-opts {:noremap true}
        to (.. ":" to "<cr>")]
    (if (a.get opts :local?)
      (nvim.buf_set_keymap 0 :n from to map-opts)
      (nvim.set_keymap :n from to map-opts))))
   
;; Begin COQ
(vim.cmd "COQnow -s")





(local carbon
  (or (and (= vim.o.background :dark)
       {:base00 "#161616" ;; The origin color or the Carbon palette
        :base01 "#262626" ;; A brighter shade color based on base00
        :base02 "#393939" ;; An even more brighter shade color of base00
        :base03 "#525252" ;; The brightest shade color based on base00
        :base04 "#dde1e6" ;; The origin color or the Snow Storm sequence.
        :base05 "#f2f4f8" ;; A brighter shade color of base04
        :base06 "#ffffff" ;; The brightest shade color based on base04
        :base07 "#08bdba" ;; A calm and highly contrasted color reminiscent of glowing ice
        :base08 "#3ddbd9" ;; The bright and shiny primary accent color reminiscent of pure and clear energy
        :base09 "#ff7eb6" ;; A more darkened and less saturated color reminiscent of cherry blossoms
        :base10 "#ee5396" ;; A dark and intensive color reminiscent of the withering flowers come fall
        :base11 "#33b1ff" ;; But never have I been a blue calm sea. I have always been a storm
        :base12 "#78a9ff" ;; And the sky was never quite the same shade of blue again
        :base13 "#42be65" ;; Nature in her green, tranquil woods heals and soothes all afflictions
        :base14 "#be95ff" ;; I want to watch wisteria grow right over my bare feet
        :base15 "#82cfff" ;; A book must be an ice axe to break the seas frozen inside our soul
        :blend  "#131313" ;; Blend of #000000 & base00 for darker accents 
        :none :NONE})
   {:base00 "#ECEFF4" ;; The origin color or the Carbon palette
     :base01 "#E5E9F0" ;; A brighter shade color based on base00
     :base02 "#D8DEE9" ;; An even more brighter shade color of base00
     :base03 "#4C566A" ;; The brightest shade color based on base00
     :base04 "#434C5E" ;; The origin color or the Snow Storm sequence.
     :base05 "#3B4252" ;; A brighter shade color of base04
     :base06 "#2E3440" ;; The brightest shade color based on base04
     :base07 "#8FBCBB" ;; A calm and highly contrasted color reminiscent of glowing ice
     :base08 "#88C0D0" ;; The bright and shiny primary accent color reminiscent of pure and clear energy
     :base09 "#81A1C1" ;; A more darkened and less saturated color reminiscent of cherry blossoms
     :base10 "#5E81AC" ;; A dark and intensive color reminiscent of the withering flowers come fall
     :base11 "#BF616A" ;; But never have I been a blue calm sea. I have always been a storm
     :base12 "#D08770" ;; And the sky was never quite the same shade of blue again
     :base13 "#EBCB8B" ;; Nature in her green, tranquil woods heals and soothes all afflictions
     :base14 "#A3BE8C" ;; I want to watch wisteria grow right over my bare feet
     :base15 "#B48EAD" ;; A book must be an ice axe to break the seas frozen inside our soul
     :blend  "#FAFAFA" ;; Blend of #000000 & base00 for darker accents 
     :none :NONE}))
