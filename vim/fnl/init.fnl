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
          ;; :actions {:open_file {:resize_window true}}
          :renderer {:indent_markers {:enable false}}}))

;; setup gitsigns with no custom configuration
(let [{: setup} (require :gitsigns)]
  (setup))


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

