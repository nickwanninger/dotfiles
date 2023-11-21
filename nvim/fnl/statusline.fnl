
;; Icons/text for each mode 
(local modes
  {:n :RW
   :no :RO
   :v  "**"
   :V  "**"
   "\022" "**"
   :s  :S
   :S  :SL
   "\019" :SB
   :i  "**"
   :ic "**"
   :R  :RA
   :Rv :RV
   :c  :VIEX
   :cv :VIEX
   :ce :EX
   :r  :r
   :rm :r
   :r? :r
   :! "!"
   :t :t})

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Configure the statusline to look nice
(fn mode-color []
  (let [mode (. (vim.api.nvim_get_mode) :mode)]
    (if (= mode :n) "%#StatusNormal#"
        ;; Insert modes
        (or (= mode :i) (= mode :ic)) "%#StatusInsert#"
        ;; The different visual modes
        (or (or (= mode :v) (= mode :V)) (= mode "\022")) "%#StatusVisual#"
        (= mode :R) "%#StatusReplace#"
        (= mode :c) "%#StatusCommand#"
        (= mode :t) "%#StatusTerminal#"
        "%#StatusLine#")))


(fn get-lsp-diagnostic []
  (local buf-clients (vim.lsp.buf_get_clients 0))
  (local next next)
  (when (= (next buf-clients) nil)
    (lua "return \"\""))

  (local diagnostics (vim.diagnostic.get 0))
  (local count [0 0 0 0])

  (each [_ diagnostic (ipairs diagnostics)]
    (tset count diagnostic.severity (+ (. count diagnostic.severity) 1)))
  (local result {:errors (. count vim.diagnostic.severity.ERROR)
                 :warnings (. count vim.diagnostic.severity.WARN)
                 :info (. count vim.diagnostic.severity.INFO)
                 :hints (. count vim.diagnostic.severity.HINT)})

  (string.format " %%#StatusLineDiagnosticWarn#%s %%#StatusLineDiagnosticError#%s "
                 (or (. result :warnings) 0) (or (. result :errors) 0)))


(local json (require :json))

(fn get-git-status []
  (if vim.b.gitsigns_status_dict
    (let [s vim.b.gitsigns_status_dict]
      ;; (print (json.encode s))
      (table.concat [s.head
                     "%#StatusLineDiagnosticWarn#"
                     (string.format " +%d " s.added)

                     "%#StatusLineDiagnosticError#"
                     (string.format " -%d " s.removed)

                     "%#StatusLineDiagnosticHint#"
                     (string.format " ~%d " s.changed)
                     "%#StatusLine#"]))
    ""))

(global statusline
  (fn [] (table.concat [(mode-color)
                        (: (string.format " %s "
                              (. modes
                                (. (vim.api.nvim_get_mode) :mode)))
                           :upper)
                        "%#StatusLine#"
                        " %f "
                        "%#StatusPosition#"
                        "%="
                        ;; (get-git-status)
                        "%#StatusPosition#"
                        " %l:%c "
                        (get-lsp-diagnostic)])))

(vim.cmd "set noshowmode")
(vim.cmd "set laststatus=3")
(vim.cmd "set statusline=%!v:lua.statusline()")
