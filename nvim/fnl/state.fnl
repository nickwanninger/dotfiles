(local json (require :json))
(local fwatch (require :fwatch))

(local state-path "/tmp/state.json")
(local handlers {})


(fn read []
  (let [file (io.open "/tmp/state.json" "r")]
     (if file (let [contents (file:read "a")]
                  ;; (print contents)
                  (file:close)
                  contents)
              "{}")))

(fn write [s]
  (let [file (io.open "/tmp/state.json" "w+")]
    (file:write (json.encode s))
    (file:close)))


(fn dispatch-new-state []
  (let [contents (read)]
     (when (~= contents "")
       (each [key value (pairs (json.decode contents))]
         (let [callback (?. handlers key)]
           (when callback (callback value)))))))


(fn state-changed [name events]
  (when (?. events :change)
    (let [contents (read)]
        (vim.schedule (fn [] (dispatch-new-state))))))

(fwatch.watch state-path {:on_event state-changed})


(fn register [key callback]
  (tset handlers key callback))


(fn set-val [key value]
  (local state (json.decode (read)))
  (tset state key value)
  (write state))


(fn default [key value]
  (local state (json.decode (read)))
  (if (?. state key)
    nil
    (do (tset state key value)
        (write state))))


(fn start []
  (dispatch-new-state))
  
  

{: register
 : set-val
 : default
 : start}


