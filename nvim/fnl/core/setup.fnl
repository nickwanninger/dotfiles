
(fn setup [name config cb]
  "A simple wrapper for the common .setup() system that most lua plugins use"
  (let [pkg (require name)]
      (pkg.setup config)
      (when cb
        (cb pkg))))


{: setup}