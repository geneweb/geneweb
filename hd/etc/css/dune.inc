
(rule
  (target datatables.min.css.gz)
  (deps (:input datatables.min.css))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target datatables.min.css.br)
  (deps (:input datatables.min.css))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target bootstrap.min.css.gz)
  (deps (:input bootstrap.min.css))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target bootstrap.min.css.br)
  (deps (:input bootstrap.min.css))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target tom-select.bootstrap5.min.css.gz)
  (deps (:input tom-select.bootstrap5.min.css))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target tom-select.bootstrap5.min.css.br)
  (deps (:input tom-select.bootstrap5.min.css))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target all.min.css.gz)
  (deps (:input all.min.css))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target all.min.css.br)
  (deps (:input all.min.css))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target css.css.gz)
  (deps (:input css.css))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target css.css.br)
  (deps (:input css.css))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target fanchart.css.gz)
  (deps (:input fanchart.css))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target fanchart.css.br)
  (deps (:input fanchart.css))
  (action
    (run brotli -f -q 11 %{input})))