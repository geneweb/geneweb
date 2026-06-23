
(rule
  (alias install)
  (package geneweb)
  (targets all.min.css.gz all.min.css.br)
  (deps (:input all.min.css))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (alias install)
  (package geneweb)
  (targets bootstrap.min.css.gz bootstrap.min.css.br)
  (deps (:input bootstrap.min.css))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (alias install)
  (package geneweb)
  (targets css.css.gz css.css.br)
  (deps (:input css.css))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (alias install)
  (package geneweb)
  (targets datatables.min.css.gz datatables.min.css.br)
  (deps (:input datatables.min.css))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (alias install)
  (package geneweb)
  (targets fanchart.css.gz fanchart.css.br)
  (deps (:input fanchart.css))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (alias install)
  (package geneweb)
  (targets tom-select.bootstrap5.min.css.gz tom-select.bootstrap5.min.css.br)
  (deps (:input tom-select.bootstrap5.min.css))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))
