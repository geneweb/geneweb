
(rule
  (target chart.umd.min.js.gz)
  (deps (:input chart.umd.min.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target chart.umd.min.js.br)
  (deps (:input chart.umd.min.js))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target bootstrap.bundle.min.js.gz)
  (deps (:input bootstrap.bundle.min.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target bootstrap.bundle.min.js.br)
  (deps (:input bootstrap.bundle.min.js))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target datatables.min.js.gz)
  (deps (:input datatables.min.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target datatables.min.js.br)
  (deps (:input datatables.min.js))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target regex.min.js.gz)
  (deps (:input regex.min.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target regex.min.js.br)
  (deps (:input regex.min.js))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target checkdata.min.js.gz)
  (deps (:input checkdata.min.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target checkdata.min.js.br)
  (deps (:input checkdata.min.js))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target fanchart.min.js.gz)
  (deps (:input fanchart.min.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target fanchart.min.js.br)
  (deps (:input fanchart.min.js))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target checkdata.js.gz)
  (deps (:input checkdata.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target checkdata.js.br)
  (deps (:input checkdata.js))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target p_mod.min.js.gz)
  (deps (:input p_mod.min.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target p_mod.min.js.br)
  (deps (:input p_mod.min.js))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target copylink.js.gz)
  (deps (:input copylink.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target copylink.js.br)
  (deps (:input copylink.js))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target rlm_builder.js.gz)
  (deps (:input rlm_builder.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target rlm_builder.js.br)
  (deps (:input rlm_builder.js))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target fanchart.js.gz)
  (deps (:input fanchart.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target fanchart.js.br)
  (deps (:input fanchart.js))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target p_mod.js.gz)
  (deps (:input p_mod.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target p_mod.js.br)
  (deps (:input p_mod.js))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target tom-select.complete.min.js.gz)
  (deps (:input tom-select.complete.min.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target tom-select.complete.min.js.br)
  (deps (:input tom-select.complete.min.js))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target jquery.min.js.gz)
  (deps (:input jquery.min.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target jquery.min.js.br)
  (deps (:input jquery.min.js))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target maphilight.min.js.gz)
  (deps (:input maphilight.min.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target maphilight.min.js.br)
  (deps (:input maphilight.min.js))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target line.js.gz)
  (deps (:input line.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target line.js.br)
  (deps (:input line.js))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target relationmatrix.js.gz)
  (deps (:input relationmatrix.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target relationmatrix.js.br)
  (deps (:input relationmatrix.js))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target clearinput.js.gz)
  (deps (:input clearinput.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target clearinput.js.br)
  (deps (:input clearinput.js))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target relationmatrix.min.js.gz)
  (deps (:input relationmatrix.min.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target relationmatrix.min.js.br)
  (deps (:input relationmatrix.min.js))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target autosize.min.js.gz)
  (deps (:input autosize.min.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target autosize.min.js.br)
  (deps (:input autosize.min.js))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target init_gallery.min.js.gz)
  (deps (:input init_gallery.min.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target init_gallery.min.js.br)
  (deps (:input init_gallery.min.js))
  (action
    (run brotli -f -q 11 %{input})))
(rule
  (target html5sortable.min.js.gz)
  (deps (:input html5sortable.min.js))
  (action
    (run gzip -9 -k -f %{input})))
(rule
  (target html5sortable.min.js.br)
  (deps (:input html5sortable.min.js))
  (action
    (run brotli -f -q 11 %{input})))