
(rule
  (package geneweb)
  (targets autosize.min.js.gz autosize.min.js.br)
  (deps (:input autosize.min.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets bootstrap.bundle.min.js.gz bootstrap.bundle.min.js.br)
  (deps (:input bootstrap.bundle.min.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets chart.umd.min.js.gz chart.umd.min.js.br)
  (deps (:input chart.umd.min.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets checkdata.js.gz checkdata.js.br)
  (deps (:input checkdata.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets checkdata.min.js.gz checkdata.min.js.br)
  (deps (:input checkdata.min.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets clearinput.js.gz clearinput.js.br)
  (deps (:input clearinput.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets cousins.js.gz cousins.js.br)
  (deps (:input cousins.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets cousins.min.js.gz cousins.min.js.br)
  (deps (:input cousins.min.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets dagSvg.js.gz dagSvg.js.br)
  (deps (:input dagSvg.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets datatables.min.js.gz datatables.min.js.br)
  (deps (:input datatables.min.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets fanchart.js.gz fanchart.js.br)
  (deps (:input fanchart.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets fanchart.min.js.gz fanchart.min.js.br)
  (deps (:input fanchart.min.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets html5sortable.min.js.gz html5sortable.min.js.br)
  (deps (:input html5sortable.min.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets jquery.min.js.gz jquery.min.js.br)
  (deps (:input jquery.min.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets line.js.gz line.js.br)
  (deps (:input line.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets maphilight.js.gz maphilight.js.br)
  (deps (:input maphilight.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets maphilight.min.js.gz maphilight.min.js.br)
  (deps (:input maphilight.min.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets notes_upd_gallery.js.gz notes_upd_gallery.js.br)
  (deps (:input notes_upd_gallery.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets notes_upd_gallery.min.js.gz notes_upd_gallery.min.js.br)
  (deps (:input notes_upd_gallery.min.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets p_mod.js.gz p_mod.js.br)
  (deps (:input p_mod.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets p_mod.min.js.gz p_mod.min.js.br)
  (deps (:input p_mod.min.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets permalink.js.gz permalink.js.br)
  (deps (:input permalink.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets permalink.min.js.gz permalink.min.js.br)
  (deps (:input permalink.min.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets person_picker.js.gz person_picker.js.br)
  (deps (:input person_picker.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets person_picker.min.js.gz person_picker.min.js.br)
  (deps (:input person_picker.min.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets regex.min.js.gz regex.min.js.br)
  (deps (:input regex.min.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets relationmatrix.js.gz relationmatrix.js.br)
  (deps (:input relationmatrix.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets relationmatrix.min.js.gz relationmatrix.min.js.br)
  (deps (:input relationmatrix.min.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets rlm_builder.js.gz rlm_builder.js.br)
  (deps (:input rlm_builder.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets tom-select.complete.min.js.gz tom-select.complete.min.js.br)
  (deps (:input tom-select.complete.min.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets updateField.js.gz updateField.js.br)
  (deps (:input updateField.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))

(rule
  (package geneweb)
  (targets updateField.min.js.gz updateField.min.js.br)
  (deps (:input updateField.min.js))
  (action
    (concurrent
      (run gzip -9 -k -f %{input})
      (run brotli -q 11 %{input}))))
