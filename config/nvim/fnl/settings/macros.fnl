(fn set! [name val]
  `(tset vim.opt ,(tostring name) ,val))

(fn multi-set! [...]
  (fn ret [name ?val rest f]
    [(set! name ?val) (unpack (f (unpack rest)))])

  (fn groups [...]
    (match [...]
      (where [& rest] (= (length rest) 0)) []
      (where [name val & rest] (not (sym? val))) (ret name val rest groups)
      [name & rest] (ret name nil rest groups)))

  (let [opts (groups ...)]
    (if (> (length opts) 1)
        `(do
           ,(unpack opts))
        (unpack opts))))

{:set! multi-set! }
