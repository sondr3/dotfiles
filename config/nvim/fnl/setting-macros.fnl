(fn set! [name ?val]
  (let [n (tostring name)
	opt (if (string.match n "^no") (string.sub n 3) n)
        v (if ?val ?val (not (= (string.sub n 1 (string.len "no")) "no")))]
    `(tset vim.opt ,opt ,v)))

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
