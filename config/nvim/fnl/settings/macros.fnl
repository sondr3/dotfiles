(fn set! [name val]
  `(tset vim.opt ,(tostring name) ,val))

(fn set-true! [...]
  `(do
     ,(unpack (icollect [_ name (ipairs [...])]
                `(tset vim.opt ,(tostring name) true)))))

(fn set-false! [...]
  `(do
     ,(unpack (icollect [_ name (ipairs [...])]
                `(tset vim.opt ,(tostring name) false)))))

(fn multi-set! [...]
  (fn groups [...]
    (match [...]
      [name val & rest] [(set! name val) (unpack (groups (unpack rest)))]
      _ []))

  `(do
     ,(unpack (groups ...))))

{:set! multi-set! : set-true! : set-false!}
