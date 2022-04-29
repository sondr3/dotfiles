(let [hm (require :heime)
      utils (require :heime.utils)
      template "
prefix=/home/sondre/.local
save-prefix=''
save-exact=true
//registry.npmjs.org/:_authToken={{ npmToken }}"
      run (fn [ctx data]
            (ctx:write_string (ctx:home_file :.npmrc)
                              (utils:template template data.data)))]
  (hm.task {:name :node :description "setup and configure node" : run}))