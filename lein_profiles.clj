{:user {:plugins [[lein-try "0.3.2"]
                  [lein-pprint "1.1.1"]
                  ;[lein-ancient "0.6.10" :exclusions [commons-logging]]
                  [lein-checkouts "1.1.0"]
                  [cider/cider-nrepl "0.15.1-SNAPSHOT"]
                  [lein-exec "0.3.5"]
                  ;[lein-depdocs "1.0.1-SNAPSHOT"]
                  ]

        ;:dependencies [[com.cemerick/piggieback "0.2.1"]
                       ;[figwheel-sidecar "0.5.9"]]
        ;:repl-options {:init (require 'figwheel-sidecar.repl-api)
                       ;:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

        :signing {:gpg-key "8DC9A386"}}}
