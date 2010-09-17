#!/bin/sh
CLASSPATH="/usr/local/Cellar/nailgun/0.7.1/nailgun-0.7.1.jar:$HOME/Languages/Clojure/clojure.jar:$HOME/Languages/Clojure/clojure-contrib.jar:$HOME/.vim/bundle/vim-clojure/server-2.2.0-SNAPSHOT.jar"

exec java -cp $CLASSPATH vimclojure.nailgun.NGServer 127.0.0.1 "$@"
