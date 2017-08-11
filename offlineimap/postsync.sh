#!/bin/bash

notmuch new
notmuch tag --batch <<EOF
+inbox tag:new and folder:Gmail/INBOX
EOF
