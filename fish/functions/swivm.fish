function swivm
    bash -c "source ~/.swivm/swivm.sh && LDFLAGS=\"-L/opt/homebrew/opt/libarchive/lib\" CPPFLAGS=\"-I/opt/homebrew/opt/libarchive/include\" OPENSSL_ROOT_DIR=\"/opt/homebrew/opt/openssl@1.1\" swivm $argv"
end
