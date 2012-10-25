setlocal makeprg=xcodebuild\ -configuration\ Debug
setlocal errorformat=%f:%l:%c:%.%#\ error:\ %m,%f:%l:%c:%.%#\ warning:\ %m,%-G%.%#
setlocal noexpandtab

command! -buffer OpenFileInXCode silent !xed %
command! -buffer RebuildTags exec "!~/bin/ctags -R --language-force=ObjectiveC ."
