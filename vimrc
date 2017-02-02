" Add these to your vimrc to automatically keep the tags file up to date.
" Unfortunately silent means the errors look a little ugly, I suppose I could
" capture those and print them out with echohl WarningMsg.
au BufWritePost *.hs            silent !init-tags %
au BufWritePost *.hsc           silent !init-tags %

" If you use qualified tags, then you have to change iskeyword to include
" a dot.  Unfortunately, that affects a lot of other commands, such as
" w, and \< \> regexes used by * and #.  For me, this is confusing because
" it's inconsistent with vim keys everywhere else.
" This binding temporarily modifies iskeyword just for the ^] command.
nnoremap <silent> <c-]> :setl iskeyword=@,_,.,48-57,39<cr><c-]>
    \:setl iskeyword=@,48-57,_,192-255<cr>
