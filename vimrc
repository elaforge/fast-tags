" Add these to your vimrc to automatically keep the tags file up to date.
" Unfortunately silent means the errors look a little ugly, I suppose I could
" capture those and print them out with echohl WarningMsg.
au BufWritePost *.hs            silent !fast-tags %
au BufWritePost *.hsc           silent !fast-tags %
