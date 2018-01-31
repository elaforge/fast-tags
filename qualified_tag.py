# Replace the ^] follow tag command with one that expands a word without
# modifying iskeyword.  Setting iskeyword is a lot simpler, but it also
# changes the behavoiour of a lot of vim commands, such as w, which is too
# confusing for me.  This can also do clever things like allow ' in
# identifiers, but strip quotes from haddock references like 'A.B'.
#
# I install it into ~/.vim/py and enable like this:
#
# if has('python')
#     py import sys, os, vim
#     py sys.path.insert(0, os.environ['HOME'] + '/.vim/py')
#     py import qualified_tag
#     nmap <silent> <c-]> :py qualified_tag.tag_word(vim)<cr>
# endif

def tag_word(vim):
    word = get_word(vim)
    vim.command('tag ' + word)

def get_word(vim):
    (row, col) = vim.current.window.cursor
    line = vim.current.buffer[row-1]
    return expand_word(line, col)

def expand_word(line, col):
    # This is broken for unicode, but vim gives the col index in bytes, and
    # python gives no way to convert byte index to unicode index, short of
    # decoding myself.  And even if I did, python uses UTF16 so it would
    # still be wrong for unicode above U+FFFF.
    start = col
    while start > 0 and is_keyword(line[start-1]):
        start -= 1
    end = col
    while end < len(line) and is_keyword(line[end]):
        end += 1
    word = line[start:end]
    if word and (word[0] == word[-1] == "'" or word[0] == word[-1] == '"'):
        # Make tags on haddock references like 'A.B' work.
        return word[1:-1]
    else:
        return word

def is_keyword(c):
    return c.isalnum() or c in "'._"
