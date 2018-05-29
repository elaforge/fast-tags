# Replace the ^] follow tag command with one that expands a word without
# modifying iskeyword.  Setting iskeyword is a lot simpler, but it also
# changes the behavoiour of a lot of vim commands, such as w, which is too
# confusing for me.  This can also do clever things like allow ' in
# identifiers, but strip quotes from haddock references like 'A.B'.
#
# This assumes you are using fast-tags --fully_qualified, since it adds
# full module qualification to qualified tags, after renaming them based on
# the 'import ... as XYZ' lines in this file.
#
# I install it into ~/.vim/py and enable like this:
#
# In global .vimrc:
#
# if has('python')
#     py import sys, os, vim
#     py sys.path.insert(0, os.environ['HOME'] + '/.vim/py')
# endif
#
# In your haskell specific vimrc:
#
# if has('python')
#     py import qualified_tag
#     nnoremap <buffer> <silent> <c-]> :py qualified_tag.tag_word(vim)<cr>
# endif
#
# If you use 'filetype', you can do:
#
# autocmd FileType haskell nnoremap ...

import sys, re

def tag_word(vim):
    word = get_word(vim)
    qual_to_module = get_qualified_imports(vim.current.buffer)
    tag = guess_tag(qual_to_module, word)
    # If I can't find a qualified target, try it without the qualification.
    # It might be a re-export from another module.
    found = has_target(vim, tag)
    if not found and '.' in tag:
        tag = tag.split('.')[-1]
        found = has_target(vim, tag)
    if found:
        vim.command('tag ' + tag)
    else:
        # I'd use echoerr, but that causes a big messy python traceback.
        vim.command('echohl ErrorMsg | echo %r | echohl None' %
            ('tag not found: ' + tag,))

def has_target(vim, tag):
    return bool(vim.eval('taglist(%r, expand("%%"))' % ('^' + tag + '$',)))

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
    # Strip punctuation in case it's in a sentence in a comment.
    word = word.strip('.')
    if word and (word[0] == word[-1] == "'" or word[0] == word[-1] == '"'):
        # Make tags on haddock references like 'A.B' work.
        return word[1:-1]
    else:
        return word

def is_keyword(c):
    return c.isalnum() or c in "'._"

def guess_tag(qual_to_module, word):
    if '.' not in word:
        return word
    components = word.split('.')
    qual = '.'.join(components[:-1])
    if qual in qual_to_module:
        return qual_to_module[qual] + '.' + components[-1]
    else:
        return word

def get_qualified_imports(lines):
    imports = get_imports(lines)
    qual_to_module = {}
    for m in re.finditer(r'\b([A-Za-z0-9.]+)\s+as\s+([A-Za-z00-9.]+)$',
            imports, re.MULTILINE):
        qual_to_module[m.group(2)] = m.group(1)
    return qual_to_module

def get_imports(lines):
    imports = []
    in_imports = False
    for line in lines:
        if line.startswith('import'):
            in_imports = True
        if in_imports:
            if finished_imports(line):
                break
            imports.append(line)
    return '\n'.join(imports)

def finished_imports(line):
    # Hacky heuristic to see if I'm past the import block.
    # If it starts with non-space but not 'import', then it's probably
    # not an import or import continuation.
    return line and not line.startswith('import') and not line[0].isspace()

def test():
    fn, tag = sys.argv[1:]
    lines = open(fn).read().split('\n')
    qual_to_module = get_qualified_imports(lines)
    print qual_to_module
    print guess_tag(qual_to_module, tag)

if __name__ == '__main__':
    test()
