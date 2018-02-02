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
    vim.command('tag ' + guess_tag(qual_to_module, word))

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
    for line in lines:
        if finished_imports(line):
            break
        imports.append(line)
    return '\n'.join(imports)

def finished_imports(line):
    # Hacky heuristic to see if I'm past the import block.
    # If it starts with non-space but not 'import', then it's probably
    # not an import or import continuation.  Look for :: just for good
    # measure, since I don't think that can show up in an import line.
    return (line and not line.startswith('import')
        and not line[0].isspace() and '::' in line)

def test():
    fn, tag = sys.argv[1:]
    lines = open(fn).read().split('\n')
    qual_to_module = get_qualified_imports(lines)
    print qual_to_module
    print guess_tag(qual_to_module, tag)

if __name__ == '__main__':
    test()
