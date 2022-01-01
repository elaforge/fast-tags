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
#     nnoremap <buffer> <silent> <c-Bslash> :py qualified_tag.tag_preview(vim)<cr>
# endif
#
# Substitute has('python3') and 'py3' if your vim uses python3.
#
# This also adds ^\ to preview a tag in a quickfix window.  I use this to get
# type signatures, haddocks, and parameter names, or data declarations.
#
# If you use 'filetype', you can do:
#
# autocmd FileType haskell nnoremap ...

from __future__ import print_function
import sys, re


### preview tag


def tag_preview(vim):
    """This abuses vim's quickfix window to show a preview of where the
        tag will go.
    """
    matches, _word = get_tag(vim)
    if not matches:
        return
    match = matches[0]
    linenumber = int(match['cmd'])
    i, lines = get_preview(match['kind'], match['filename'], linenumber)
    # This is sketchy, since it only works if python repr is valid viml, but
    # "sketchy" is normal for vim.  Speaking of which, for some unknown
    # reason, | is a special character when appearing in expressions, but
    # only for :cgetexpr, not for e.g. :echo.
    vim.command('cgetexpr ' + repr(lines).replace('|', '\\|'))
    vim.command('copen ' + repr(len(lines)))
    vim.command('cc ' + str(i+1))
    # TODO Set title to file:linenumber, or file.match['name']
    # Go to previous window, to get out of the qf window.
    vim.command('wincmd p')

def get_preview(kind, fname, linenumber):
    fp = open(fname)
    lines = fp.readlines()
    fp.close()
    before, after = kind_context.get(kind, context_generic)(
        lines, linenumber-1)
    return len(before), list(map(str.rstrip, before + after))

def context_type(lines, i):
    before = back_until_haddock(lines, i)
    after = forward_until(lines, i, 5, ['}', 'deriving'], dedent=True)
    return before, after

def context_function(lines, i):
    # The tag should be on the line with ::, but maybe not.
    before = back_until_haddock(lines, i)
    after = forward_until(lines, i, 3, ['='])
    return before, after

def context_generic(lines, i):
    return lines[i-1 : i], forward_until(lines, i, 3, [], dedent=True)

def truncate(maxlen, xs):
    if len(xs) > maxlen:
        return xs[:maxlen] + ['...']
    else:
        return xs

def back_until_haddock(lines, start):
    maxlines = 5
    i = start
    while i > 0:
        if not lines[i].strip():
            i += 1
            break
        elif lines[i].startswith(('-- |', '{- |')):
            break
        else:
            i -= 1
    return truncate(maxlines, lines[i : start])

def forward_until(lines, start, maxlines, infixes, dedent=False):
    i = start
    indented = False
    while i < len(lines) and i - start < maxlines:
        if not lines[i].strip():
            i -= 1
            break
        elif dedent and indented and not lines[i][0].isspace():
            # A dedented after an indent must be the next definition.
            i -= 1
            break
        else:
            ws = lines[i].split()
            if any(infix in ws for infix in infixes):
                break
            else:
                if lines[i][0].isspace():
                    indented = True
                i += 1
    return lines[start : i+1]

kind_context = {
    'm': context_generic, # module
    'f': context_function, # function
    'c': context_type, # class
    't': context_type, # type
    'C': context_type, # constructor
    'o': context_function, # function
    'p': context_generic, # pattern * = ...
    'F': context_generic, # type family * = ...
    'D': context_generic, # define
}

### follow tag

def tag_word(vim):
    matches, word = get_tag(vim)
    if not matches:
        # I'd use echoerr, but that causes a big messy python traceback.
        vim.command('echohl ErrorMsg | echo %r | echohl None'
            % ('tag not found: ' + word,))
    else:
        vim.command('tag ' + word)

def get_tag(vim):
    """Get the word under the cursor, return
        [str] - Exact names for tags matching the word.
        str - Word from the cursor.
    """
    word = get_word(vim)
    qual_to_module = get_qualified_imports(vim.current.buffer)
    tag = guess_tag(qual_to_module, word)
    # If I can't find a qualified target, try it without the qualification.
    # It might be a re-export from another module.
    matches = has_target(vim, tag)
    if not matches and '.' in tag:
        tag = tag.split('.')[-1]
        matches = has_target(vim, tag)
    return matches, tag

def has_target(vim, tag):
    return vim.eval('taglist(%r)' % ('^' + tag + '$',))

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
    # If column 0 isn't 'import' or a space or a comment.
    return line and not (
        line.startswith('import') or line[0].isspace()
        or line.startswith('--')
    )

def test():
    fn, tag = sys.argv[1:]
    lines = open(fn).read().split('\n')
    qual_to_module = get_qualified_imports(lines)
    print(qual_to_module)
    print(guess_tag(qual_to_module, tag))

if __name__ == '__main__':
    test()
