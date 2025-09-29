# A Language Server Implementation for the Glint Programming Language using pygls
#
# pip install pygls
#
# To use in Emacs (replace path to `glintd.py`):
# (add-to-list 'eglot-server-programs '(glint-ts-mode . ("python3" "/home/lens_r/Programming/play/LensorCompilerCollection/editor/glint/glintd.py")))

# TODO:
# - Implement "go to"s and "find" server features (see https://pygls.readthedocs.io/en/latest/servers/examples/goto.html)


import re

# You have to build this yourself, like, with a C++ compiler
from glinttools import getCompletions, findDecl

from itertools import islice

from pygls.server import LanguageServer
from lsprotocol import types

server = LanguageServer("glint-server", "v0.1")


_MARKDOWN_CHARACTERS_TO_ESCAPE = set(r"\`*_{}[]<>()#+-.!|")

def escaped_markdown(text: str) -> str:
    return "".join(
        f"\\{character}" if character in _MARKDOWN_CHARACTERS_TO_ESCAPE else character
        for character in text
    )

def find_many(instring: str, start_position: int, substrings):
    pat = re.compile('|'.join([re.escape(s) for s in substrings]))
    match = pat.search(instring[start_position:])
    if match is None:
        return -1
    else:
        return start_position + match.start()


# TODO: Make this more complex
def tokenize(source: str):
    results = []
    current_pos = 0
    while True:
        next_delimiter_pos = find_many(source, current_pos, [' ', ';', ',', '(', ')', '\n', ':', '<', '>', '=', '-', '*', '@', '{', '}', '!', '%', '~', '^', '|', '&'])

        if next_delimiter_pos == -1:
            results.append((source[current_pos:], current_pos))
            break

        token_source = source[current_pos:next_delimiter_pos]

        if token_source == ' ' or token_source == '\n':
            current_pos = next_delimiter_pos + 1
            break

        results.append((token_source, current_pos))
        current_pos = next_delimiter_pos + 1

    return results


@server.feature(types.TEXT_DOCUMENT_HOVER)
def hover(ls: LanguageServer, params: types.HoverParams):
    pos = params.position

    document_uri = params.text_document.uri
    document = ls.workspace.get_text_document(document_uri)

    try:
        line = document.lines[pos.line]
    except IndexError:
        return None

    try:
        selectedc = line[pos.character]
    except IndexError:
        return None

    selected_token = next(
        (token for token in reversed(tokenize(line))
         if token[1] <= pos.character and pos.character < token[1] + len(token[0])),
        None
    )
    def token_pos_string(token):
        return f"{pos.line}:{token[1]}"

    # If there is nothing we can do to add info about the selected token/
    # context at point, print this.
    # hover_content = f"{pos.line}:{pos.character}"
    hover_content = ""
    if selected_token:
        context_source = "".join(document.lines[:pos.line])
        # TODO: if isIdentifier(selected_token)
        decl_info = findDecl(context_source, selected_token[0])
        if decl_info.decl_location.byte_offset >= 0:
            decl_source = context_source[decl_info.location.byte_offset:decl_info.location.byte_offset+decl_info.location.length]
            hover_content = f"{token_pos_string(selected_token)}: {selected_token[0]} : **{escaped_markdown(decl_info.type_representation)}**"
        else:
            hover_content = f"{token_pos_string(selected_token)}: {escaped_markdown(selected_token[0])}"

    if len(hover_content) != 0:
        return types.Hover(
            contents=types.MarkupContent(
                kind=types.MarkupKind.Markdown,
                value=hover_content,
            ),
            range=types.Range(
                start=types.Position(line=pos.line, character=0),
                end=types.Position(line=pos.line + 1, character=0),
            ),
        )
    else:
        return None

@server.feature(
    types.TEXT_DOCUMENT_COMPLETION,
    types.CompletionOptions(trigger_characters=["."]),
)
def completions(params: types.CompletionParams):
    document = params.text_document

    return []

    try:
        line = document.lines[pos.line]
    except IndexError:
        return []

    if not line.endswith("hello."):
        return []

    return [
        types.CompletionItem(label="world"),
        types.CompletionItem(label="friend"),
    ]


if __name__ == "__main__":
    server.start_io()
