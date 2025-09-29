# A Language Server Implementation for the Glint Programming Language using pygls
#
# pip install pygls
#
# To use in Emacs (replace path to `glintd.py`):
# (add-to-list 'eglot-server-programs '(('glint-ts-mode) . ("python3" "/home/lens_r/Programming/play/LensorCompilerCollection/editor/glint/glintd.py")))

import re

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
        next_delimiter_pos = find_many(source, current_pos, [' ', ';', ',', '(', ')', '\n'])

        if next_delimiter_pos == -1:
            results.append((source[current_pos:next_delimiter_pos].strip().strip(",;"), current_pos))
            break

        # Skip delimiter-only results
        if next_delimiter_pos == current_pos:
            current_pos = next_delimiter_pos + 1
            continue

        # Strip source of whitespace and expression separators
        token_source = source[current_pos:next_delimiter_pos].strip().strip(",;")

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

    # context_source = "".join(document.lines[:pos.line])
    # tokenized_lines = ((i, tokenize(line)) for i,line in enumerate(document.lines[:pos.line]))

    selected_token = next(
        (token for token in reversed(tokenize(line))
         if token[1] <= pos.character and pos.character < token[1] + len(token[0])),
        None
    )

    hover_content = f"{pos.line}:{pos.character}"
    if selected_token:
        # hover_content = f"The tokens you are hovering over is: {tokenize(line)}, selectedc={selectedc}, selected_token={selected_token}"
        hover_content = f"{pos.line}:{selected_token[1]}: {escaped_markdown(selected_token[0])}"
        # TODO: find first occurences of the selected token and use them to do something meaningful
        # first_occurence = next(
        #     (token for token in (tokenized_line for tokenized_line in islice(tokenized_lines, 0, pos.line - 1))
        #      if token[0] == selected_token[0]),
        #     None
        # )
        # if first_occurence:
        #     hover_content += f" declared at byte offset {first_occurence[1]}"

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

@server.feature(
    types.TEXT_DOCUMENT_COMPLETION,
    types.CompletionOptions(trigger_characters=["."]),
)
def completions(params: types.CompletionParams):
    document = params.text_document

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
