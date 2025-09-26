# A Language Server Implementation for the Glint Programming Language using pygls
#
# pip install pygls
#
# To use in Emacs (replace path to `glintd.py`):
# (add-to-list 'eglot-server-programs
#  '('glint-ts-mode :language-id "glint")
#   . ("python3" "./editor/glint/glintd.py"))

from pygls.server import LanguageServer
from lsprotocol import types

server = LanguageServer("glint-server", "v0.1")

# TODO: Make this more complex
def tokenize(source: str):
    results = []
    current_pos = 0
    while True:
        next_delimiter_pos = source.find(" ", current_pos)
        if next_delimiter_pos == -1:
            results.append((source[current_pos:].strip().strip(",;"), current_pos))
            break

        # Skip delimiter-only results
        if next_delimiter_pos == current_pos:
            current_pos = next_delimiter_pos + 1
            continue

        results.append((source[current_pos:next_delimiter_pos].strip().strip(",;"), current_pos))
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

    tokens = tokenize(line)

    selected_token = next(
        (token for token in reversed(tokens)
         if token[1] <= pos.character and pos.character < token[1] + len(token[0])),
        None
    )

    # TODO: if selected_token is identifier, find declaration with that
    # identifier and print it's type, if possible

    hover_content = f"The tokens you are hovering over is: {tokens}, selectedc={selectedc}, selected_token={selected_token}"

    hover_flat = "".join(hover_content)

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
    document = server.workspace.get_text_document(params.text_document.uri)
    current_line = document.lines[params.position.line].strip()

    if not current_line.endswith("hello."):
        return []

    return [
        types.CompletionItem(label="world"),
        types.CompletionItem(label="friend"),
    ]


if __name__ == "__main__":
    server.start_io()
