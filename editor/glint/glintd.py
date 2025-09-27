# A Language Server Implementation for the Glint Programming Language using pygls
#
# pip install pygls
#
# To use in Emacs (replace path to `glintd.py`):
# (add-to-list 'eglot-server-programs
#  '('glint-ts-mode :language-id "glint")
#   . ("python3" "./editor/glint/glintd.py"))

import re
from pygls.server import LanguageServer
from lsprotocol import types

server = LanguageServer("glint-server", "v0.1")

_MARKDOWN_CHARACTERS_TO_ESCAPE = set(r"\`*_{}[]<>()#+-.!|")

def escaped_markdown(text: str) -> str:
    return "".join(
        f"\\{character}" if character in _MARKDOWN_CHARACTERS_TO_ESCAPE else character
        for character in text
    )

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

    # In Emacs syntax... "out\s*:\s*\\([^;=]+\\)"
    pattern = f"{re.escape(selected_token[0])}\\s*:\\s*(?P<type>[^;=]+)"
    found = re.search(pattern, "".join(document.lines), re.MULTILINE)
    found_type = ""
    if found:
        found_type = found.group("type").strip()

    # Hack to remove extra types surrounding found type (usually parameters)...
    # TODO: Just parse an actual type (either using bindings to the compiler
    # itself, or calling the compiler executable).
    if not "(" in found_type:
        found_type = found_type.strip().strip(")")
        if "," in found_type:
            found_type = found_type.split(",")[0]

    # hover_content = f"The tokens you are hovering over is: {tokens}, selectedc={selectedc}, selected_token={selected_token}"
    hover_content = f"{pos.line}:{selected_token[1]}: {escaped_markdown(selected_token[0])}"
    if found:
        hover_content += f" : **{escaped_markdown(found_type)}**"

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
    pos = params.position
    document_uri = params.text_document.uri
    document = ls.workspace.get_text_document(document_uri)

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
