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
from glinttools import (
    DeclInfo, Diagnostic, DiagnosticSeverity, Location, Token, Type, TypeKind,
    findDecl, getDiagnostics, getScopeAtPoint, getScopes, getTypeAtPoint, getValidSymbols, tokenize
)

from itertools import islice

from pygls.server import LanguageServer
from pygls.workspace import TextDocument
from lsprotocol import types

server = LanguageServer("glint-server", "v0.1")

def log(msg : str):
    server.show_message_log(msg)


_MARKDOWN_CHARACTERS_TO_ESCAPE = set(r"\`*_{}[]<>()#+-.!|")

def escaped_markdown(text: str) -> str:
    return "".join(
        f"\\{character}" if character in _MARKDOWN_CHARACTERS_TO_ESCAPE else character
        for character in text
    )

def find_many(instring: str, start_position: int, substrings):
    pat = re.compile('|'.join([re.escape(s) for s in substrings]))
    m = pat.search(instring[start_position:])
    if m is None:
        return -1
    else:
        return start_position + m.start()


def convert_severity(severity :DiagnosticSeverity):
    if severity == DiagnosticSeverity.Warning:
        return types.DiagnosticSeverity.Warning
    if severity == DiagnosticSeverity.Note:
        return types.DiagnosticSeverity.Hint
    return types.DiagnosticSeverity.Error

def publish_diagnostics(doc :TextDocument):
    lcc_diagnostics: list[Diagnostic] = getDiagnostics(doc.source)

    diagnostics = []
    for d in lcc_diagnostics:
        # Convert 1-indexed LCC line information to 0-indexed LSP line
        # information.
        start_line = max(d.location.line - 1, 0)
        end_line = start_line # TODO: Multi-line locations

        start_character = d.location.character
        end_character = start_character + d.location.length

        start = types.Position(start_line, start_character)
        end = types.Position(end_line, end_character)

        diagnostics.append(types.Diagnostic(
            message = d.message,
            severity = convert_severity(d.severity),
            range=types.Range(start=start, end=end)
        ))

    server.publish_diagnostics(
        uri=doc.uri,
        version=doc.version,
        diagnostics=diagnostics
    )

@server.feature(types.TEXT_DOCUMENT_HOVER)
def hover(ls: LanguageServer, params: types.HoverParams):
    pos = params.position

    document_uri = params.text_document.uri
    document = ls.workspace.get_text_document(document_uri)

    publish_diagnostics(document);

    try:
        line = document.lines[pos.line]
    except IndexError:
        return None

    try:
        selectedc = line[pos.character]
    except IndexError:
        return None

    # As well as getting the /token/ currently selected, it would also be
    # useful to have a getTypeAtPoint sort of thing (currently shoddily
    # implemented with findDecl). Ideally, what we would do is find the
    # selected /node/ given a source location, and return the type of that.

    tokens = tokenize(line)
    # for t in tokens:
    #     log(f".source '{t.source}', .byte_offset {t.location.byte_offset}, .length {t.location.length}, .line {t.location.line}, .character {t.location.character}")

    selected_token = next(
        (token for token in reversed(tokens)
         if token.location.byte_offset <= pos.character and pos.character < token.location.byte_offset + token.location.length),
        None
    )

    def token_pos_string(token :Token):
        return f"{pos.line+1}:{token.location.character}"

    # If there is nothing we can do to add info about the selected token/
    # context at point, print this.
    hover_content = ""

    # TODO: Use this once it's not so buggy...
    # loc = Location(document.offset_at_position(pos), 1, pos.line + 1, pos.character)
    # type_at_point = getTypeAtPoint(document.source, loc)
    # if (len(type_at_point.representation) != 0):
    #     hover_content += f" :!: {type_at_point.representation}"

    if selected_token:
        hover_content = f"{token_pos_string(selected_token)}: {escaped_markdown(selected_token.source)}"

        # If selected token is known (i.e. a keyword or a built-in type, add doc string)
        if (selected_token.source == "byte"):
            hover_content += "  | A built-in unsigned integer type of byte width 1"
        elif (selected_token.source == "void"):
            hover_content += "  | A built-in incomplete type, representing the absence of semantics"
        elif (selected_token.source == "int"):
            hover_content += "  | Signed integer type"
        elif (selected_token.source == "uint"):
            hover_content += "  | Unsigned integer type"
        elif (selected_token.source == "cshort"):
            hover_content += "  | C's `short` type"
        elif (selected_token.source == "cushort"):
            hover_content += "  | C's `unsigned short` type"
        elif (selected_token.source == "cint"):
            hover_content += "  | C's `int` type"
        elif (selected_token.source == "cuint"):
            hover_content += "  | C's `unsigned int` type"
        elif (selected_token.source == "clong"):
            hover_content += "  | C's `long` type"
        elif (selected_token.source == "culong"):
            hover_content += "  | C's `unsigned long` type"
        elif (selected_token.source == "clonglong"):
            hover_content += "  | C's `long long` type"
        elif (selected_token.source == "culonglong"):
            hover_content += "  | C's `unsigned long long` type"
        elif (selected_token.source == "print"):
            hover_content += "  | A built-in function-like that formats each argument into a dynamic array of byte, then prints that"
        elif (selected_token.source == "while"):
            hover_content += "  | A looping construct that iterates until a condition is false, in the form of `while <condition>, <body>;`"
        elif (selected_token.source == "for"):
            hover_content += "  | Range-based looping construct, utilizing .data and .size members"
        elif (selected_token.source == "cfor"):
            hover_content += "  | Looping construct mimicking C's `for`"
        elif (selected_token.source == "match"):
            hover_content += "  | Different control flow for each member of a sum type"
        elif (selected_token.source == "return"):
            hover_content += "  | Return to the caller"
        elif (selected_token.source.startswith("`") and selected_token.source.endswith("`")):
            hover_content += "  | Byte literal"

        decl_info = findDecl(document.source, selected_token.source)
        if decl_info.location.byte_offset != -1:         # confidence check
            if len(decl_info.type_representation) != 0:  # Glint C++ API fed us type of thing at point
                hover_content += f" : {decl_info.type_representation}"


    if len(hover_content) == 0:
        return None

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
    document = server.workspace.get_text_document(params.text_document.uri)

    try:
        line = document.lines[pos.line]
    except IndexError:
        return None

    selected_token = None
    # Move character selected backwards until we reach a token we can lookup...
    # Ideally, we would just be able to parse the lhs of the member access
    # (or everything left of '.' until ';' or ','), buuuut... yeah
    i = pos.character - 1
    tokens = tokenize(line)
    while not selected_token and i >= 0:
        selected_token = next(
            (token for token in tokens
             if token.location.byte_offset <= i and i < token.location.byte_offset + token.location.length),
            None
        )
        i -= 1

    if selected_token:
        completions = getValidSymbols(document.source)
        return types.CompletionList(
            is_incomplete=False, # completion list is complete
            items=(types.CompletionItem(
                label=completion
            ) for completion in completions)
        )

    completions = getValidSymbols(document.source)
    return types.CompletionList(
        is_incomplete=False, # completion list is complete
        items=(types.CompletionItem(
            label=completion
        ) for completion in completions)
    )

if __name__ == "__main__":
    server.start_io()
