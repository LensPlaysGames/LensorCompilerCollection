module Parser;

import SimpleFile;

Location : struct {
    begin: usz;
    end: usz;
};

TokenKind : enum {
    NONE;
    INTEGER;
    PLUS;
    HYPHEN;
    ASTERISK;
    SLASH;
};

binop_precedence: bool(k: TokenKind) {
    if k = TokenKind.PLUS or k = TokenKind.HYPHEN
        return 1;
    if k = TokenKind.ASTERISK or k = TokenKind.SLASH
        return 2;
    return 0;
};

Token : struct {
    kind : TokenKind;
    location: Location;
};

is_digit : bool(c: byte) {
    return c = b"0" or c = b"1"
        or c = b"2" or c = b"3"
        or c = b"4" or c = b"5"
        or c = b"6" or c = b"7"
        or c = b"8" or c = b"9";
};

is_whitespace : bool(c: byte) {
    return c = b" " or c = b"\n"
        or c = b"\r" or c = b"\t"
        or c = b"\f" or c = b"\v";
};

lex : Token(input: [byte].ref, start: usz.ref) {
    if not input.size or start >= input.size
        return Token TokenKind.NONE (Location 0 0);

    ;; Skip whitespace
    while is_whitespace(@input[start]) {
        ++start;
        if start >= input.size
            return Token TokenKind.NONE (Location 0 0);
    }

    loc :: (Location start start);
    c :: @input[start];
    if is_digit(c) {
        while is_digit(@input[start]) {
            ++start;
            if start >= input.size break;
        }
        loc.end := start;
        return Token TokenKind.INTEGER loc;
    } else if c = b"+" {
        ++start;
        loc.end := start;
        return Token TokenKind.PLUS loc;
    } else if c = b"-" {
        ++start;
        loc.end := start;
        return Token TokenKind.HYPHEN loc;
    } else if c = b"*" {
        ++start;
        loc.end := start;
        return Token TokenKind.ASTERISK loc;
    } else if c = b"/" {
        ++start;
        loc.end := start;
        return Token TokenKind.SLASH loc;
    } else {
        print "Unrecognized byte in input";
        print c;
        print c"\n";
    }

    print "UNREACHABLE in lexer";
    return Token TokenKind.NONE (Location 0 0);
}

NodeKind : enum {
    NONE,
    INTEGER,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    ROOT,
};

binop_token_kind_to_node_kind: NodeKind(k: TokenKind) {
    if k = TokenKind.PLUS return NodeKind.ADD;
    if k = TokenKind.HYPHEN return NodeKind.SUBTRACT;
    if k = TokenKind.ASTERISK return NodeKind.MULTIPLY;
    if k = TokenKind.SLASH return NodeKind.DIVIDE;
    return NodeKind.NONE;
}

Node : struct {
    kind: NodeKind;
    children: [Node];
};

parse : Node(input: [byte].ref, prec: uint) {
    out :: Node NodeKind.ROOT !{};

    offset : usz 0;

    tok :: lex input, offset;
    while tok.kind != TokenKind.NONE {
        lhs : Node;

        ;; Ensure we are at a token that may start an expression.
        ;; Expression starters: PLUS, HYPHEN, INTEGER

        ;; TODO: Unary prefix + and -
        if tok.kind = TokenKind.INTEGER {
            lhs.kind := NodeKind.INTEGER;
            ;; Eat integer.
            tok := lex input, offset;
        } else {
            print "Expected expression starting token";
            return out;
        }

        ;; Check if token is a binary operator, AND that that binary operator's
        ;; precedence is >= to the current precedence. If so, create a new binary
        ;; expression with what we already had parsed as the lhs, and then parse
        ;; what's next into the rhs.
        while true {
            prec :: binop_precedence tok.kind;
            if prec < current_precedence break;

            ;; Eat binary operator
            op :: tok.kind;
            tok := lex input, offset;

            rhs :: parse(input, prec);

            lhs := Node (binop_token_kind_to_node_kind op) !{lhs rhs};
        }

        ;; Commit node to parse tree.
        out.children += lhs;

        ;; Advance lexer.
        tok := lex input, offset;
    };

    return out;
};
