import re

# Token types
TOKEN_TYPES = {
    'KEYWORD': r'\b(?:Algorithm|Const|Var|Debut|Fin|Si|Sinon|Finsi|Pour|jusqua|faire|lire|ecrire)\b',
    'IDENTIFIER': r'\b[A-Za-z_][A-Za-z0-9_]*\b',
    'NUMBER': r'\b\d+(?:\.\d+)?\b',
    'STRING': r'\".*?\"',
    'CHAR': r"\'.?\'",
    'ASSIGN': r':=',
    'OPERATOR': r'[+\-*/]',
    'COMPARATOR': r'[<>!=]=?|==',
    'DELIMITER': r'[;:,]',
    'PAREN': r'[()]',
    'BRACE': r'[{}]',
    'WHITESPACE': r'\s+',
}

class Token:
    def __init__(self, type_, value):
        self.type = type_
        self.value = value

    def __repr__(self):
        return f"Token(type='{self.type}', value='{self.value}')"

def tokenize(code):
    """Converts the input pseudocode into a list of tokens."""
    token_regex = '|'.join(f'(?P<{name}>{pattern})' for name, pattern in TOKEN_TYPES.items())
    tokens = []

    for match in re.finditer(token_regex, code):
        kind = match.lastgroup
        value = match.group()

        if kind == 'WHITESPACE':
            continue

        tokens.append(Token(kind, value))

    return tokens

def tokenize_from_gui(pseudocode):
    """Wrapper for GUI tokenization."""
    return tokenize(pseudocode)

if __name__ == "__main__":
    sample_code = """
    Algorithm Example;
    Const x = 10; y = 20;
    Var z: entier;
    Debut
        ecrire("Hello World", x + y);
    Fin
    """

    tokens = tokenize(sample_code)
    for token in tokens:
        print(token)
