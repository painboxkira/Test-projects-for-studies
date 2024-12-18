import sys
from lexer import tokenize
from parser import Parser
from interpreter import Interpreter
from symbol_table import SymbolTable
from gui import run_pseudocode

def compile_and_run(code):
    """
    Compiles and runs the given pseudocode.
    """
    try:
        # Lexical Analysis
        tokens = tokenize(code)
        print("Tokens:", tokens)

        # Parsing
        parser = Parser(tokens)
        ast = parser.parse_program()
        print("AST:", ast)

        # Symbol Table
        symbol_table = SymbolTable()

        # Interpretation
        interpreter = Interpreter(ast, symbol_table)
        interpreter.evaluate()

    except Exception as e:
        print(f"Error: {str(e)}")

if __name__ == "__main__":
    if len(sys.argv) > 1:
        # If a file is provided as an argument
        filepath = sys.argv[1]
        try:
            with open(filepath, "r") as f:
                code = f.read()
            compile_and_run(code)
        except FileNotFoundError:
            print(f"Error: File '{filepath}' not found.")
    else:
        # Launch the GUI
        run_pseudocode()
