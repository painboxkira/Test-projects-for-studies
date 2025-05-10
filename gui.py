import tkinter as tk
from tkinter import messagebox
from lexer import tokenize_from_gui
from parser import Parser
from symbol_table import SymbolTable
from interpreter import Interpreter

def run_pseudocode():
    pseudocode = text_widget.get("1.0", tk.END).strip()
    if not pseudocode:
        messagebox.showerror("Error", "No pseudocode entered.")
        return

    output_widget.config(state=tk.NORMAL)
    output_widget.delete("1.0", tk.END)

    try:
        # Tokenize the input
        tokens = tokenize_from_gui(pseudocode)
        output_widget.insert(tk.END, "Tokens:\n" + "\n".join(map(str, tokens)) + "\n\n")

        # Parse the tokens
        symbol_table = SymbolTable()
        parser = Parser(tokens)
        ast = parser.parse_program()
        output_widget.insert(tk.END, "AST:\n" + repr(ast) + "\n\n")

        # Interpret the AST
        interpreter = Interpreter(ast, symbol_table)
        interpreter.evaluate()

        output_widget.insert(tk.END, "Execution finished successfully.\n")
        messagebox.showinfo("Success", "Pseudocode executed successfully.")
    except Exception as e:
        output_widget.insert(tk.END, f"Execution Error: {str(e)}\n")
        messagebox.showerror("Execution Error", str(e))

    output_widget.config(state=tk.DISABLED)

# Create the GUI window
root = tk.Tk()
root.title("Pseudocode Compiler")

# Input text widget
text_widget = tk.Text(root, wrap=tk.WORD, width=80, height=20)
text_widget.pack(pady=10)

# Output text widget
output_widget = tk.Text(root, wrap=tk.WORD, width=80, height=10, state=tk.DISABLED)
output_widget.pack(pady=10)

# Run button
run_button = tk.Button(root, text="Run", command=run_pseudocode)
run_button.pack(pady=10)

# Start the GUI event loop
root.mainloop()
