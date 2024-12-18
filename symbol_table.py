class SymbolTable:
    def __init__(self):
        self.constants = {}
        self.variables = {}

    def add_constant(self, name, value):
        if name in self.constants or name in self.variables:
            raise RuntimeError(f"Constant '{name}' is already defined.")
        self.constants[name] = value

    def add_variable(self, name, var_type):
        if name in self.constants or name in self.variables:
            raise RuntimeError(f"Variable '{name}' is already defined.")
        self.variables[name] = None  # Initialize variable with None

    def set_variable(self, name, value):
        if name in self.constants:
            raise RuntimeError(f"Cannot modify constant '{name}'.")
        if name not in self.variables:
            raise RuntimeError(f"Variable '{name}' is not defined.")
        self.variables[name] = value

    def get_variable(self, name):
        if name in self.variables:
            return self.variables[name]
        if name in self.constants:
            return self.constants[name]
        raise RuntimeError(f"'{name}' is not defined.")

if __name__ == "__main__":
    # Test the SymbolTable class
    symbol_table = SymbolTable()

    # Add constants
    symbol_table.add_constant("PI", 3.14)
    symbol_table.add_constant("E", 2.71)

    # Add variables
    symbol_table.add_variable("x", "entier")
    symbol_table.add_variable("y", "reel")

    # Set variable values
    symbol_table.set_variable("x", 42)
    symbol_table.set_variable("y", 3.14)

    # Retrieve values
    print("PI:", symbol_table.get_variable("PI"))  # Output: PI: 3.14
    print("x:", symbol_table.get_variable("x"))    # Output: x: 42
    print("y:", symbol_table.get_variable("y"))    # Output: y: 3.14
