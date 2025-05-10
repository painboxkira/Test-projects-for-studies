from parser import Expression, ASTNode
from symbol_table import SymbolTable

class Interpreter:
    def __init__(self, ast, symbol_table):
        self.ast = ast
        self.symbol_table = symbol_table

    def evaluate(self):
        self.execute(self.ast)

    def execute(self, node):
        if node.type == 'Program':
            # Execute each section of the program in order
            for child in node.children:
                self.execute(child)

        elif node.type == 'Constants':
            # Add each constant definition to the symbol table
            for const in node.children:
                name, value = const.value
                self.symbol_table.add_constant(name, value)

        elif node.type == 'Variables':
            # Declare each variable in the symbol table
            for var in node.children:
                name, var_type = var.value
                self.symbol_table.add_variable(name, var_type)

        elif node.type == 'Instructions':
            # Execute each instruction in the instructions node
            for instruction in node.children:
                self.execute(instruction)

        elif node.type == 'Read':
            # Read input from the user and assign it to a variable
            var_name = node.value
            if var_name not in self.symbol_table.variables:
                raise RuntimeError(f"Variable '{var_name}' is not defined.")
            user_input = input(f"Enter value for {var_name}: ")
            # Attempt to convert input to int if it is numeric
            if user_input.isdigit():
                user_input = int(user_input)
            self.symbol_table.set_variable(var_name, user_input)

        elif node.type == 'Write':
            # Print the values of variables, constants, or string literals
            output = []
            for value in node.value:
                if isinstance(value, str):
                    # If it's a string, it could be a variable, constant, or string literal
                    if value in self.symbol_table.variables:
                        output.append(self.symbol_table.get_variable(value))
                    elif value in self.symbol_table.constants:
                        output.append(self.symbol_table.get_variable(value))
                    else:
                        # Remove surrounding quotes if present (for string literals)
                        if value.startswith('"') and value.endswith('"'):
                            value = value[1:-1]
                        output.append(value)
                elif isinstance(value, Expression):
                    # Evaluate any expression nodes
                    val = self.evaluate_expression(value)
                    output.append(val)
                else:
                    # In case of unexpected type, just append
                    output.append(value)
            print(" ".join(map(str, output)))

        elif node.type == 'Assignment':
            # Assign the evaluated expression to the specified variable
            var_name, expr = node.value
            if var_name not in self.symbol_table.variables:
                raise RuntimeError(f"Variable '{var_name}' is not defined.")
            result = self.evaluate_expression(expr)
            self.symbol_table.set_variable(var_name, result)

        elif node.type == 'If':
            # Conditional execution
            condition, true_block, false_block = node.value
            cond_val = self.evaluate_expression(condition)
            if cond_val:
                self.execute(true_block)
            elif false_block:
                self.execute(false_block)

        elif node.type == 'For':
            # For loop execution
            iterator, start, end, body = node.value
            start_val = self.evaluate_expression(start)
            end_val = self.evaluate_expression(end)
            for i in range(int(start_val), int(end_val) + 1):
                self.symbol_table.set_variable(iterator, i)
                self.execute(body)

        else:
            raise RuntimeError(f"Unknown AST node type: {node.type}")

    def evaluate_expression(self, expr):
        # Evaluate different kinds of expressions
        if isinstance(expr, Expression):
            if expr.type == 'Number':
                return expr.value
            elif expr.type == 'Identifier':
                return self.symbol_table.get_variable(expr.value)
            elif expr.type == 'BinaryExpression':
                operator, left, right = expr.value
                left_val = self.evaluate_expression(left)
                right_val = self.evaluate_expression(right)

                if operator == '+':
                    return left_val + right_val
                elif operator == '-':
                    return left_val - right_val
                elif operator == '*':
                    return left_val * right_val
                elif operator == '/':
                    return left_val / right_val
                else:
                    raise ValueError(f"Unsupported operator: {operator}")
            else:
                raise RuntimeError(f"Unknown expression type: {expr.type}")

        elif isinstance(expr, str):
            # Could be a variable, constant, or a numeric string
            if expr in self.symbol_table.variables:
                return self.symbol_table.get_variable(expr)
            elif expr in self.symbol_table.constants:
                return self.symbol_table.get_variable(expr)
            elif expr.isdigit():
                return int(expr)
            else:
                raise RuntimeError(f"Cannot evaluate expression: {expr}")

        else:
            raise RuntimeError(f"Cannot evaluate expression of type {type(expr)}: {expr}")
