class Expression:
    """
    A class to represent expressions that can be evaluated
    """
    def __init__(self, type_, value):
        self.type = type_
        self.value = value

    def __repr__(self):
        return f"Expression(type='{self.type}', value='{self.value}')"

    def is_binary(self):
        return self.type == 'BinaryExpression'

    def is_literal(self):
        return self.type in {'Number', 'Identifier'}

class Interpreter:
    def evaluate_expression(self, expr, symbol_table):
        """
        Evaluate an expression, handling different types
        """
        if isinstance(expr, str):
            # Simple identifier or literal
            return symbol_table.get(expr, expr)
        
        if not isinstance(expr, Expression):
            return expr

        if expr.type == 'Number':
            return expr.value
        
        if expr.type == 'Identifier':
            return symbol_table.get(expr.value, expr.value)
        
        if expr.type == 'BinaryExpression':
            # Unpack binary expression
            operator, left, right = expr.value
            
            # Evaluate left and right sides
            left_val = self.evaluate_expression(left, symbol_table)
            right_val = self.evaluate_expression(right, symbol_table)
            
            # Perform operation
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

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def current_token(self):
        return self.tokens[self.pos] if self.pos < len(self.tokens) else None

    def eat(self, token_type):
        current = self.current_token()
        if current and current.type == token_type:
            self.pos += 1
            return current
        else:
            raise SyntaxError(f"Expected {token_type}, got {current.type if current else 'EOF'}")

    def parse_program(self):
        self.eat('KEYWORD')  # Expect 'Algorithm'
        program_name = self.eat('IDENTIFIER').value
        self.eat('DELIMITER')  # Expect ';'

        consts = self.parse_constants()
        vars_ = self.parse_variables()
        instructions = self.parse_instructions()

        return ASTNode('Program', program_name).with_children([consts, vars_, instructions])

    def parse_constants(self):
        # Check if constants section exists
        if not (self.current_token() and self.current_token().value == 'Const'):
            return None

        self.eat('KEYWORD')  # Consume 'Const'
        consts_node = ASTNode('Constants')

        # Parse constant definitions
        while (self.current_token() and 
               self.current_token().type != 'KEYWORD'):
            # Constant name
            name = self.eat('IDENTIFIER').value
            self.eat('ASSIGN')  # Expect '='
            
            # Constant value (assuming only numbers for now)
            value = self.eat('NUMBER').value
            consts_node.children.append(ASTNode('Const', (name, value)))

            # Check for delimiter or end of constants
            if (self.current_token() and 
                self.current_token().type == 'DELIMITER'):
                self.eat('DELIMITER')  # Consume ';'
            else:
                break

        return consts_node

    def parse_variables(self):
        if not (self.current_token() and self.current_token().value == 'Var'):
            return None

        self.eat('KEYWORD')  # Consume 'Var'
        vars_node = ASTNode('Variables')

        while (self.current_token() and 
               self.current_token().type != 'KEYWORD'):
            name = self.eat('IDENTIFIER').value
            self.eat('DELIMITER')  # Consume ':' 
            var_type = self.eat('IDENTIFIER').value
            vars_node.children.append(ASTNode('Variable', (name, var_type)))

            # Check for additional variables or end of variables
            if (self.current_token() and 
                self.current_token().type == 'DELIMITER'):
                self.eat('DELIMITER')  # Consume ','
            else:
                break

        return vars_node

    def parse_instructions(self):
        self.eat('KEYWORD')  # Consume 'Debut'
        instr_node = ASTNode('Instructions')

        while (self.current_token() and 
               self.current_token().value != 'Fin'):
            instr_node.children.append(self.parse_instruction())

        self.eat('KEYWORD')  # Consume 'Fin'
        return instr_node

    def parse_instruction(self):
        token = self.current_token()
        if token.value == 'lire':
            return self.parse_read()
        elif token.value == 'ecrire':
            return self.parse_write()
        elif token.type == 'IDENTIFIER':
            return self.parse_assignment()
        else:
            raise SyntaxError(f"Unknown instruction: {token.value}")

    def parse_assignment(self):
        var_name = self.eat('IDENTIFIER').value
        self.eat('ASSIGN')  # Consume ':='
        expr = self.parse_expression()
        self.eat('DELIMITER')  # Consume ';'
        return ASTNode('Assignment', (var_name, expr))

    def parse_read(self):
        self.eat('KEYWORD')  # Consume 'lire'
        var_name = self.eat('IDENTIFIER').value
        self.eat('DELIMITER')  # Consume ';'
        return ASTNode('Read', var_name)

    def parse_write(self):
        self.eat('KEYWORD')  # Consume 'ecrire'
        self.eat('PAREN')  # Consume '('
        values = []

        while self.current_token().type in {'STRING', 'IDENTIFIER'}:
            values.append(self.eat(self.current_token().type).value)

            if self.current_token().type == 'DELIMITER':
                self.eat('DELIMITER')  # Consume ','
            else:
                break

        self.eat('PAREN')  # Consume ')'
        self.eat('DELIMITER')  # Consume ';'
        return ASTNode('Write', values)

    def parse_expression(self):
        """
        Parse arithmetic expressions
        """
        return self._parse_binary_expression()

    def _parse_binary_expression(self):
        """
        Parse binary expressions
        """
        # Start with the left operand
        left = self._parse_primary()

        # Check for binary operators
        while (self.current_token() and 
               self.current_token().type == 'OPERATOR'):
            # Store the operator
            op = self.current_token().value
            self.eat('OPERATOR')

            # Parse the right operand
            right = self._parse_primary()

            # Create a binary expression
            left = Expression('BinaryExpression', (op, left, right))

        return left

    def _parse_primary(self):
        """
        Parse primary expressions (identifiers or numbers)
        """
        token = self.current_token()
        
        if token.type == 'NUMBER':
            self.eat('NUMBER')
            return Expression('Number', int(token.value))
        elif token.type == 'IDENTIFIER':
            self.eat('IDENTIFIER')
            return Expression('Identifier', token.value)
        else:
            raise SyntaxError(f"Unexpected token in expression: {token.type}")

class ASTNode:
    def __init__(self, type_, value=None):
        self.type = type_
        self.value = value
        self.children = []

    def __repr__(self):
        return f"ASTNode(type='{self.type}', value='{self.value}', children={self.children})"

    def add_child(self, child):
        self.children.append(child)

    def with_children(self, children):
        children = [child for child in children if child is not None]
        self.children.extend(children)
        return self

# Example of how to use the Parser and Interpreter
def run_algorithm(tokens):
    # Parse the tokens into an AST
    parser = Parser(tokens)
    ast = parser.parse_program()

    # Create a symbol table to track variable values
    symbol_table = {}

    # Process constants if present
    constants = next((child for child in ast.children if child.type == 'Constants'), None)
    if constants:
        for const_node in constants.children:
            name, value = const_node.value
            symbol_table[name] = value

    # Process instructions
    instructions = next(child for child in ast.children if child.type == 'Instructions')
    
    interpreter = Interpreter()

    # Execute each instruction
    for instr in instructions.children:
        if instr.type == 'Assignment':
            var_name, expr = instr.value
            # Evaluate the expression and store in symbol table
            symbol_table[var_name] = interpreter.evaluate_expression(expr, symbol_table)
        elif instr.type == 'Write':
            # Write values from symbol table
            for var in instr.value:
                print(symbol_table.get(var, var))

    return symbol_table
