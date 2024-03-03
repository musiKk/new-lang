package com.github.musiKk;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;

import com.github.musiKk.Tokenizer.TokenType;
import com.github.musiKk.Tokenizer.Tokens;

public class Parser {

    public CompilationUnit parseCompilationUnit(Tokens tokens) {
        var token = tokens.peek();

        List<Statement> statements = new ArrayList<>();

        while (token.type() != TokenType.EOF) {
            var statement = parseStatement(tokens);
            statements.add(statement);
            token = tokens.peek();
        }

        return new CompilationUnit(statements);
    }

    private Statement parseStatement(Tokens tokens) {
        var token = tokens.peek();

        return switch (token.type()) {
            case IMPORT -> parseImportStatement(tokens);
            case DATA -> parseDataDefinition(tokens);
            case TRAIT -> parseTraitDefinition(tokens);
            case IMPL -> parseTraitImplementation(tokens);
            case DEF -> parseFunctionDeclaration(tokens);
            case NATIVE -> parseNativeFunctionDeclaration(tokens);
            case VAR -> {
                tokens.next();
                yield parseVariableDeclaration(tokens);
            }
            default -> new ExpressionStatement(parseExpression(tokens));
        };
    }

    private Import parseImportStatement(Tokens tokens) {
        tokens.next(TokenType.IMPORT);
        var nameToken = tokens.next(TokenType.IDENTIFIER);
        return new Import(nameToken.image());
    }

    // <> impl typeName is traitName { [functionDeclaration]* }
    private TraitImplementation parseTraitImplementation(Tokens tokens) {
        tokens.next(TokenType.IMPL);
        var typeNameToken = tokens.next(TokenType.IDENTIFIER);
        tokens.next(TokenType.IS);
        var traitNameToken = tokens.next(TokenType.IDENTIFIER);
        tokens.next(TokenType.LBRACE);

        List<FunctionDeclaration> functionDeclarations = new ArrayList<>();

        while (tokens.matches(TokenType.DEF)) {
            functionDeclarations.add(parseFunctionDeclaration(tokens));
        }

        tokens.next(TokenType.RBRACE);
        return new TraitImplementation(typeNameToken.image(), traitNameToken.image(), functionDeclarations);
    }

    private TraitDefinition parseTraitDefinition(Tokens tokens) {
        tokens.next(TokenType.TRAIT);
        var nameToken = tokens.next(TokenType.IDENTIFIER);
        tokens.next(TokenType.LBRACE);

        List<FunctionSignature> signatures = new ArrayList<>();

        while (!tokens.matches(TokenType.RBRACE)) {
            signatures.add(parseFunctionSignature(tokens));
        }

        tokens.next(TokenType.RBRACE);

        return new TraitDefinition(nameToken.image(), signatures);
    }

    private DataDefinition parseDataDefinition(Tokens tokens) {
        tokens.next(TokenType.DATA);

        var nameToken = tokens.next(TokenType.IDENTIFIER);
        tokens.next(TokenType.LBRACE);

        List<VariableDeclaration> variableDeclarations = new ArrayList<>();

        while (true) {
            var token = tokens.peek();
            if (token.type() == TokenType.RBRACE) {
                tokens.next();
                break;
            }

            tokens.peek(TokenType.IDENTIFIER);
            var variableDeclaration = parseVariableDeclaration(tokens);
            variableDeclarations.add(variableDeclaration);
        }

        return new DataDefinition(nameToken.image(), variableDeclarations);
    }

    // <> def name "(" parameters* ") [: type]?"
    private FunctionSignature parseFunctionSignature(Tokens tokens) {
        tokens.next(TokenType.DEF);
        var nameToken = tokens.next(TokenType.IDENTIFIER);

        FunctionReceiverType receiver;
        if (tokens.peek().type() == TokenType.DOT) {
            tokens.next();
            receiver = new FunctionReceiverVariable(nameToken.image());
            nameToken = tokens.next(TokenType.IDENTIFIER);
        } else {
            receiver = StandaloneFunctionReceiver.get();
        }

        List<VariableDeclaration> parameters = new ArrayList<>();
        tokens.next(TokenType.LPAREN);
        while (!tokens.matches(TokenType.RPAREN)) {
            var variableDeclaration = parseVariableDeclaration(tokens);
            parameters.add(variableDeclaration);
            if (tokens.matches(TokenType.COMMA)) {
                tokens.next(TokenType.COMMA);
            }
        }
        tokens.next(TokenType.RPAREN);

        String returnType;
        if (tokens.matches(TokenType.COLON)) {
            tokens.next(TokenType.COLON);
            var typeToken = tokens.next(TokenType.IDENTIFIER);
            returnType = typeToken.image();
        } else {
            returnType = "void";
        }
        return new FunctionSignature(receiver, nameToken.image(), parameters, returnType);
    }

    // <> native def name "(" parameters* ") [: type]?"
    private NativeFunctionDeclaration parseNativeFunctionDeclaration(Tokens tokens) {
        tokens.next(TokenType.NATIVE);
        var functionSignature = parseFunctionSignature(tokens);
        return new NativeFunctionDeclaration(functionSignature);
    }

    // <> def (receiver .)? name "(" parameters* ")" (":" type)? "=" expression
    private UserFunctionDeclaration parseFunctionDeclaration(Tokens tokens) {
        var functionSignature = parseFunctionSignature(tokens);
        tokens.next(TokenType.EQUALS);

        var body = parseExpression(tokens);
        return new UserFunctionDeclaration(functionSignature, body);
    }

    private VariableDeclaration parseVariableDeclaration(Tokens tokens) {
        var nameToken = tokens.next(TokenType.IDENTIFIER);

        Optional<String> type = Optional.empty();
        if (tokens.matches(TokenType.COLON)) {
            tokens.next();
            var typeToken = tokens.next(TokenType.IDENTIFIER);
            type = Optional.of(typeToken.image());
        }
        Optional<Expression> initialization = Optional.empty();
        if (tokens.matches(TokenType.EQUALS)) {
            tokens.next();
            initialization = Optional.of(parseExpression(tokens));
        }
        return new VariableDeclaration(nameToken.image(), type, initialization);
    }

    private Expression parseExpression(Tokens tokens) {
        return parseAssignment(tokens);
    }

    private Expression parseAssignment(Tokens tokens) {
        var expr = parseComparison(tokens);

        if (tokens.matches(TokenType.EQUALS)) {
            if (expr instanceof VariableExpression ve) {
                tokens.next();
                var right = parseAssignment(tokens);
                expr = new AssignmentExpression(ve, right);
            } else {
                throw new RuntimeException("Left-hand side of assignment must be a variable");
            }
        }
        return expr;
    }

    private Expression parseComparison(Tokens tokens) {
        var expr = parsePlus(tokens);

        while (tokens.matches(TokenType.EQUALS_EQUALS, TokenType.NOT_EQUALS, TokenType.LT, TokenType.GT, TokenType.LE, TokenType.GE)) {
            var operator = tokens.next().type();
            var right = parsePlus(tokens);
            expr = new BinaryExpression(expr, operator, right);
        }
        return expr;
    }

    private Expression parsePlus(Tokens tokens) {
        var expr = parseTimes(tokens);

        while (tokens.matches(TokenType.PLUS, TokenType.MINUS)) {
            var operator = tokens.next().type();
            var right = parseTimes(tokens);
            expr = new BinaryExpression(expr, operator, right);
        }
        return expr;
    }

    private Expression parseTimes(Tokens tokens) {
        var expr = parseAtom(tokens);

        while (tokens.matches(TokenType.STAR, TokenType.SLASH)) {
            var operator = tokens.next().type();
            var right = parseAtom(tokens);
            expr = new BinaryExpression(expr, operator, right);
        }
        return expr;
    }

    private Expression parseAtom(Tokens tokens) {
        var token = tokens.peek();

        var expression = switch (token.type()) {
            case IDENTIFIER -> parseNameExpression(tokens);
            case NUMBER -> {
                var numberToken = tokens.next();
                yield new NumberExpression(Long.parseLong(numberToken.image()));
            }
            case STRING -> {
                var stringToken = tokens.next();
                yield new StringExpression(stringToken.image());
            }
            case LBRACE -> parseBlockExpression(tokens);
            case LPAREN -> {
                tokens.next(TokenType.LPAREN);
                var e = parseExpression(tokens);
                tokens.next(TokenType.RPAREN);
                yield e;
            }
            case IF -> parseIfExpression(tokens);
            default -> throw new RuntimeException("Unexpected token: " + token);
        };

        while (tokens.matches(TokenType.DOT)) {
            tokens.next();
            var invocation = parseNameExpression(tokens);
            expression = switch (invocation) {
                case VariableExpression ve -> {
                    yield new VariableExpression(expression, ve.name());
                }
                case FunctionEvaluationExpression fee -> {
                    yield new FunctionEvaluationExpression(expression, fee.name(), fee.arguments());
                }
                default -> throw new RuntimeException("Unexpected parse: " + invocation);
            };
        }
        return expression;
    }

    private Expression parseIfExpression(Tokens tokens) {
        tokens.next(TokenType.IF);
        var condition = parseExpression(tokens);
        var thenBranch = parseExpression(tokens);
        var elseBranch = Optional.<Expression>empty();
        if (tokens.matches(TokenType.ELSE)) {
            tokens.next(TokenType.ELSE);
            elseBranch = Optional.of(parseExpression(tokens));
        }
        return new IfExpression(condition, thenBranch, elseBranch);
    }

    private Expression parseNameExpression(Tokens tokens) {
        var nameToken = tokens.next(TokenType.IDENTIFIER);
        var token = tokens.peek();

        return switch (token.type()) {
            case LPAREN -> parseFunctionEvaluationExpression(tokens, nameToken.image(), Optional.empty());
            default -> new VariableExpression(Optional.empty(), nameToken.image());
        };
    }

    // target? .? name <>
    private FunctionEvaluationExpression parseFunctionEvaluationExpression(Tokens tokens, String name, Optional<Expression> target) {
        tokens.next(TokenType.LPAREN);

        List<Expression> arguments = new ArrayList<>();

        {
            var token = tokens.peek();
            if (token.type() == TokenType.RPAREN) {
                tokens.next();
                return new FunctionEvaluationExpression(target, name, List.of());
            } else {
                var expression = parseExpression(tokens);
                arguments.add(expression);
            }
        }

        while (true) {
            var token = tokens.peek();
            if (token.type() == TokenType.RPAREN) {
                tokens.next();
                break;
            }
            tokens.next(TokenType.COMMA);
            var expression = parseExpression(tokens);
            arguments.add(expression);
        }
        return new FunctionEvaluationExpression(target, name, arguments);
    }

    private BlockExpression parseBlockExpression(Tokens tokens) {
        tokens.next(TokenType.LBRACE);
        var statements = new ArrayList<Statement>();
        while (true) {
            var token = tokens.peek();
            if (token.type() == TokenType.RBRACE) {
                tokens.next();
                break;
            }
            var statement = parseStatement(tokens);
            statements.add(statement);
        }
        return new BlockExpression(statements);
    }

    public static void main(String[] args) {
        var p = new Parser();

        List<TestCase<?>> testCases = new ArrayList<>();
        testCases.add(new StatementTestCase(
            "data Foo { x: int }",
            new DataDefinition("Foo", List.of(new VariableDeclaration("x", "int")))
            ));
        testCases.add(new StatementTestCase(
            "var x = 1", new VariableDeclaration("x", new NumberExpression(1))));
        testCases.add(new StatementTestCase(
            "def foo() = {}",
            new UserFunctionDeclaration(new FunctionSignature(StandaloneFunctionReceiver.get(), "foo", List.of(), "void"), new BlockExpression(List.of()))));
        testCases.add(new StatementTestCase(
            "def foo(i) = {}",
            new UserFunctionDeclaration(new FunctionSignature(StandaloneFunctionReceiver.get(), "foo", List.of(new VariableDeclaration("i")), "void"), new BlockExpression(List.of()))));
        testCases.add(new StatementTestCase(
            "def Foo.foo() = {}",
            new UserFunctionDeclaration(new FunctionSignature(new FunctionReceiverVariable("Foo"), "foo", List.of(), "void"), new BlockExpression(List.of()))));
        testCases.add(new StatementTestCase(
            "trait Dog { def bark() }",
            new TraitDefinition("Dog", List.of(new FunctionSignature(StandaloneFunctionReceiver.get(), "bark", List.of(), "void")))));
        testCases.add(new ExpressionTestCase(
            "foo()",
            new FunctionEvaluationExpression("foo", List.of())));
        testCases.add(new ExpressionTestCase(
            "foo(1)",
            new FunctionEvaluationExpression("foo", List.of(new NumberExpression(1)))));
        testCases.add(new ExpressionTestCase(
            "foo(\"foo\")",
            new FunctionEvaluationExpression("foo", List.of(new StringExpression("foo")))));
        testCases.add(new ExpressionTestCase(
            "foo.bar",
            new VariableExpression(new VariableExpression("foo"), "bar")));
        testCases.add(new ExpressionTestCase(
            "foo.bar()",
            new FunctionEvaluationExpression(new VariableExpression("foo"), "bar", List.of())));
        testCases.add(new ExpressionTestCase(
            "a + b",
            new BinaryExpression(new VariableExpression("a"), TokenType.PLUS, new VariableExpression("b"))));
        testCases.add(new ExpressionTestCase(
            "a + b * c",
            new BinaryExpression(
                new VariableExpression("a"),
                TokenType.PLUS,
                new BinaryExpression(
                    new VariableExpression("b"),
                    TokenType.STAR,
                    new VariableExpression("c")))));
        testCases.add(new ExpressionTestCase(
            "a + b.c",
            new BinaryExpression(
                new VariableExpression("a"),
                TokenType.PLUS,
                new VariableExpression(new VariableExpression("b"), "c"))));
        testCases.add(new ExpressionTestCase(
            "a + b.c()",
            new BinaryExpression(
                new VariableExpression("a"),
                TokenType.PLUS,
                new FunctionEvaluationExpression(new VariableExpression("b"), "c", List.of()))));
        testCases.add(new ExpressionTestCase(
            "b.c() + a",
            new BinaryExpression(
                new FunctionEvaluationExpression(new VariableExpression("b"), "c", List.of()),
                TokenType.PLUS,
                new VariableExpression("a"))));
        testCases.add(new ExpressionTestCase(
            "a.b().c.d()",
            new FunctionEvaluationExpression(
                new VariableExpression(new FunctionEvaluationExpression(new VariableExpression("a"), "b", List.of()), "c"),
                "d",
                List.of())));
        testCases.add(new ExpressionTestCase(
            "a.b.c",
            new VariableExpression(new VariableExpression(new VariableExpression("a"), "b"), "c")));
        testCases.add(new ExpressionTestCase(
            "a = b",
            new AssignmentExpression(new VariableExpression("a"), new VariableExpression("b"))));
        testCases.add(new ExpressionTestCase(
            "a.b = c",
            new AssignmentExpression(new VariableExpression(new VariableExpression("a"), "b"), new VariableExpression("c"))));
        testCases.add(new ExpressionTestCase(
            "a = b == c", new AssignmentExpression(
                new VariableExpression("a"),
                new BinaryExpression(
                    new VariableExpression("b"),
                    TokenType.EQUALS_EQUALS,
                    new VariableExpression("c")))));
        testCases.add(new ExpressionTestCase("a = b = c", new AssignmentExpression(
            new VariableExpression("a"),
            new AssignmentExpression(
                new VariableExpression("b"),
                new VariableExpression("c")))));

        for (var testCase : testCases) {
            System.err.printf("%-30s", testCase.input());

            var tokens = new Tokenizer().tokenize(testCase.input());
            Object result;
            try {
                result = testCase.parseFn().apply(p, tokens);
            } catch (Exception e) {
                System.err.println("FAIL");
                e.printStackTrace();
                continue;
            }
            if (!result.equals(testCase.expected())) {
                System.err.println("FAIL");
                System.err.println("got:      " + result);
                System.err.println("expected: " + testCase.expected());

            } else {
                System.err.println("OK");
            }
        }
    }

    interface TestCase<T> {
        String input();
        T expected();
        BiFunction<Parser, Tokens, T> parseFn();
    }

    record StatementTestCase(
        String input,
        Statement expected
    ) implements TestCase<Statement> {
        public BiFunction<Parser, Tokens, Statement> parseFn() {
            return (parser, tokens) -> parser.parseStatement(tokens);
        }
    }

    record ExpressionTestCase(
        String input,
        Expression expected
    ) implements TestCase<Expression> {
        public BiFunction<Parser, Tokens, Expression> parseFn() {
            return (parser, tokens) -> parser.parseExpression(tokens);
        }
    }

}

sealed interface Statement {}

record ExpressionStatement(
    Expression expression
) implements Statement {}

sealed interface Expression {}

record AssignmentExpression(
    VariableExpression target,
    Expression value
) implements Expression {}
record BlockExpression(
    List<Statement> statements
) implements Expression {}
record NumberExpression(
    long number
) implements Expression {}
record StringExpression(
    String string
) implements Expression {}
record VariableExpression(
    Optional<Expression> target,
    String name
) implements Expression {
    public VariableExpression(String name) {
        this(Optional.empty(), name);
    }
    public VariableExpression(Expression target, String name) {
        this(Optional.of(target), name);
    }
}
record BinaryExpression(
    Expression left,
    TokenType operator,
    Expression right
) implements Expression {}

record FunctionEvaluationExpression(
    Optional<Expression> target,
    String name,
    List<Expression> arguments
) implements Expression {
    public FunctionEvaluationExpression(String name, List<Expression> arguments) {
        this(Optional.empty(), name, arguments);
    }
    public FunctionEvaluationExpression(Expression target, String name, List<Expression> arguments) {
        this(Optional.of(target), name, arguments);
    }
}

sealed interface FunctionReceiverType permits FunctionReceiverVariable, StandaloneFunctionReceiver {}
record FunctionReceiverVariable(
    String name
) implements FunctionReceiverType {}
record StandaloneFunctionReceiver() implements FunctionReceiverType {
    private static final StandaloneFunctionReceiver INSTANCE = new StandaloneFunctionReceiver();
    public static StandaloneFunctionReceiver get() {
        return INSTANCE;
    }
}

record IfExpression(
    Expression condition,
    Expression thenBranch,
    Optional<Expression> elseBranch
) implements Expression {}

record Import(String name) implements Statement {}

sealed interface FunctionDeclaration extends Statement {
    FunctionSignature signature();
}
record NativeFunctionDeclaration(
    FunctionSignature signature
) implements FunctionDeclaration {
}
record UserFunctionDeclaration(
    FunctionSignature signature,
    Expression body
) implements FunctionDeclaration {}
record VariableDeclaration(
    String name,
    Optional<String> type,
    Optional<Expression> initializer
) implements Statement {
    public VariableDeclaration(String name) {
        this(name, Optional.empty(), Optional.empty());
    }
    public VariableDeclaration(String name, Optional<String> type) {
        this(name, type, Optional.empty());
    }
    public VariableDeclaration(String name, String type) {
        this(name, Optional.of(type), Optional.empty());
    }
    public VariableDeclaration(String name, Expression initializer) {
        this(name, Optional.empty(), Optional.of(initializer));
    }
}

record FunctionSignature(
    FunctionReceiverType receiver,
    String name,
    List<VariableDeclaration> parameters,
    String returnType
) {}

record DataDefinition(
    String name,
    List<VariableDeclaration> variableDeclarations
) implements Statement {}
record TraitDefinition(
    String name,
    List<FunctionSignature> functionSignatures
) implements Statement {}
record TraitImplementation(
    String typeName,
    String traitName,
    List<FunctionDeclaration> functionDeclarations
) implements Statement {}

record CompilationUnit(
    List<Statement> statements
) {}
