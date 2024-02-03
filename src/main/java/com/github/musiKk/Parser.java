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
            case DEF -> parseFunctionDeclaration(tokens);
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

    // <> def receiver? . name "(" parameters* ")" (":" type)? "=" expression
    private FunctionDeclaration parseFunctionDeclaration(Tokens tokens) {
        tokens.next(TokenType.DEF);
        var nameToken = tokens.next(TokenType.IDENTIFIER);

        FunctionReceiverType receiver;
        if (tokens.peek().type() == TokenType.DOT) {
            tokens.next();
            var receiverToken = tokens.next(TokenType.IDENTIFIER);
            receiver = new FunctionReceiverVariable(receiverToken.image());
        } else {
            receiver = StandaloneFunctionReceiver.get();
        }

        List<VariableDeclaration> parameters = new ArrayList<>();
        tokens.next(TokenType.LPAREN);
        var maybeParameter = tokens.peek();
        if (!tokens.matches(TokenType.RPAREN)) {
            var variableDeclaration = parseVariableDeclaration(tokens);
            parameters.add(variableDeclaration);
            while (tokens.matches(TokenType.COMMA)) {
                maybeParameter = tokens.peek();
                if (maybeParameter.type() == TokenType.RPAREN) {
                    tokens.next();
                    break;
                }
                tokens.next(TokenType.COMMA);
                variableDeclaration = parseVariableDeclaration(tokens);
                parameters.add(variableDeclaration);
            }
        }
        tokens.next(TokenType.RPAREN);
        var token = tokens.peek();

        String returnType;
        if (token.type() == TokenType.COLON) {
            tokens.next(TokenType.COLON);
            var typeToken = tokens.next(TokenType.IDENTIFIER);
            returnType = typeToken.image();
        } else {
            returnType = "void";
        }
        tokens.next(TokenType.EQUALS);

        var body = parseExpression(tokens);
        return new FunctionDeclaration(receiver, nameToken.image(), parameters, returnType, body);
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
        return parsePlus(tokens);
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
            case IDENTIFIER -> {
                yield parseNameExpression(tokens);
            }
            case NUMBER -> {
                var numberToken = tokens.next();
                yield new NumberExpression(Integer.parseInt(numberToken.image()));
            }
            case STRING -> {
                var stringToken = tokens.next();
                yield new StringExpression(stringToken.image());
            }
            case VAR -> {
                tokens.next();
                var variableDeclaration = parseVariableDeclaration(tokens);
                yield new VariableExpression(Optional.empty(), variableDeclaration.name());
            }
            case LBRACE -> parseBlockExpression(tokens);
            default -> throw new RuntimeException("Unexpected token: " + token);
        };

        token = tokens.peek();
        if (token.type() == TokenType.DOT) {
            tokens.next();
            var invocation = parseExpression(tokens);
            switch (invocation) {
                case VariableExpression ve -> {
                    return new VariableExpression(Optional.of(expression), ve.name());
                }
                case FunctionEvaluationExpression fee -> {
                    return new FunctionEvaluationExpression(Optional.of(expression), fee.name(), fee.arguments());
                }
                default -> throw new RuntimeException("Unexpected token: " + token);
            }
        } else {
            return expression;
        }


        // if (token.type() == TokenType.IDENTIFIER) {
        //     return parseNameExpression(tokens);
        // } else if (token.type() == TokenType.NUMBER) {
        //     var numberToken = tokens.next();
        //     return new NumberExpression(Integer.parseInt(numberToken.image()));
        // } else if (token.type() == TokenType.VAR) {
        //     tokens.next();
        //     var variableDeclaration = parseVariableDeclaration(tokens);
        //     return new VariableExpression(Optional.empty(), variableDeclaration.name());
        // } else if (token.type() == TokenType.LBRACE) {
        //     var expression = parseBlockExpression(tokens);
        //     return expression;
        // } else {
        //     throw new RuntimeException("Unexpected token: " + token);
        // }
    }

    // <> name
    private Expression parseNameExpression(Tokens tokens) {
        var nameToken = tokens.next(TokenType.IDENTIFIER);
        var token = tokens.peek();

        return switch (token.type()) {
            case LPAREN -> parseFunctionEvaluationExpression(tokens, nameToken.image(), Optional.empty());
            case DOT -> parseDottedExpression(tokens, new VariableExpression(Optional.empty(), nameToken.image()));
            default -> new VariableExpression(Optional.empty(), nameToken.image());
        };
    }

    // expression <> .
    private Expression parseDottedExpression(Tokens tokens, Expression expression) {
        tokens.next(TokenType.DOT);

        var memberNameToken = tokens.next(TokenType.IDENTIFIER);

        var maybeParen = tokens.peek();
        return switch (maybeParen.type()) {
            case LPAREN -> parseFunctionEvaluationExpression(tokens, memberNameToken.image(), Optional.of(expression));
            default -> new VariableExpression(Optional.of(expression), memberNameToken.image());
        };
    }

    // name . <>
    // private Expression parseDottedExpression(Tokens tokens, Token nameToken) {
    //     tokens.next(TokenType.DOT);

    //     var memberNameToken = tokens.next(TokenType.IDENTIFIER);

    //     var token = tokens.peek();
    //     return switch (token.type()) {
    //         case LPAREN -> parseFunctionEvaluationExpression(tokens, memberNameToken.image(), Optional.of(new VariableExpression(nameToken.image())));
    //         default -> new VariableExpression(Optional.of(nameToken.image()), memberNameToken.image());
    //     };
    // }

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
            new FunctionDeclaration(StandaloneFunctionReceiver.get(), "foo", List.of(), "void", new BlockExpression(List.of()))));
        testCases.add(new StatementTestCase(
            "def foo(i) = {}",
            new FunctionDeclaration(StandaloneFunctionReceiver.get(), "foo", List.of(new VariableDeclaration("i")), "void", new BlockExpression(List.of()))));
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
            new VariableExpression(Optional.of(new VariableExpression("foo")), "bar")));
        testCases.add(new ExpressionTestCase(
            "foo.bar()",
            new FunctionEvaluationExpression(Optional.of(new VariableExpression("foo")), "bar", List.of())));
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
                new VariableExpression(Optional.of(new VariableExpression("b")), "c"))));
        testCases.add(new ExpressionTestCase(
            "a + b.c()",
            new BinaryExpression(
                new VariableExpression("a"),
                TokenType.PLUS,
                new FunctionEvaluationExpression(Optional.of(new VariableExpression("b")), "c", List.of()))));
        testCases.add(new ExpressionTestCase(
            "a.b.c",
            new VariableExpression(Optional.of(new VariableExpression( Optional.of(new VariableExpression("a")), "b")), "c")));

        for (var testCase : testCases) {
            System.err.printf("%-30s", testCase.input());

            var tokens = new Tokenizer().tokenize(testCase.input());
            var result = testCase.parseFn().apply(p, tokens);
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

sealed interface Statement permits
    ExpressionStatement,
    Import,
    FunctionDeclaration,
    VariableDeclaration,
    DataDefinition
{}

record ExpressionStatement(
    Expression expression
) implements Statement {}

sealed interface Expression permits
    BlockExpression,
    NumberExpression,
    StringExpression,
    VariableExpression,
    FunctionEvaluationExpression,
    BinaryExpression
{}
record BlockExpression(
    List<Statement> statements
) implements Expression {}
record NumberExpression(
    int number
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

record Import(String name) implements Statement {}
record FunctionDeclaration(
    FunctionReceiverType receiver,
    String name,
    List<VariableDeclaration> parameters,
    String returnType,
    Expression body
) implements Statement {}
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
record DataDefinition(
    String name,
    List<VariableDeclaration> variableDeclarations
) implements Statement {}
record CompilationUnit(
    List<Statement> statements
) {}
