package com.github.musiKk.parser;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import com.github.musiKk.Tokenizer.TokenType;
import com.github.musiKk.Tokenizer.Tokens;
import com.github.musiKk.parser.CompilationUnit.ArrayAssignmentExpression;
import com.github.musiKk.parser.CompilationUnit.ArrayCreationExpression;
import com.github.musiKk.parser.CompilationUnit.ArrayLookupExpression;
import com.github.musiKk.parser.CompilationUnit.AssignmentExpression;
import com.github.musiKk.parser.CompilationUnit.BinaryExpression;
import com.github.musiKk.parser.CompilationUnit.BlockExpression;
import com.github.musiKk.parser.CompilationUnit.BooleanExpression;
import com.github.musiKk.parser.CompilationUnit.DataDefinition;
import com.github.musiKk.parser.CompilationUnit.Expression;
import com.github.musiKk.parser.CompilationUnit.ExpressionStatement;
import com.github.musiKk.parser.CompilationUnit.ForExpression;
import com.github.musiKk.parser.CompilationUnit.FunctionDeclaration;
import com.github.musiKk.parser.CompilationUnit.FunctionEvaluationExpression;
import com.github.musiKk.parser.CompilationUnit.FunctionSignature;
import com.github.musiKk.parser.CompilationUnit.IfExpression;
import com.github.musiKk.parser.CompilationUnit.Import;
import com.github.musiKk.parser.CompilationUnit.NativeFunctionDeclaration;
import com.github.musiKk.parser.CompilationUnit.NullExpression;
import com.github.musiKk.parser.CompilationUnit.NumberExpression;
import com.github.musiKk.parser.CompilationUnit.Statement;
import com.github.musiKk.parser.CompilationUnit.StringExpression;
import com.github.musiKk.parser.CompilationUnit.TraitDefinition;
import com.github.musiKk.parser.CompilationUnit.TraitImplementation;
import com.github.musiKk.parser.CompilationUnit.UserFunctionDeclaration;
import com.github.musiKk.parser.CompilationUnit.VariableDeclaration;
import com.github.musiKk.parser.CompilationUnit.VariableExpression;

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

    Statement parseStatement(Tokens tokens) {
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

        Optional<String> receiver;
        if (tokens.peek().type() == TokenType.DOT) {
            tokens.next();
            receiver = Optional.of(nameToken.image());
            nameToken = tokens.next(TokenType.IDENTIFIER);
        } else {
            receiver = Optional.empty();
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

    Expression parseExpression(Tokens tokens) {
        return parseAssignment(tokens);
    }

    private Expression parseAssignment(Tokens tokens) {
        var expr = parseComparison(tokens);

        if (tokens.matches(TokenType.EQUALS)) {
            tokens.next();
            if (expr instanceof VariableExpression ve) {
                var right = parseAssignment(tokens);
                expr = new AssignmentExpression(ve, right);
            } else if (expr instanceof ArrayLookupExpression ale) {
                Expression right = parseAssignment(tokens);
                expr = new ArrayAssignmentExpression(ale, right);
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
            case NULL -> {
                tokens.next();
                yield new NullExpression();
            }
            case TRUE, FALSE -> {
                var booleanToken = tokens.next();
                yield new BooleanExpression(booleanToken.type() == TokenType.TRUE);
            }
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
            case LBRACKET -> {
                tokens.next(TokenType.LBRACKET);
                var lengthExpression = Optional.<Expression>empty();
                if (!tokens.matches(TokenType.RBRACKET)) {
                    lengthExpression = Optional.of(parseExpression(tokens));
                }
                tokens.next(TokenType.RBRACKET);
                var typeName = tokens.next(TokenType.IDENTIFIER);
                tokens.next(TokenType.LPAREN);
                List<Expression> initializations = new ArrayList<>();
                while (!tokens.matches(TokenType.RPAREN)) {
                    initializations.add(parseExpression(tokens));
                    if (tokens.matches(TokenType.COMMA)) {
                        tokens.next(TokenType.COMMA);
                    }
                }
                tokens.next(TokenType.RPAREN);
                yield new ArrayCreationExpression(typeName.image(), lengthExpression, initializations);
            }
            case IF -> parseIfExpression(tokens);
            case FOR -> parseForExpression(tokens);
            default -> throw new RuntimeException("Unexpected token: " + token);
        };

        while (tokens.matches(TokenType.DOT, TokenType.LBRACKET)) {
            var postfixToken = tokens.next();
            expression = switch (postfixToken.type()) {
                case TokenType.DOT -> {
                    var invocation = parseNameExpression(tokens);
                    var dotExpression = switch (invocation) {
                        case VariableExpression ve -> {
                            yield new VariableExpression(expression, ve.name());
                        }
                        case FunctionEvaluationExpression fee -> {
                            yield new FunctionEvaluationExpression(expression, fee.name(), fee.arguments());
                        }
                        default -> throw new RuntimeException("Unexpected parse: " + invocation);
                    };
                    yield dotExpression;
                }
                case TokenType.LBRACKET -> {
                    var indexExpression = parseExpression(tokens);
                    tokens.next(TokenType.RBRACKET);
                    yield new ArrayLookupExpression(expression, indexExpression);
                }
                default -> throw new RuntimeException("Unexpected token " + postfixToken);
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

    private Expression parseForExpression(Tokens tokens) {
        tokens.next(TokenType.FOR);
        var condition = parseExpression(tokens);
        var body = parseExpression(tokens);
        return new ForExpression(condition, body);
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

}
