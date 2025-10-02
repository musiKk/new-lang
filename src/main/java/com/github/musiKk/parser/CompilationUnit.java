package com.github.musiKk.parser;

import java.util.List;
import java.util.Optional;

import com.github.musiKk.Tokenizer.TokenType;

public record CompilationUnit(List<Statement> statements) {

    public sealed interface Statement {}
    public record ExpressionStatement(Expression expression) implements Statement {}

    public sealed interface Expression {}

    public record NullExpression() implements Expression {}
    public record BooleanExpression(boolean value) implements Expression {}
    public record AssignmentExpression(VariableExpression target, Expression value) implements Expression {}
    public record ArrayAssignmentExpression(ArrayLookupExpression target, Expression value) implements Expression {}
    public record BlockExpression(List<Statement> statements) implements Expression {}
    public record NumberExpression(long number) implements Expression {}
    public record StringExpression(String string) implements Expression {}
    public record VariableExpression(Optional<Expression> target, String name) implements Expression {
        public VariableExpression(String name) {
            this(Optional.empty(), name);
        }
        public VariableExpression(Expression target, String name) {
            this(Optional.of(target), name);
        }
    }
    public record BinaryExpression(Expression left, TokenType operator, Expression right) implements Expression {}

    public record FunctionEvaluationExpression(Optional<Expression> target, String name, List<Expression> arguments) implements Expression {
        public FunctionEvaluationExpression(String name, List<Expression> arguments) {
            this(Optional.empty(), name, arguments);
        }
        public FunctionEvaluationExpression(Expression target, String name, List<Expression> arguments) {
            this(Optional.of(target), name, arguments);
        }
    }

    public record IfExpression(Expression condition, Expression thenBranch, Optional<Expression> elseBranch) implements Expression {}
    public record ForExpression(Expression condition, Expression body) implements Expression {}

    public record ArrayCreationExpression(String typeName, Optional<Expression> lengthExpression, List<Expression> initializations) implements Expression {}
    public record ArrayLookupExpression(Expression target, Expression indexExpression) implements Expression {}

    public record Import(String name) implements Statement {}

    public sealed interface FunctionDeclaration extends Statement {
        FunctionSignature signature();
    }
    public record NativeFunctionDeclaration(FunctionSignature signature) implements FunctionDeclaration { }
    public record UserFunctionDeclaration(FunctionSignature signature, Expression body) implements FunctionDeclaration {}
    public record TraitDefinition(String name, List<FunctionSignature> functionSignatures) implements Statement {}
    public record FunctionSignature(Optional<String> receiver, String name, List<VariableDeclaration> parameters, String returnType) {}

    public record VariableDeclaration(String name, Optional<String> type, Optional<Expression> initializer) implements Statement {
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

    public record DataDefinition(String name, List<VariableDeclaration> variableDeclarations) implements Statement {}
    public record TraitImplementation(String typeName, String traitName, List<FunctionDeclaration> functionDeclarations) implements Statement {}

}
