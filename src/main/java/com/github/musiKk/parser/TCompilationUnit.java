package com.github.musiKk.parser;

import java.util.List;
import java.util.Optional;

import com.github.musiKk.Tokenizer.TokenType;

public record TCompilationUnit(
        List<TDataDefinition> dataDefinitions,
        List<TFunctionDeclaration> functionDeclarations,
        List<TStatement> statements) {

    public sealed interface TStatement {}
    public record TExpressionStatement(TExpression expression) implements TStatement {}

    public sealed interface TExpression {
        Type type();
    }

    public record TNullExpression() implements TExpression {
        public Type type() { return Type.Builtin.ANY; }
    }
    public record TBooleanExpression(boolean value) implements TExpression {
        public Type type() { return Type.Builtin.BOOL; }
    }
    public record TAssignmentExpression(TVariableExpression target, TExpression value) implements TExpression {
        public Type type() { return value.type(); }
    }
    public record TArrayAssignmentExpression(TArrayLookupExpression target, TExpression value) implements TExpression {
        public Type type() { return value.type(); }
    }
    public record TBlockExpression(List<TStatement> statements, Type type) implements TExpression {}
    public record TNumberExpression(long number) implements TExpression {
        public Type type() { return Type.Builtin.INT; }
    }
    public record TStringExpression(String string) implements TExpression {
        public Type type() { return Type.Builtin.STRING; }
    }
    public record TVariableExpression(Optional<TExpression> target, String name, Type type) implements TExpression {}
    public record TBinaryExpression(TExpression left, TokenType operator, TExpression right, Type type) implements TExpression {}

    public record TFunctionEvaluationExpression(Optional<TExpression> target, String name, List<TExpression> arguments, Type type) implements TExpression {}

    public record TIfExpression(TExpression condition, TExpression thenBranch, Optional<TExpression> elseBranch, Type type) implements TExpression {}
    public record TForExpression(TExpression condition, TExpression body, Type type) implements TExpression {}

    public record TArrayCreationExpression(String typeName, Optional<TExpression> lengthExpression, List<TExpression> initializations, Type type) implements TExpression {}
    public record TArrayLookupExpression(TExpression target, TExpression indexExpression, Type type) implements TExpression {}

    public sealed interface TFunctionDeclaration extends TStatement {
        TFunctionSignature signature();
    }
    public record TNativeFunctionDeclaration(TFunctionSignature signature) implements TFunctionDeclaration { }
    public record TUserFunctionDeclaration(TFunctionSignature signature, TExpression body) implements TFunctionDeclaration {}
    public record TTraitDefinition(String name, List<TFunctionSignature> functionSignatures) implements TStatement {}
    public record TFunctionSignature(Optional<Type> receiver, String name, List<TVariableDeclaration> parameters, Type returnType) {}

    public record TVariableDeclaration(String name, Type type, Optional<TExpression> initializer) implements TStatement {}

    public record TDataDefinition(String name, List<TVariableDeclaration> variableDeclarations, Type type) implements TStatement {}
    public record TTraitImplementation(String typeName, String traitName, List<TFunctionDeclaration> functionDeclarations) implements TStatement {}
}
