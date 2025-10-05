package com.github.musiKk.parser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.github.musiKk.parser.CompilationUnit.AssignmentExpression;
import com.github.musiKk.parser.CompilationUnit.BinaryExpression;
import com.github.musiKk.parser.CompilationUnit.BlockExpression;
import com.github.musiKk.parser.CompilationUnit.DataDefinition;
import com.github.musiKk.parser.CompilationUnit.Expression;
import com.github.musiKk.parser.CompilationUnit.ExpressionStatement;
import com.github.musiKk.parser.CompilationUnit.FunctionDeclaration;
import com.github.musiKk.parser.CompilationUnit.FunctionEvaluationExpression;
import com.github.musiKk.parser.CompilationUnit.FunctionSignature;
import com.github.musiKk.parser.CompilationUnit.NullExpression;
import com.github.musiKk.parser.CompilationUnit.NumberExpression;
import com.github.musiKk.parser.CompilationUnit.Statement;
import com.github.musiKk.parser.CompilationUnit.StringExpression;
import com.github.musiKk.parser.CompilationUnit.UserFunctionDeclaration;
import com.github.musiKk.parser.CompilationUnit.VariableDeclaration;
import com.github.musiKk.parser.CompilationUnit.VariableExpression;
import com.github.musiKk.parser.TCompilationUnit.TAssignmentExpression;
import com.github.musiKk.parser.TCompilationUnit.TBinaryExpression;
import com.github.musiKk.parser.TCompilationUnit.TBlockExpression;
import com.github.musiKk.parser.TCompilationUnit.TDataDefinition;
import com.github.musiKk.parser.TCompilationUnit.TExpression;
import com.github.musiKk.parser.TCompilationUnit.TExpressionStatement;
import com.github.musiKk.parser.TCompilationUnit.TFunctionEvaluationExpression;
import com.github.musiKk.parser.TCompilationUnit.TFunctionSignature;
import com.github.musiKk.parser.TCompilationUnit.TNullExpression;
import com.github.musiKk.parser.TCompilationUnit.TNumberExpression;
import com.github.musiKk.parser.TCompilationUnit.TStatement;
import com.github.musiKk.parser.TCompilationUnit.TStringExpression;
import com.github.musiKk.parser.TCompilationUnit.TUserFunctionDeclaration;
import com.github.musiKk.parser.TCompilationUnit.TVariableDeclaration;
import com.github.musiKk.parser.TCompilationUnit.TVariableExpression;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class AstTyper {
    private final CompilationUnit cu;

    @Getter
    private final FunctionRegistry functionRegistry = new FunctionRegistry();
    private final DataRegistry dataRegistry = new DataRegistry(functionRegistry);

    public TCompilationUnit resolveTypes() {
        collectTypes();
        collectFunctions();

        var scope = Scope.EMPTY;
        return typeCompilationUnit(cu, scope);
    }

    private TCompilationUnit typeCompilationUnit(CompilationUnit cu, Scope scope) {
        return new TCompilationUnit(
            cu.statements().stream()
                .filter(s -> !(s instanceof DataDefinition))
                .map(s -> typeStatement(s, scope))
                .toList()
        );
    }

    private TStatement typeStatement(Statement statement, Scope scope) {
        return switch (statement) {
            case ExpressionStatement es -> new TExpressionStatement(typeExpression(es.expression(), scope));
            case VariableDeclaration vd -> typeVariableDeclaration(vd, scope);
            case UserFunctionDeclaration ufd -> {
                var sig = ufd.signature();
                var receiverType = sig.receiver().map(Type::of);
                var name = sig.name();
                List<TVariableDeclaration> params = new ArrayList<>();
                receiverType.ifPresent(t -> {
                    params.add(new TVariableDeclaration("self", t, Optional.empty()));
                });
                sig.parameters().stream()
                        .forEach(p -> params.add(typeVariableDeclaration(p, scope)));
                var returnType = Type.of(sig.returnType());
                var functionScope = scope.newScope();
                params.stream().forEach(p -> functionScope.vars.put(p.name(), p.type()));
                var body = typeExpression(ufd.body(), functionScope);
                yield new TUserFunctionDeclaration(
                    new TFunctionSignature(receiverType, name, params, returnType),
                    body
                );
            }
            default -> throw new RuntimeException("unsupported statement " + statement);
        };
    }

    private TVariableDeclaration typeVariableDeclaration(VariableDeclaration vd, Scope scope) {
        var initializer = vd.initializer().map(i -> typeExpression(i, scope));
        var type = Type.of(vd.type().get());
        // TODO missing type compatibility check
        var name = vd.name();
        scope.vars.put(name, type);
        return new TVariableDeclaration(name, type, initializer);
    }

    private TExpression typeExpression(Expression expression, Scope scope) {
        return switch (expression) {
            case NullExpression ne -> new TNullExpression();
            case NumberExpression ne -> new TNumberExpression(ne.number());
            case StringExpression se -> new TStringExpression(se.string());
            case VariableExpression ve -> typeVariableExpression(ve, scope);
            case AssignmentExpression ae -> {
                var target = typeVariableExpression(ae.target(), scope);
                var e = typeExpression(ae.value(), scope);
                // TODO missing type compatibility check
                yield new TAssignmentExpression(target, e);
            }
            case BlockExpression be -> {
                var blockScope = scope.newScope();
                List<TStatement> tStatements = new ArrayList<>();
                for (int i = 0; i < be.statements().size() - 1; i++) {
                    tStatements.add(typeStatement(be.statements().get(i), blockScope));
                }
                var returnStatement = be.statements().getLast();
                if (!(returnStatement instanceof ExpressionStatement)) {
                    throw new RuntimeException("block must result in a value but ends in statement " + returnStatement);
                }
                var returnExpression = ((ExpressionStatement) returnStatement).expression();
                var tReturnExpression = typeExpression(returnExpression, blockScope);
                tStatements.add(new TExpressionStatement(tReturnExpression));
                yield new TBlockExpression(tStatements, tReturnExpression.type());
            }
            case FunctionEvaluationExpression fee -> {
                var target = fee.target().map(t -> typeExpression(t, scope));
                var targetType = target.map(TExpression::type);
                var name = fee.name();
                var arguments = fee.arguments().stream()
                        .map(a -> typeExpression(a, scope))
                        .toList();
                var type = functionRegistry.lookup(targetType, name).type;
                yield new TFunctionEvaluationExpression(target, name, arguments, type);
            }
            case BinaryExpression be -> {
                var left = typeExpression(be.left(), scope);
                var right = typeExpression(be.right(), scope);
                // this crudely assumes that operators result in whatever is on the left
                // this should carry us for a while
                yield new TBinaryExpression(left, be.operator(), right, left.type());
            }
            default -> throw new RuntimeException("unsupported expression " + expression);
        };
    }

    private TVariableExpression typeVariableExpression(VariableExpression ve, Scope scope) {
        var target = ve.target().map(t -> typeExpression(t, scope));
        var name = ve.name();
        if (target.isPresent()) {
            var targetExpression = target.get();
            var targetType = targetExpression.type();
            var type = dataRegistry.lookupFieldType(targetType, name);
            return new TVariableExpression(Optional.of(targetExpression), name, type);
        } else {
            return new TVariableExpression(target, name, scope.lookupType(ve.name()));
        }
    }

    private void collectFunctions() {
        cu.statements().stream()
                .<FunctionDeclaration>mapMulti((s, consumer) -> {
                    if (s instanceof FunctionDeclaration fd) {
                        consumer.accept(fd);
                    }
                })
                .map(FunctionDeclaration::signature)
                .forEach(functionRegistry::registerFunction);
    }

    public void addPrototype(FunctionSignature sig) {
        functionRegistry.registerFunction(sig);
    }

    static class FunctionRegistry {
        Map<CallPair, Function> map = new HashMap<>();

        public void registerFunction(FunctionSignature sig) {
            var callPair = new CallPair(
                sig.receiver().map(Type::of),
                sig.name()
            );
            if (map.containsKey(callPair)) {
                throw new RuntimeException("function " + callPair + " already defined");
            }
            var function = new Function(
                    sig.receiver().map(Type::of),
                    sig.name(),
                    Type.of(sig.returnType()),
                    sig.parameters().stream()
                            .map(p -> new Function.Parameter(p.name(), Type.of(p.type().get())))
                            .toList());
            map.put(callPair, function);
        }

        Function lookup(Optional<Type> receiver, String name) {
            System.err.println("looking up " + receiver + "." + name);
            return map.get(new CallPair(receiver, name));
        }

        private record CallPair(Optional<Type> receiver, String name) {}

        record Function(
                Optional<Type> receiver,
                String name,
                Type type,
                List<Parameter> parameters) {
            record Parameter(String name, Type type) {}
        }
    }

    void collectTypes() {
        cu.statements().stream()
                .<DataDefinition>mapMulti((s, consumer) -> {
                    if (s instanceof DataDefinition dd) {
                        consumer.accept(dd);
                    }
                })
                .forEach(dataRegistry::register);
    }

    @RequiredArgsConstructor
    static class DataRegistry {
        final FunctionRegistry functionRegistry;
        final Map<Type, TDataDefinition> map = new HashMap<>();

        void register(DataDefinition dd) {
            var name = dd.name();
            var varDecls = dd.variableDeclarations().stream()
                    .map(vd -> new TVariableDeclaration(
                            vd.name(),
                            Type.of(vd.type().get()),
                            // TODO second pass for initializers
                            // this does not allow for typing the initializers
                            // needs a second pass after all types and functions
                            // are collected and the scope is established
                            Optional.empty()))
                    .toList();
            map.put(Type.of(name), new TDataDefinition(name, varDecls));

            functionRegistry.map.put(
                    new FunctionRegistry.CallPair(Optional.empty(), name),
                    new FunctionRegistry.Function(Optional.empty(),name, Type.of(name), varDecls.stream().map(vd -> new FunctionRegistry.Function.Parameter(vd.name(), vd.type())).toList()));
        }

        Type lookupFieldType(Type type, String name) {
            System.err.println("looking up " + type + "." + name);
            // TODO look ma, no error handling
            return map.get(type)
                    .variableDeclarations().stream()
                            .filter(vd -> vd.name().equals(name))
                            .findFirst().get().type();
        }
    }

    @RequiredArgsConstructor
    static class Scope {
        final Scope parentScope;

        static final Scope EMPTY = new Scope(null) {
            Type lookupType(String name) {
                throw new RuntimeException("name " + name + " not found");
            }
        };

        Scope newScope() {
            return new Scope(this);
        }

        final Map<String, Type> vars = new HashMap<>();
        Type lookupType(String name) {
            var result = vars.get(name);
            if (result == null) {
                return parentScope.lookupType(name);
            }
            return result;
        }
    }

}
