package com.github.musiKk.parser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import com.github.musiKk.Tokenizer.TokenType;
import com.github.musiKk.parser.CompilationUnit.AssignmentExpression;
import com.github.musiKk.parser.CompilationUnit.BinaryExpression;
import com.github.musiKk.parser.CompilationUnit.BlockExpression;
import com.github.musiKk.parser.CompilationUnit.BooleanExpression;
import com.github.musiKk.parser.CompilationUnit.DataDefinition;
import com.github.musiKk.parser.CompilationUnit.Expression;
import com.github.musiKk.parser.CompilationUnit.ExpressionStatement;
import com.github.musiKk.parser.CompilationUnit.FunctionDeclaration;
import com.github.musiKk.parser.CompilationUnit.FunctionEvaluationExpression;
import com.github.musiKk.parser.CompilationUnit.FunctionSignature;
import com.github.musiKk.parser.CompilationUnit.IfExpression;
import com.github.musiKk.parser.CompilationUnit.Import;
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
import com.github.musiKk.parser.TCompilationUnit.TBooleanExpression;
import com.github.musiKk.parser.TCompilationUnit.TDataDefinition;
import com.github.musiKk.parser.TCompilationUnit.TExpression;
import com.github.musiKk.parser.TCompilationUnit.TExpressionStatement;
import com.github.musiKk.parser.TCompilationUnit.TFunctionDeclaration;
import com.github.musiKk.parser.TCompilationUnit.TFunctionEvaluationExpression;
import com.github.musiKk.parser.TCompilationUnit.TFunctionSignature;
import com.github.musiKk.parser.TCompilationUnit.TIfExpression;
import com.github.musiKk.parser.TCompilationUnit.TNullExpression;
import com.github.musiKk.parser.TCompilationUnit.TNumberExpression;
import com.github.musiKk.parser.TCompilationUnit.TStatement;
import com.github.musiKk.parser.TCompilationUnit.TStringExpression;
import com.github.musiKk.parser.TCompilationUnit.TUserFunctionDeclaration;
import com.github.musiKk.parser.TCompilationUnit.TVariableDeclaration;
import com.github.musiKk.parser.TCompilationUnit.TVariableExpression;

import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class AstTyper {
    private final CompilationUnitLoader cuLoader;

    private final FunctionRegistry functionRegistry = new FunctionRegistry();
    private final DataRegistry dataRegistry = new DataRegistry(functionRegistry);

    public TCompilationUnit typeProgram(String name) {
        var cu = cuLoader.load(name);
        var c = new Object() {
            List<DataDefinition> dds = new ArrayList<>();
            List<FunctionDeclaration> fds = new ArrayList<>();
            List<Statement> stmts = new ArrayList<>();
        };
        cu.statements().stream()
                .forEach(s -> {
                    switch (s) {
                        case DataDefinition dd -> c.dds.add(dd);
                        case FunctionDeclaration fd -> c.fds.add(fd);
                        default -> c.stmts.add(s);
                    };
                });

        var scope = Scope.empty(name).newScope();
        collectTypes(c.dds, scope);
        collectFunctions(c.fds, scope);

        var tStmts = typeStatements(c.stmts, scope);

        return new TCompilationUnit(
                new ArrayList<>(dataRegistry.map.values()),
                functionRegistry.map.values().stream()
                        .filter(f -> f.functionDeclaration != null)
                        .map(FunctionRegistry.Function::functionDeclaration)
                        .map(f -> typeFunctionDeclaration(f, scope))
                        .toList(),
                tStmts);
    }

    private List<TStatement> typeStatements(List<Statement> statements, Scope scope) {
        return statements.stream()
                .map(s -> typeStatement(s, scope))
                .toList();
    }

    private TStatement typeStatement(Statement statement, Scope scope) {
        return switch (statement) {
            case ExpressionStatement es -> new TExpressionStatement(typeExpression(es.expression(), scope));
            case VariableDeclaration vd -> typeVariableDeclaration(vd, scope);
            case DataDefinition dd -> dataRegistry.map.get(scope.lookupType(dd.name()));
            case Import(var name) -> {
                // 1. check if name is already loaded
                // 2. if not, load which will populate the function and data registry
                // 3. add the module to the scope so that its names are available for lookup
                yield null;
            }
            default -> throw new RuntimeException("unsupported statement " + statement);
        };
    }

    private TFunctionDeclaration typeFunctionDeclaration(UserFunctionDeclaration ufd, Scope scope) {
        var sig = ufd.signature();
        var receiverType = sig.receiver().map(scope::lookupType);
        var name = sig.name();
        List<TVariableDeclaration> params = new ArrayList<>();
        receiverType.ifPresent(t -> {
            params.add(new TVariableDeclaration("self", t, Optional.empty()));
        });
        sig.parameters().stream()
                .forEach(p -> params.add(typeVariableDeclaration(p, scope)));
        var returnType = scope.lookupType(sig.returnType());
        var functionScope = scope.newScope();
        params.stream().forEach(p -> functionScope.put(p.name(), p.type()));
        var body = typeExpression(ufd.body(), functionScope);
        return new TUserFunctionDeclaration(
            new TFunctionSignature(receiverType, name, params, returnType),
            body
        );
    }

    private TVariableDeclaration typeVariableDeclaration(VariableDeclaration vd, Scope scope) {
        var initializer = vd.initializer().map(i -> typeExpression(i, scope));
        var type = scope.lookupType(vd.type().get());
        initializer.ifPresent(i -> {
            if (i.type() != type) {
                throw new RuntimeException("incompatible types " + i.type() + " <-> " + type);
            }
        });
        var name = vd.name();
        scope.put(name, type);
        return new TVariableDeclaration(name, type, initializer);
    }

    private TExpression typeExpression(Expression expression, Scope scope) {
        return switch (expression) {
            case NullExpression _ -> new TNullExpression();
            case NumberExpression ne -> new TNumberExpression(ne.number());
            case StringExpression se -> new TStringExpression(se.string());
            case BooleanExpression be -> new TBooleanExpression(be.value());
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
                var type = functionRegistry.lookup(scope.module, targetType, name).type;
                yield new TFunctionEvaluationExpression(target, name, arguments, type);
            }
            case BinaryExpression be -> {
                var left = typeExpression(be.left(), scope);
                var right = typeExpression(be.right(), scope);
                var operator = be.operator();

                // this crudely assumes that operators result in whatever is on the left
                // for non comparisons. this should carry us for a while
                var resultType = BOOLEAN_OPERATORS.contains(operator) ? Type.Builtin.BOOL : left.type();
                yield new TBinaryExpression(left, operator, right, resultType);
            }
            case IfExpression ie -> {
                var condition = typeExpression(ie.condition(), scope);
                if (condition.type() != Type.Builtin.BOOL) {
                    throw new RuntimeException("if condition must be of type Bool " + condition);
                }
                var thenExpression = typeExpression(ie.thenBranch(), scope);
                var elseExpression = ie.elseBranch().map(e -> typeExpression(e, scope));
                var resultType = elseExpression.map(e -> {
                    if (e.type() != thenExpression.type()) {
                        return Type.Builtin.ANY;
                    }
                    return e.type();
                }).orElse(thenExpression.type());
                yield new TIfExpression(condition, thenExpression, elseExpression, resultType);
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

    private void collectFunctions(List<FunctionDeclaration> functionDeclarations, Scope scope) {
        functionDeclarations.stream()
                .forEach(fd -> functionRegistry.registerFunction(fd, scope));
    }

    public void addPrototype(FunctionSignature sig, Scope scope) {
        functionRegistry.registerPrototype(sig, scope);
    }

    static class FunctionRegistry {
        Map<CallPair, Function> map = new HashMap<>();

        public void registerPrototype(FunctionSignature sig, Scope scope) {
            registerFunction(null, sig, scope);
        }

        public void registerFunction(FunctionDeclaration fd, Scope scope) {
            registerFunction(fd, fd.signature(), scope);
        }

        private void registerFunction(FunctionDeclaration fd, FunctionSignature sig, Scope scope) {
            var receiverType = sig.receiver().map(scope::lookupType);

            var callPair = new CallPair(
                scope.module,
                receiverType,
                sig.name()
            );
            if (map.containsKey(callPair)) {
                throw new RuntimeException("function " + callPair + " already defined");
            }

            UserFunctionDeclaration ufd = null;
            if (fd instanceof UserFunctionDeclaration) {
                ufd = (UserFunctionDeclaration) fd;
            }

            var function = new Function(
                    sig.receiver().map(scope::lookupType),
                    sig.name(),
                    scope.lookupType(sig.returnType()),
                    sig.parameters().stream()
                            .map(p -> new Function.Parameter(p.name(), scope.lookupType(p.type().get())))
                            .toList(),
                    ufd);
            map.put(callPair, function);
        }

        Function lookup(String module, Optional<Type> receiver, String name) {
            return map.get(new CallPair(module, receiver, name));
        }

        private record CallPair(String module, Optional<Type> receiver, String name) {}

        record Function(
                Optional<Type> receiver,
                String name,
                Type type,
                List<Parameter> parameters,
                UserFunctionDeclaration functionDeclaration) {
            record Parameter(String name, Type type) {}
        }
    }

    void collectTypes(List<DataDefinition> dataDefinitions, Scope scope) {
        dataDefinitions.stream().forEach(dd -> {
            var name = dd.name();
            var dataType = Type.of(scope.module, name);
            scope.put(name, dataType);
        });
        dataDefinitions.stream().forEach(dd -> dataRegistry.register(dd, scope));
    }

    @RequiredArgsConstructor
    static class DataRegistry {
        final FunctionRegistry functionRegistry;
        final Map<Type, TDataDefinition> map = new HashMap<>();

        void register(DataDefinition dd, Scope scope) {
            var name = dd.name();
            var varDecls = dd.variableDeclarations().stream()
                    .map(vd -> new TVariableDeclaration(
                            vd.name(),
                            scope.lookupType(vd.type().get()),
                            // TODO second pass for initializers
                            // this does not allow for typing the initializers
                            // needs a second pass after all types and functions
                            // are collected and the scope is established
                            Optional.empty()))
                    .toList();
            Type dataType = Type.of(scope.module, name);
            map.put(dataType, new TDataDefinition(name, varDecls, Type.of(scope.module, name)));

            functionRegistry.registerPrototype(new FunctionSignature(
                Optional.empty(), name, dd.variableDeclarations(), name
            ), scope);
        }

        Type lookupFieldType(Type type, String name) {
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
        final String module;

        static Scope empty(String module) {
            return new Scope(null, module) {
                Type lookupType(String name) {
                    return switch (name) {
                        case "Int", "Void", "String" -> Type.of("core", name);
                        default -> throw new RuntimeException("name " + name + " not found");
                    };
                }
            };
        }

        Scope newScope() {
            return new Scope(this, this.module);
        }

        // split into var, type, function names?
        final Map<String, Type> names = new HashMap<>();
        Type lookupType(String name) {
            var result = names.get(name);
            if (result == null) {
                return parentScope.lookupType(name);
            }
            return result;
        }

        public void put(String name, Type dataType) {
            names.put(name, dataType);
        }
    }

    private static final Set<TokenType> BOOLEAN_OPERATORS = Set.of(
            TokenType.EQUALS_EQUALS, TokenType.NOT_EQUALS,
            TokenType.LT, TokenType.GT, TokenType.LE, TokenType.GE
    );

    public static interface CompilationUnitLoader {
        CompilationUnit load(String name);
    }

}
