package com.github.musiKk.parser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

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
import lombok.ToString;

@RequiredArgsConstructor
public class AstTyper {
    private final CompilationUnitLoader cuLoader;

    private final FunctionRegistry functionRegistry = new FunctionRegistry();
    private final DataRegistry dataRegistry = new DataRegistry(functionRegistry);

    private final TyperResults typerResults = new TyperResults();

    public Map<String, TCompilationUnit> typeProgram(String name) {
        var tr = typeModule(name);
        typerResults.putMainModule(name, tr);
        return typerResults.map.entrySet().stream()
                .collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue().tcu));
    }

    public TyperResult typeModule(String name) {
        var cu = cuLoader.load(name);
        var c = new Object() {
            List<DataDefinition> dds = new ArrayList<>();
            List<FunctionDeclaration> fds = new ArrayList<>();
            List<Statement> stmts = new ArrayList<>();
        };
        List<String> imports = new ArrayList<>();
        cu.statements().stream()
                .forEach(s -> {
                    switch (s) {
                        case DataDefinition dd -> c.dds.add(dd);
                        case FunctionDeclaration fd -> c.fds.add(fd);
                        case Import i -> {
                            imports.add(i.name());
                            c.stmts.add(i);
                        }
                        default -> c.stmts.add(s);
                    };
                });

        resolveImports(imports);

        var scope = Scope.empty(this, name).newScope();
        scope.importedModules.addAll(imports);
        collectTypes(c.dds, scope);
        collectFunctions(c.fds, scope);

        var tDds = dataRegistry.map.values().stream()
                // XXX hack
                .filter(tdd -> tdd.type().module().equals(name))
                .toList();
        var tStmts = typeStatements(c.stmts, scope);
        var tFds = functionRegistry.map.entrySet().stream()
                // XXX hack
                .filter(e -> e.getKey().module().equals(name))
                .map(Map.Entry::getValue)
                .filter(f -> f.functionDeclaration != null)
                .map(FunctionRegistry.Function::functionDeclaration)
                .map(f -> typeFunctionDeclaration(f, scope))
                .toList();

        return new TyperResult(scope, new TCompilationUnit(tDds, tFds, tStmts));
    }

    private void resolveImports(List<String> modules) {
        modules.stream().forEach(this::resolveModule);
    }

    private void resolveModule(String name) {
        if (typerResults.isLoaded(name)) return;
        var tr = typeModule(name);
        typerResults.put(name, tr);
    }

    private List<TStatement> typeStatements(List<Statement> statements, Scope scope) {
        return statements.stream()
                .map(s -> typeStatement(s, scope))
                .filter(s -> s != null)
                .toList();
    }

    private TStatement typeStatement(Statement statement, Scope scope) {
        return switch (statement) {
            case ExpressionStatement es -> new TExpressionStatement(typeExpression(es.expression(), scope));
            case VariableDeclaration vd -> typeVariableDeclaration(vd, scope);
            case DataDefinition dd -> dataRegistry.map.get(scope.lookupType(dd.name()));
            case Import(var name) -> {
                // this is how it should be but it throws b/c it modifies the
                // function registry while iterating over it in typeModule
                // scope.importedModules.add(name);
                // resolveImport(name);

                // XXX ugh, hack
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

                var fType = scope.lookupType(new Scope.TargetAndName(targetType, name));
                var type = functionRegistry.lookup(fType.module(), targetType, name).type;
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

            scope.put(
                    new Scope.TargetAndName(receiverType, sig.name()),
                    Type.of(
                            scope.module,
                            sig.receiver().map(scope::lookupType),
                            sig.name(),
                            scope.lookupType(sig.returnType()),
                            sig.parameters().stream().map(p -> scope.lookupType(p.type().get())).toList()));
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

    @ToString
    @RequiredArgsConstructor
    class Scope {
        final Scope parentScope;
        final String module;
        final List<String> importedModules = new ArrayList<>();

        static Scope empty(AstTyper typer, String module) {
            return typer.new Scope(null, module) {
                Type lookupType(String name) {
                    return switch (name) {
                        case "Int", "Void", "String" -> Type.of("core", name);
                        default -> throw new RuntimeException("name " + name + " not found");
                    };
                }
                Type lookupType(TargetAndName tan) {
                    throw new RuntimeException("function " + tan + " not found");
                }
            };
        }

        Scope newScope() {
            return new Scope(this, this.module);
        }

        // types and variables
        final Map<String, Type> names = new HashMap<>();
        // functions
        final Map<TargetAndName, Type> functions = new HashMap<>();

        Type lookupType(String name) {
            var result = names.get(name);
            if (result == null) {
                for (var m : importedModules) {
                    var ms = typerResults.map.get(m).scope;
                    try {
                        return ms.lookupType(name);
                    } catch (Exception e) {}
                }
                return parentScope.lookupType(name);
            }
            return result;
        }

        Type lookupType(TargetAndName tan) {
            var result = functions.get(tan);
            if (result == null) {
                for (var m : importedModules) {
                    var ms = typerResults.map.get(m).scope;
                    try {
                        return ms.lookupType(tan);
                    } catch (Exception e) {}
                }
                return parentScope.lookupType(tan);
            }
            return result;
        }

        public void put(String name, Type dataType) {
            names.put(name, dataType);
        }
        public void put(TargetAndName tan, Type functionType) {
            functions.put(tan, functionType);
        }

        static record TargetAndName(Optional<Type> target, String name) {}
    }

    private static final Set<TokenType> BOOLEAN_OPERATORS = Set.of(
            TokenType.EQUALS_EQUALS, TokenType.NOT_EQUALS,
            TokenType.LT, TokenType.GT, TokenType.LE, TokenType.GE
    );

    public static interface CompilationUnitLoader {
        CompilationUnit load(String name);
    }

    @ToString
    public static class TyperResults {
        private final Map<String, TyperResult> map = new HashMap<>();
        private String mainModule;

        void putMainModule(String module, TyperResult tr) {
            mainModule = module;
            put(module, tr);
        }

        public boolean isLoaded(String name) {
            return map.containsKey(name);
        }

        void put(String module, TyperResult tr) {
            map.put(module, tr);
        }
    }
    public static record TyperResult(Scope scope, TCompilationUnit tcu) {}

}
