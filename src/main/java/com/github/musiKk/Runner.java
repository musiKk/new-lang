package com.github.musiKk;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.github.musiKk.Tokenizer.TokenType;

public class Runner implements ConfigReader.ConfigTarget {

    Map<Path, RuntimeFile> runtimeFiles = new HashMap<>();
    private List<String> lookupPath = new ArrayList<>();

    @Override
    public void setLookupPath(List<String> lookupPath) {
        this.lookupPath = lookupPath;
    }

    public void runFile(String file) {
        var mainStackFrame = new StackFrame();
        var runtimeFile = this.getOrInitRuntimeFile("main", file, mainStackFrame);
        var main = runtimeFile.scope.getVariable("main");
        if (main != null && main.variable.type() == Type.FUNCTION) {
            this.evaluateFunction(new FunctionEvaluationExpression("main", List.of()), new StackFrame(null, main.scope));
        }

    }

    public static void main(String[] args) {
        var runner = new Runner();
        ConfigReader.readConfig().applyConfig(runner);
        runner.runFile("test.tst");
    }

    private RuntimeFile getOrInitRuntimeFile(String moduleName, String pathString, StackFrame frame) {
        var resolvedPath = Path.of(pathString);
        if (!resolvedPath.isAbsolute()) {
            for (String lookupPathEntry : lookupPath) {
                var path = Path.of(lookupPathEntry, pathString);
                if (Files.isRegularFile(path)) {
                    resolvedPath = path;
                    break;
                }
            }
        }
        if (!runtimeFiles.containsKey(resolvedPath)) {
            var runtimeFile = initRuntimeFile(moduleName, resolvedPath, frame.pushFrame(new Scope()));
            runtimeFiles.put(resolvedPath, runtimeFile);
        }
        return runtimeFiles.get(resolvedPath);
    }

    private void execute(List<Statement> statements, StackFrame frame) {
        for (var statement : statements) {
            execute(statement, frame);
        }
    }

    private Value evaluateExpression(Expression expression, StackFrame frame) {
        return switch (expression) {
            case NumberExpression ne -> new NumberValue(ne.number());
            case StringExpression se -> new StringValue(se.string());
            case VariableExpression ve -> {
                var optTarget = ve.target();
                Optional<Value> optReceiver = optTarget.map(t -> evaluateExpression(t, frame));
                yield optReceiver.map(receiver -> {
                    if (receiver instanceof Data dv) {
                        return dv.variables().get(ve.name());
                    } else {
                        throw new RuntimeException("cannot look up fields in values " + receiver);
                    }
                }).orElse(frame.getVariable(ve.name()).variable.value());
            }
            case FunctionEvaluationExpression fe -> {
                yield evaluateFunction(fe, frame);
            }
            case BlockExpression be -> {
                frame.pushScope();
                try {
                    Value result = null;
                    for (var statement : be.statements()) {
                        result = execute(statement, frame);
                    }
                    yield result;
                } finally {
                    frame.popScope();
                }
            }
            case BinaryExpression be -> {
                yield switch (be.operator()) {
                    case TokenType.PLUS -> {
                        var left = evaluateExpression(be.left(), frame);
                        var right = evaluateExpression(be.right(), frame);
                        if (left instanceof NumberValue ln && right instanceof NumberValue rn) {
                            yield new NumberValue(ln.number() + rn.number());
                        } else {
                            yield new StringValue(left.toString() + right.toString());
                        }
                    }
                    case TokenType.MINUS -> {
                        var left = (NumberValue) evaluateExpression(be.left(), frame);
                        var right = (NumberValue) evaluateExpression(be.right(), frame);
                        yield new NumberValue(left.number() - right.number());
                    }
                    case TokenType.STAR -> {
                        var left = (NumberValue) evaluateExpression(be.left(), frame);
                        var right = (NumberValue) evaluateExpression(be.right(), frame);
                        yield new NumberValue(left.number() * right.number());
                    }
                    case TokenType.SLASH -> {
                        var left = (NumberValue) evaluateExpression(be.left(), frame);
                        var right = (NumberValue) evaluateExpression(be.right(), frame);
                        yield new NumberValue(left.number() / right.number());
                    }
                    case TokenType.EQUALS_EQUALS -> {
                        var left = evaluateExpression(be.left(), frame);
                        var right = evaluateExpression(be.right(), frame);
                        yield left.equals(right) ? True : False;
                    }
                    case TokenType.NOT_EQUALS -> {
                        var left = evaluateExpression(be.left(), frame);
                        var right = evaluateExpression(be.right(), frame);
                        yield left.equals(right) ? False : True;
                    }
                    case TokenType.LT -> {
                        var left = (NumberValue) evaluateExpression(be.left(), frame);
                        var right = (NumberValue) evaluateExpression(be.right(), frame);
                        yield left.number() < right.number() ? True : False;
                    }
                    case TokenType.GT -> {
                        var left = (NumberValue) evaluateExpression(be.left(), frame);
                        var right = (NumberValue) evaluateExpression(be.right(), frame);
                        yield left.number() > right.number() ? True : False;
                    }
                    case TokenType.LE -> {
                        var left = (NumberValue) evaluateExpression(be.left(), frame);
                        var right = (NumberValue) evaluateExpression(be.right(), frame);
                        yield left.number() <= right.number() ? True : False;
                    }
                    case TokenType.GE -> {
                        var left = (NumberValue) evaluateExpression(be.left(), frame);
                        var right = (NumberValue) evaluateExpression(be.right(), frame);
                        yield left.number() >= right.number() ? True : False;
                    }
                    default -> throw new RuntimeException("not yet implemented " + be.operator());
                };
            }
            case IfExpression ie -> {
                var condition = evaluateExpression(ie.condition(), frame);
                if (condition instanceof BooleanValue bv) {
                    if (bv.value()) {
                        yield evaluateExpression(ie.thenBranch(), frame);
                    } else {
                        yield ie.elseBranch().map(elseExpression -> evaluateExpression(elseExpression, frame)).orElse(null);
                    }
                } else {
                    throw new RuntimeException("condition must be a boolean");
                }
            }
            case AssignmentExpression ae -> {
                var lhs = ae.target();
                var rhs = evaluateExpression(ae.value(), frame);
                if (lhs.target().isPresent()) {
                    // XXX causes VerifyError
                    // switch (evaluateExpression(lhs.target().get(), frame)) {
                    //     case Data d -> d.variables().put(lhs.name(), rhs);
                    //     default -> throw new RuntimeException("cannot assign to " + lhs);
                    // }
                    var destination = evaluateExpression(lhs.target().get(), frame);
                    if (destination instanceof Data data) {
                        data.variables.put(lhs.name(), rhs);
                    } else {
                        throw new RuntimeException("cannot assign to " + lhs);
                    }
                } else {
                    try {
                        ScopedVariable sv = frame.getVariable(lhs.name());
                        sv.scope.putVariable(lhs.name(), new Variable(rhs));
                    } catch (SymbolNotFoundException e) {
                        frame.putVariable(lhs.name(), new Variable(rhs));
                    }
                }
                yield rhs;
            }
            default -> throw new RuntimeException("not yet implemented " + expression);
        };
    }

    private Value evaluateFunction(FunctionEvaluationExpression fe, StackFrame frame) {
        interface EvaluationTarget { String name(); }
        record DataTarget(Data data) implements EvaluationTarget {
            public String name() {
                return data.typeName;
            }
        }
        record UfcsTarget(String name) implements EvaluationTarget {}

        Optional<EvaluationTarget> optTarget = fe.target()
            .map(targetExpression -> {
                var targetValue = evaluateExpression(targetExpression, frame);
                return switch (targetValue) {
                    case Data data -> new DataTarget(data);
                    case DataCreationFunction dcf -> new UfcsTarget(dcf.name);
                    default -> throw new RuntimeException("cannot call functions on " + targetValue.type());
                };
            });

        var functionLookupName = optTarget.map(t -> t.name() + ".").orElse("") + fe.name();

        final Function function;
        final Scope functionScope;
        final List<Parameter> parameters;
        switch (frame.getVariable(functionLookupName).variable.value) {
            case Function f -> {
                function = f;
                functionScope = f.scope();
                parameters = f.parameters();
            }
            default -> throw new RuntimeException("not a function " + functionLookupName);
        }

        var newStackFrame = frame.pushFrame(functionScope).pushScope();
        optTarget.map(target -> {
            return switch (target) {
                case DataTarget dt -> dt.data;
                case UfcsTarget __ -> evaluateExpression(fe.arguments().get(0), frame);
                default -> null;
            };
        }).ifPresent(target -> {
            newStackFrame.putVariable("this", new Variable(target));
        });

        int firstIndex = optTarget.isPresent() && optTarget.get() instanceof UfcsTarget ? 1 : 0;
        for (int i = firstIndex; i < fe.arguments().size(); i++) {
            var argument = fe.arguments().get(i);
            var value = evaluateExpression(argument, frame);
            var name = parameters.get(i).name();
            newStackFrame.putVariable(name, new Variable(value));
        }

        return switch (function) {
            case DataCreationFunction dataCreationFunction -> {
                yield createData(dataCreationFunction, newStackFrame);
            }
            case UserFunction uf -> {
                yield evaluateExpression(uf.body(), newStackFrame);
            }
            case NativeFunction nf -> {
                var nativeFunctionHandle = nf.handle;
                yield nativeFunctionHandle.call(newStackFrame);
            }
        };
    }

    private Data createData(DataCreationFunction dataCreationFunction, StackFrame frame) {
        Map<String, Value> variables = new HashMap<>();
        dataCreationFunction.parameters().forEach(parameter -> {
            var val = frame.getVariable(parameter.name()).variable.value;
            variables.put(parameter.name(), val);
        });
        return new Data(dataCreationFunction.name(), variables);
    }

    private Value execute(Statement expression, StackFrame frame) {
        return switch (expression) {
            /*
             * This should be
             *     case ExpressionStatement(Expression e) -> evaluateExpression(e, frame);
             * but record pattern matching is thwarted by this bug: https://github.com/redhat-developer/vscode-java/issues/3479
             */
            case ExpressionStatement es -> evaluateExpression(es.expression(), frame);
            case VariableDeclaration vds -> {
                var optValue = vds.initializer().map(initializer -> evaluateExpression(initializer, frame));
                frame.putVariable(vds.name(), new Variable(optValue.orElse(null)));
                yield null;
            }
            case Import imp -> {
                processImport(imp, frame);
                yield null;
            }
            case DataDefinition dd -> {
                processDataDefinition(dd, frame.scope);
                yield null;
            }
            default -> throw new RuntimeException("not yet implemented " + expression);
        };
    }

    private void processImport(Import imp, StackFrame frame) {
        var runtimeFile = getOrInitRuntimeFile(imp.name(), imp.name() + ".tst", frame);
        frame.scope().addImportScope(imp.name(), runtimeFile.scope);
    }

    private RuntimeFile initRuntimeFile(String moduleName, Path path, StackFrame frame) {
        String fileContent;
        try {
            fileContent = Files.readString(path);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        var compilationUnit = new Parser().parseCompilationUnit(new Tokenizer().tokenize(fileContent));

        RuntimeFile runtimeFile = new RuntimeFile(frame.scope);

        var processingResult = compilationUnit.statements().stream()
                .reduce(new CompilationUnitProcessingResult(), (result, statement) -> switch (statement) {
                    case DataDefinition dataDefinition -> result.addDataDefinition(dataDefinition);
                    case FunctionDeclaration functionDeclaration -> result.addFunctionDeclaration(functionDeclaration);
                    case VariableDeclaration variableDeclaration -> result.addVariableDeclaration(variableDeclaration);
                    default -> result.addStatement(statement);
                }, (r1, r2) -> {
                    r1.statements.addAll(r2.statements);
                    r1.dataDefinitions.putAll(r2.dataDefinitions);
                    r1.functionDeclarations.putAll(r2.functionDeclarations);
                    r1.variableDeclarations.putAll(r2.variableDeclarations);
                    return r1;
                });

        processingResult.dataDefinitions().forEach((name, dataDefinition) -> {
            processDataDefinition(dataDefinition, frame.scope);
        });
        processingResult.functionDeclarations().forEach((__, functionDeclaration) -> {
            var functionName = functionDeclaration.name();
            var lookupName = switch (functionDeclaration.receiver()) {
                case FunctionReceiverVariable(String name) -> name + "." + functionName;
                default -> functionName;
            };
            var function = Function.of(moduleName, functionDeclaration, runtimeFile.scope());
            frame.putVariable(lookupName, new Variable(Type.FUNCTION, function));
        });
        processingResult.variableDeclarations().forEach((name, variableDeclaration) -> {
            execute(variableDeclaration, frame);
        });

        execute(processingResult.statements(), frame.pushFrame(runtimeFile.scope));

        return runtimeFile;
    }

    record CompilationUnitProcessingResult(
        List<Statement> statements,
        Map<String, DataDefinition> dataDefinitions,
        Map<String, VariableDeclaration> variableDeclarations,
        Map<String, FunctionDeclaration> functionDeclarations
    ) {
        public CompilationUnitProcessingResult() {
            this(new ArrayList<>(), new HashMap<>(), new HashMap<>(), new HashMap<>());
        }
        CompilationUnitProcessingResult addStatement(Statement statement) {
            statements.add(statement);
            return this;
        }
        CompilationUnitProcessingResult addDataDefinition(DataDefinition dataDefinition) {
            dataDefinitions.put(dataDefinition.name(), dataDefinition);
            return this;
        }
        CompilationUnitProcessingResult addFunctionDeclaration(FunctionDeclaration functionDeclaration) {
            functionDeclarations.put(functionDeclaration.name(), functionDeclaration);
            return this;
        }
        CompilationUnitProcessingResult addVariableDeclaration(VariableDeclaration variableDeclaration) {
            variableDeclarations.put(variableDeclaration.name(), variableDeclaration);
            return this;
        }
    }

    static void processDataDefinition(DataDefinition dataDefinition, Scope scope) {
        scope.putVariable(
            dataDefinition.name(),
            new Variable(
                Type.DATA_CREATION_FUNCTION,
                DataCreationFunction.of(dataDefinition, scope)));
    }

    record RuntimeFile(Scope scope) {}

    static class Scope {
        Map<String, Variable> variables;
        Scope parent;
        Map<String, Scope> importedScopes;

        Scope(Map<String, Variable> variables, Scope parent, Map<String, Scope> importedScopes) {
            this.variables = variables;
            this.parent = parent;
            this.importedScopes = importedScopes;
        }

        Scope() {
            this(new HashMap<>(), null, new HashMap<>());
        }
        Scope(Scope parent) {
            this(new HashMap<>(), parent, new HashMap<>());
        }
        public ScopedVariable getVariable(String name) {
            if (variables.containsKey(name)) {
                return new ScopedVariable(variables.get(name), this);
            }

            for (var importedScope : importedScopes.values()) {
                try {
                    return importedScope.getVariable(name);
                } catch (SymbolNotFoundException e) {
                    // ignore, we have to check parents first
                }
            }
            if (parent != null) {
                return parent.getVariable(name);
            } else {
                throw new SymbolNotFoundException("symbol " + name + " not found");
            }
        }
        public void putVariable(String name, Variable variable) {
            variables.put(name, variable);
        }
        public void addImportScope(String name, Scope scope) {
            importedScopes.put(name, scope);
        }
    }

    public record ScopedVariable(Variable variable, Scope scope) {}

    public static class StackFrame {
        StackFrame parent;
        Scope scope;

        public StackFrame(StackFrame parent, Scope scope) {
            this.parent = parent;
            this.scope = scope;
        }

        /**
         * The stack frame that runs the program does not have a scope and thus
         * cannot hold any symbols.
         */
        public StackFrame() {
            this(null, null);
        }

        Scope scope() {
            return scope;
        }

        /**
         * Pushes a frame with a new scope when running a new function. The
         * scope passed is the scope of the function definition.
         */
        public StackFrame pushFrame(Scope scope) {
            return new StackFrame(this, scope);
        }
        public StackFrame pushScope() {
            var newScope = new Scope(scope);
            this.scope = newScope;
            return this;
        }
        public StackFrame popScope() {
            this.scope = scope.parent;
            return this;
        }

        // convenience methods that pass through to the scope; not sure if they are needed
        @Deprecated
        public ScopedVariable getVariable(String name) {
            return scope.getVariable(name);
        }
        @Deprecated
        public void putVariable(String name, Variable variable) {
            scope.putVariable(name, variable);
        }
        void dumpFrames() {
            StackFrame current = this;
            while (current != null) {
                System.out.println(current.scope.variables);
                current = current.parent;
            }
        }
    }

    public interface Value {
        Type type();
    }

    public record NumberValue(long number) implements Value {
        public Type type() {
            return Type.NUMBER;
        }
        @Override
        public String toString() {
            return Long.toString(number);
        }
    }
    public record StringValue(String string) implements Value {
        public Type type() {
            return Type.STRING;
        }
        @Override
        public String toString() {
            return string;
        }
    }
    public record Data(String typeName, Map<String, Value> variables) implements Value {
        public Type type() {
            return Type.DATA;
        }
    }
    public record BooleanValue(boolean value) implements Value {
        public Type type() {
            return Type.BOOLEAN;
        }
    }
    public static Value True = new BooleanValue(true);
    public static Value False = new BooleanValue(false);

    sealed interface Function extends Value {
        List<Parameter> parameters();
        Scope scope();
        default Optional<String> receiver() {
            return Optional.empty();
        }

        static Function of(String moduleName, FunctionDeclaration functionDeclaration, Scope scope) {
            var parameters = mapParameters(functionDeclaration.parameters());
            return switch (functionDeclaration) {
                case UserFunctionDeclaration ufd -> UserFunction.of(ufd, parameters, scope);
                case NativeFunctionDeclaration nfd -> NativeFunction.of(nfd, parameters, scope, moduleName);
                default -> throw new RuntimeException("not yet implemented " + functionDeclaration);
            };
        }
        static List<Parameter> mapParameters(List<VariableDeclaration> variableDeclarations) {
            return variableDeclarations.stream()
                    .map(varDecl -> new Parameter(varDecl.name(), varDecl.type().map(Type::of), varDecl.initializer()))
                    .toList();
        }
    }

    record UserFunction(Optional<String> receiver, String name, List<Parameter> parameters, Expression body, Scope scope) implements Function {
        static UserFunction of(UserFunctionDeclaration functionDeclaration, List<Parameter> parameters, Scope scope) {
            Optional<String> receiver = switch (functionDeclaration.receiver()) {
                case FunctionReceiverVariable(String name) -> Optional.of(name);
                default -> Optional.empty();
            };
            return new UserFunction(
                receiver,
                functionDeclaration.name(),
                parameters,
                functionDeclaration.body(),
                scope);
        }
        public Type type() {
            return Type.FUNCTION;
        }
    }
    record DataCreationFunction(String name, List<Parameter> parameters, Scope scope) implements Function {
        static DataCreationFunction of(DataDefinition dataDefinition, Scope scope) {
            var parameters = dataDefinition.variableDeclarations().stream()
                    .map(varDecl -> new Parameter(varDecl.name(), Type.of(varDecl.type().get())))
                    .toList();
            return new DataCreationFunction(dataDefinition.name(), parameters, scope);
        }
        public Type type() {
            return Type.DATA_CREATION_FUNCTION;
        }
    }

    public record Variable(Type type, Value value) {
        public Variable(Value value) {
            this(value.type(), value);
        }

        <T> T as(Class<T> clazz) {
            return clazz.cast(value);
        }
    }

    enum Type {
        NUMBER,
        STRING,
        BOOLEAN,
        DATA,
        DATA_DEFINITION,
        DATA_CREATION_FUNCTION,
        FUNCTION,
        NATIVE_FUNCTION;
        static Type of(String descriptor) {
            return switch (descriptor) {
                case "Int" -> NUMBER;
                case "String" -> STRING;
                case "Boolean" -> BOOLEAN;
                default -> DATA;
            };
        }
    }
    record Parameter(String name, Optional<Type> value, Optional<Expression> initializer) {
        public Parameter(String name, Type value) {
            this(name, Optional.of(value), Optional.empty());
        }
    }

    record NativeFunction(String name, List<Parameter> parameters, Scope scope, NativeFunctionHandle handle) implements Function {
        static NativeFunction of(NativeFunctionDeclaration nfd, List<Parameter> parameters, Scope scope, String moduleName) {
            var moduleNameInitFirst = moduleName.substring(0, 1).toUpperCase() + moduleName.substring(1);
            try {
                var cls = Class.forName("com.github.musiKk.natives." + moduleNameInitFirst);
                var moduleInstance = cls.getDeclaredConstructor().newInstance();
                var handle = new NativeFunctionHandle() {
                    @Override
                    public Value call(StackFrame stackFrame) {
                        try {
                            var method = cls.getMethod(nfd.name(), StackFrame.class);
                            return (Value) method.invoke(moduleInstance, stackFrame);
                        } catch (NoSuchMethodException | SecurityException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
                            throw new RuntimeException(e);
                        }
                    }
                };
                return new NativeFunction(nfd.name(), parameters, scope, handle);
            } catch (ClassNotFoundException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
                throw new RuntimeException(e);
            }
        }
        public Type type() {
            return Type.NATIVE_FUNCTION;
        }
    }

    interface NativeFunctionHandle {
        Value call(StackFrame stackFrame);
    }

    static class SymbolNotFoundException extends RuntimeException {
        public SymbolNotFoundException(String message) {
            super(message);
        }
    }
}
