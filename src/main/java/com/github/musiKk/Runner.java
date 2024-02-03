package com.github.musiKk;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public class Runner {

    Map<Path, RuntimeFile> runtimeFiles = new HashMap<>();

    Map<String, NativeFunction> nativeFunctions = new HashMap<>();

    {
        nativeFunctions.put("print", new PrintFunction());
    }

    public static void main(String[] args) {
        // new Runner().runFile("test.tst");
        var runner = new Runner();
        var mainStackFrame = new StackFrame();
        var runtimeFile = runner.getOrInitRuntimeFile("test.tst", mainStackFrame);
        var main = runtimeFile.scope.getVariable("main");
        if (main != null && main.variable.type() == Type.FUNCTION) {
            runner.execute(main.variable.as(Function.class), mainStackFrame);
        }
    }

    private RuntimeFile getOrInitRuntimeFile(String pathString, StackFrame frame) {
        var path = Path.of(pathString);
        if (!runtimeFiles.containsKey(path)) {
            var runtimeFile = initRuntimeFile(path, frame.pushFrame(new DefaultScope()));
            runtimeFiles.put(path, runtimeFile);
        }
        return runtimeFiles.get(path);
    }

    private void execute(List<Statement> statements, StackFrame frame) {
        for (var statement : statements) {
            execute(statement, frame);
        }
    }

    private void execute(Function functionDeclaration, StackFrame frame) {
        execute(List.of(new ExpressionStatement(functionDeclaration.body())), frame.pushFrame(functionDeclaration.scope));
    }

    private Value evaluateExpression(Expression expression, StackFrame frame) {
        return switch (expression) {
            case NumberExpression ne -> new NumberValue(ne.number());
            case StringExpression se -> new StringValue(se.string());
            case VariableExpression ve -> {
                var optTarget = ve.target();
                Optional<Value> optReceiver = optTarget.map(t -> evaluateExpression(t, frame));
                yield optReceiver.map(receiver -> {
                    if (receiver instanceof DataValue dv) {
                        return dv.fields().get(ve.name());
                    } else {
                        throw new RuntimeException("cannot look up fields in values " + receiver);
                    }
                }).orElse(frame.getVariable(ve.name()).variable.value());
            }
            case FunctionEvaluationExpression fe -> {
                yield evaluateFunction(fe, frame);
            }
            case BlockExpression be -> {
                var blockFrame = frame.pushScope();
                Value result = null;
                for (var statement : be.statements()) {
                    result = execute(statement, blockFrame);
                }
                frame.popScope();
                yield result;
            }
            default -> throw new RuntimeException("not yet implemented " + expression);
        };
    }

    private Value evaluateFunction(FunctionEvaluationExpression fe, StackFrame frame) {
        var functionName = fe.name();
        final Scope functionScope;

        List<Parameter> parameters;
        if (nativeFunctions.containsKey(functionName)) {
            var nativeFunction = nativeFunctions.get(functionName);
            functionScope = nativeFunction.scope();
            parameters = nativeFunction.parameters();
        } else {
            var function = frame.getVariable(functionName).variable.as(Function.class);
            functionScope = function.scope();
            parameters = function.parameters();
        }

        var newStackFrame = frame.pushFrame(functionScope);
        for (int i = 0; i < fe.arguments().size(); i++) {
            var argument = fe.arguments().get(i);
            var value = evaluateExpression(argument, frame);
            var name = parameters.get(i).name();
            newStackFrame.putVariable(name, new Variable(name, value));
        }

        try {
            if (nativeFunctions.containsKey(functionName)) {
                var nativeFunction = nativeFunctions.get(functionName);
                return nativeFunction.call(newStackFrame);
            }
            var function = frame.getVariable(functionName).variable.as(Function.class);
            return evaluateExpression(function.body(), newStackFrame);
        } finally {
            frame.scope().clearImports();
        }
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
                frame.putVariable(vds.name(), new Variable(vds.name(), optValue.orElse(null)));
                yield null;
            }
            case Import imp -> {
                processImport(imp, frame);
                yield null;
            }
            default -> throw new RuntimeException("not yet implemented " + expression);
        };
    }

    private void processImport(Import imp, StackFrame frame) {
        var runtimeFile = getOrInitRuntimeFile(imp.name() + ".tst", frame);
        frame.scope().addImportScope(imp.name(), runtimeFile.scope);
    }

    private RuntimeFile initRuntimeFile(Path path, StackFrame frame) {
        String fileContent;
        try {
            fileContent = Files.readString(path);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        var compilationUnit = new Parser().parseCompilationUnit(new Tokenizer().tokenize(fileContent));

        RuntimeFile runtimeFile = new RuntimeFile();

        var processingResult = compilationUnit.statements().stream()
                .reduce(new CompilationUnitProcessingResult(), (result, statement) -> {
                    if (statement instanceof DataDefinition dataDefinition) {
                        result.addDataDefinition(dataDefinition);
                    } else if (statement instanceof FunctionDeclaration functionDeclaration) {
                        result.addFunction(Function.of(functionDeclaration, runtimeFile.scope()));
                    } else {
                        result.addStatement(statement);
                    }
                    return result;
                }, (r1, r2) -> {
                    r1.statements.addAll(r2.statements);
                    r1.dataDefinitions.putAll(r2.dataDefinitions);
                    r1.functions.putAll(r2.functions);
                    return r1;
                });

        processingResult.dataDefinitions().forEach((name, dataDefinition) -> {
            runtimeFile.scope.putVariable(name, new Variable(name, Type.DATA_DEFINITION, new DataValue(name, new HashMap<>())));
        });
        processingResult.functions().forEach((name, function) -> {
            runtimeFile.scope.putVariable(name, new Variable(name, Type.FUNCTION, function));
        });

        execute(processingResult.statements(), frame.pushFrame(runtimeFile.scope));

        return runtimeFile;
    }

    record CompilationUnitProcessingResult(
        List<Statement> statements,
        Map<String, DataDefinition> dataDefinitions,
        Map<String, Function> functions
    ) {
        public CompilationUnitProcessingResult() {
            this(new ArrayList<>(), new HashMap<>(), new HashMap<>());
        }
        CompilationUnitProcessingResult addStatement(Statement statement) {
            statements.add(statement);
            return this;
        }
        CompilationUnitProcessingResult addDataDefinition(DataDefinition dataDefinition) {
            dataDefinitions.put(dataDefinition.name(), dataDefinition);
            return this;
        }
        CompilationUnitProcessingResult addFunction(Function function) {
            functions.put(function.name(), function);
            return this;
        }
    }

    record RuntimeFile(Scope scope) {
        public RuntimeFile() {
            this(new DefaultScope());
        }
    }

    record DefaultScope(Map<String, Variable> variables, Scope parent, Map<String, Scope> importedScopes) implements Scope {
        DefaultScope() {
            this(new HashMap<>(), null, new HashMap<>());
        }
        DefaultScope(Scope parent) {
            this(new HashMap<>(), parent, new HashMap<>());
        }
        @Override
        public ScopedVariable getVariable(String name) {
            if (variables.containsKey(name)) {
                return new ScopedVariable(variables.get(name), this);
            }

            for (var importedScope : importedScopes.values()) {
                var variable = importedScope.getVariable(name);
                if (variable != null) {
                    return variable;
                }
            }
            if (parent != null) {
                return parent.getVariable(name);
            } else {
                throw new RuntimeException("symbol " + name + " not found");
            }
        }
        @Override
        public void putVariable(String name, Variable variable) {
            variables.put(name, variable);
        }
        @Override
        public void addImportScope(String name, Scope scope) {
            importedScopes.put(name, scope);
        }
        @Override
        public void clearImports() {
            importedScopes.clear();
        }
    }

    interface Scope {
        ScopedVariable getVariable(String name);
        void putVariable(String name, Variable variable);
        void addImportScope(String name, Scope scope);
        void clearImports();
    }

    record ScopedVariable(Variable variable, Scope scope) {}

    record StackFrame(
        StackFrame parent,
        Scope scope
    ) {
        /**
         * The stack frame that runs the program does not have a scope and thus
         * cannot hold any symbols.
         */
        public StackFrame() {
            this(null, null);
        }
        /**
         * Pushes a frame with a new scope when running a new function. The
         * scope passed is the scope of the function definition.
         */
        public StackFrame pushFrame(Scope scope) {
            return new StackFrame(this, scope);
        }
        public StackFrame pushScope() {
            var newScope = new DefaultScope(scope);
            return new StackFrame(this, newScope);
        }
        public StackFrame popScope() {
            return parent;
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
    }

    interface Value {
        Type type();
    }

    record NumberValue(int number) implements Value {
        public Type type() {
            return Type.NUMBER;
        }
    }
    record StringValue(String string) implements Value {
        public Type type() {
            return Type.STRING;
        }
    }
    record DataValue(String typeName, Map<String, Value> fields) implements Value {
        public Type type() {
            return Type.DATA;
        }
    }
    record Function(String name, List<Parameter> parameters, Expression body, Scope scope) implements Value {
        static Function of(FunctionDeclaration functionDeclaration, Scope scope) {
            var parameters = functionDeclaration.parameters().stream()
                    .map(varDecl -> new Parameter(varDecl.name(), Type.of(varDecl.type().get()), varDecl.initializer()))
                    .toList();
            return new Function(
                functionDeclaration.name(),
                parameters,
                functionDeclaration.body(),
                scope);
        }
        public Type type() {
            return Type.FUNCTION;
        }
    }

    record Variable(String name, Type type, Value value) {
        public Variable(String name, Value value) {
            this(name, value.type(), value);
        }

        <T> T as(Class<T> clazz) {
            return clazz.cast(value);
        }
    }

    enum Type {
        NUMBER,
        STRING,
        DATA,
        DATA_DEFINITION,
        FUNCTION;
        static Type of(String descriptor) {
            return switch (descriptor) {
                case "Int" -> NUMBER;
                case "String" -> STRING;
                default -> DATA;
            };
        }
    }
    record Parameter(String name, Type value, Optional<Expression> initializer) {
        public Parameter(String name, Type value) {
            this(name, value, Optional.empty());
        }
    }

    interface NativeFunction {
        Value call(StackFrame stackFrame);
        List<Parameter> parameters();
        Scope scope();
    }

    static class PrintFunction implements NativeFunction {
        @Override
        public Value call(StackFrame stackFrame) {
            var args = stackFrame.getVariable("args");
            String representation = switch (args.variable.value()) {
                case null -> "null";
                case NumberValue nv -> String.valueOf(nv.number());
                case StringValue sv -> sv.string();
                case DataValue dv -> dv.typeName() + "[...fields...]";
                default -> args.toString();
            };
            System.out.println(representation);
            return null;
        }
        public List<Parameter> parameters() {
            return List.of(new Parameter("args", Type.DATA));
        }
        public Scope scope() {
            return new DefaultScope();
        }
    }
}
