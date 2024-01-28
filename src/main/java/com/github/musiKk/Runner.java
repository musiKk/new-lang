package com.github.musiKk;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
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
        var rf = runtimeFiles.computeIfAbsent(path, __ -> {
            var runtimeFile = initRuntimeFile(path, frame.pushFrame(new DefaultScope()));
            return runtimeFile;
        });
        runtimeFiles.put(path, rf);
        return rf;
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
                var functionName = fe.name();
                final Scope functionScope;

                List<Parameter> parameters;
                if (nativeFunctions.containsKey(functionName)) {
                    var nativeFunction = nativeFunctions.get(functionName);
                    functionScope = nativeFunction.scope();
                    parameters = nativeFunction.parameters();
                } else {
                    // TODO look up function in avalable symbols
                    // parameters = runtimeFiles.get(fe.name()).functions.get(fe.name()).parameters();
                    throw new RuntimeException("user defined functions not supported yet");
                }

                var newStackFrame = frame.pushFrame(functionScope);
                for (int i = 0; i < fe.arguments().size(); i++) {
                    var argument = fe.arguments().get(i);
                    var value = evaluateExpression(argument, frame);
                    var name = parameters.get(i).name();
                    newStackFrame.putVariable(name, new Variable(name, value));
                }
                if (nativeFunctions.containsKey(functionName)) {
                    var nativeFunction = nativeFunctions.get(functionName);
                    yield nativeFunction.call(newStackFrame);
                }
                throw new RuntimeException("user defined functions not supported yet");
                // var function = runtimeFiles.get(fe.name()).symbols.get(fe.name());
                // if (function.type != Type.FUNCTION) {
                //     throw new RuntimeException("cannot call non-function " + function);
                // }
                // yield evaluateExpression(function.as(Function.class).body(), newStackFrame);
            }
            case BlockExpression be -> {
                frame.pushScope();
                Value result = null;
                for (var statement : be.statements()) {
                    result = execute(statement, frame);
                }
                frame.popScope();
                yield result;
            }
            default -> throw new RuntimeException("not yet implemented " + expression);
        };
    }

    private Value execute(Statement expression, StackFrame frame) {
        return switch (expression) {
            case ExpressionStatement(Expression e) -> evaluateExpression(e, frame);
            case VariableDeclaration vds -> {
                var optValue = vds.initializer().map(initializer -> evaluateExpression(initializer, frame));
                frame.putVariable(vds.name(), new Variable(vds.name(), optValue.orElse(null)));
                yield null;
            }
            default -> throw new RuntimeException("not yet implemented " + expression);
        };
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
                        result.addFunction(Function.of(functionDeclaration, frame.scope()));
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

    record DefaultScope(Map<String, Variable> variables, Scope parent) implements Scope {
        DefaultScope() {
            this(new HashMap<>(), null);
        }
        DefaultScope(Scope parent) {
            this(new HashMap<>(), parent);
        }
        @Override
        public ScopedVariable getVariable(String name) {
            if (variables.containsKey(name)) {
                return new ScopedVariable(variables.get(name), this);
            } else if (parent != null) {
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
        public Scope pushScope() {
            return new DefaultScope(this);
        }
        @Override
        public Scope pushScope(Scope scope) {
            return new DefaultScope(scope);
        }
    }

    interface Scope {
        ScopedVariable getVariable(String name);
        void putVariable(String name, Variable variable);
        Scope pushScope();
        Scope pushScope(Scope scope);
    }

    record ScopedVariable(Variable variable, Scope scope) {}

    record StackFrame(
        StackFrame parent,
        Deque<Scope> scopes
    ) {
        /**
         * The stack frame that runs the program does not have a scope and thus
         * cannot hold any symbols.
         */

        public StackFrame() {
            this(null, new ArrayDeque<>());
        }
        // pushes a frame with a new scope
        public StackFrame pushFrame(Scope scope) {
            var newScopes = new ArrayDeque<Scope>();
            newScopes.addFirst(scope);
            return new StackFrame(this, newScopes);
        }
        public Scope scope() {
            return scopes.peek();
        }
        public Scope pushScope() {
            var newScope = scope().pushScope();
            scopes.push(newScope);
            return newScope;
        }
        public void popScope() {
            scopes.pop();
        }

        // convenience methods that pass through to the scope; not sure if they are needed
        @Deprecated
        public ScopedVariable getVariable(String name) {
            return scopes.peek().getVariable(name);
        }
        @Deprecated
        public void putVariable(String name, Variable variable) {
            scopes.peek().putVariable(name, variable);
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
            var parameters = new ArrayList<Parameter>();
            for (var varDecl : functionDeclaration.parameters()) {
                new Parameter(varDecl.name(), Type.of(varDecl.type().get()), varDecl.initializer());
            }
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
