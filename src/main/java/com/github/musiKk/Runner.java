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
        var mainStackFrame = new DefaultStackFrame();
        var runtimeFile = runner.getOrInitRuntimeFile("test.tst", mainStackFrame);
        var main = runtimeFile.functions.get("main");
        if (main != null) {
            runner.execute(main, mainStackFrame);
        }
    }

    private RuntimeFile getOrInitRuntimeFile(String pathString, StackFrame frame) {
        var path = Path.of(pathString);
        var rf = runtimeFiles.computeIfAbsent(path, __ -> {
            var runtimeFile = initRuntimeFile(path, frame);
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
        execute(List.of(new ExpressionStatement(functionDeclaration.body())), frame.pushFrame());
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
                }).orElse(frame.getVariable(ve.name()));
            }
            case FunctionEvaluationExpression fe -> {
                var functionName = fe.name();

                List<Parameter> parameters;
                if (nativeFunctions.containsKey(functionName)) {
                    parameters = nativeFunctions.get(functionName).parameters();
                } else {
                    // TODO look up function in avalable symbols
                    parameters = runtimeFiles.get(fe.name()).functions.get(fe.name()).parameters();
                }

                var newStackFrame = frame.pushFrame();
                for (int i = 0; i < fe.arguments().size(); i++) {
                    var argument = fe.arguments().get(i);
                    var value = evaluateExpression(argument, frame);
                    newStackFrame.putVariable(parameters.get(i).name(), value);
                }
                if (nativeFunctions.containsKey(functionName)) {
                    var nativeFunction = nativeFunctions.get(functionName);
                    yield nativeFunction.call(newStackFrame);
                }
                var function = runtimeFiles.get(fe.name()).functions.get(fe.name());
                yield evaluateExpression(function.body(), newStackFrame);
            }
            case BlockExpression be -> {
                frame.pushFrame();
                Value result = null;
                for (var statement : be.statements()) {
                    result = execute(statement, frame);
                }
                yield result;
            }
            default -> throw new RuntimeException("not yet implemented " + expression);
        };
    }

    private Value execute(Statement expression, StackFrame frame) {
        return switch (expression) {
            case ExpressionStatement(Expression e) -> evaluateExpression(e, frame);
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
                        result.addFunction(Function.of(functionDeclaration));
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

        runtimeFile.dataDefinitions.putAll(processingResult.dataDefinitions());
        runtimeFile.functions.putAll(processingResult.functions());

        execute(processingResult.statements(), new RuntimeFileStackFrame(runtimeFile, frame));

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

    static class RuntimeFile {
        private Map<String, Value> variables = new HashMap<>();
        private Map<String, Function> functions = new HashMap<>();
        private Map<String, DataDefinition> dataDefinitions = new HashMap<>();
    }

    interface StackFrame {
        Value getVariable(String name);
        void putVariable(String name, Value value);
        StackFrame pushFrame();
    }

    record RuntimeFileStackFrame(
        RuntimeFile runtimeFile,
        StackFrame parent
    ) implements StackFrame {
        @Override
        public StackFrame pushFrame() {
            return new DefaultStackFrame(this);
        }
        @Override
        public Value getVariable(String name) {
            return runtimeFile.variables.get(name);
        }
        @Override
        public void putVariable(String name, Value value) {
            runtimeFile.variables.put(name, value);
        }
    }

    record DefaultStackFrame(
        Map<String, Value> variables,
        StackFrame parent
    ) implements StackFrame {
        DefaultStackFrame() {
            this(new HashMap<>(), null);
        }
        DefaultStackFrame(StackFrame parent) {
            this(new HashMap<>(), parent);
        }
        @Override
        public StackFrame pushFrame() {
            return new DefaultStackFrame(this);
        }
        @Override
        public Value getVariable(String name) {
            return variables.get(name);
        }
        @Override
        public void putVariable(String name, Value value) {
            variables.put(name, value);
        }
    }

    interface Value {
    }

    record NumberValue(int number) implements Value {}
    record StringValue(String string) implements Value {}
    record DataValue(String typeName, Map<String, Value> fields) implements Value {}
    record Function(String name, List<Parameter> parameters, Expression body) implements Value {
        static Function of(FunctionDeclaration functionDeclaration) {
            var parameters = new ArrayList<Parameter>();
            for (var varDecl : functionDeclaration.parameters()) {
                new Parameter(varDecl.name(), Type.of(varDecl.type()), varDecl.initializer());
            }
            return new Function(
                functionDeclaration.name(),
                parameters,
                functionDeclaration.body());
        }
    }

    enum Type {
        NUMBER,
        STRING,
        DATA;
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
    }

    static class PrintFunction implements NativeFunction {
        @Override
        public Value call(StackFrame stackFrame) {
            var args = stackFrame.getVariable("args");
            String representation = switch (args) {
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
    }
}
