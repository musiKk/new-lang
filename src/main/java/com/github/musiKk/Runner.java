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

    Map<String, RuntimeFile> runtimeFiles = new HashMap<>();

    Map<String, NativeFunction> nativeFunctions = new HashMap<>();

    {
        nativeFunctions.put("print", new PrintFunction());
    }

    public static void main(String[] args) {
        new Runner().runFile("test.tst");
    }

    private void runFile(String name) {
        String fileContent;
        try {
            fileContent = Files.readString(Path.of(name));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        var compilationUnit = new Parser().parseCompilationUnit(
            new Tokenizer().tokenize(fileContent));

        var runtimeFile = creaRuntimeFile(name, compilationUnit);
        var main = runtimeFile.functions.get("main");

        execute(main, new StackFrame());
    }

    private void execute(Function functionDeclaration, StackFrame stack) {
        var stackFrame = new StackFrame();
        var scope = new Scope();
        stackFrame.scopes.push(scope);

        evaluateExpression(functionDeclaration.body(), stackFrame);
    }

    private Value evaluateExpression(Expression expression, StackFrame stackFrame) {
        return switch (expression) {
            case NumberExpression ne -> new NumberValue(ne.number());
            case StringExpression se -> new StringValue(se.string());
            case VariableExpression ve -> {
                var optTarget = ve.target();
                Optional<Value> optReceiver = optTarget.map(t -> evaluateExpression(t, stackFrame));
                yield optReceiver.map(receiver -> {
                    if (receiver instanceof DataValue dv) {
                        return dv.fields().get(ve.name());
                    } else {
                        throw new RuntimeException("cannot look up fields in values " + receiver);
                    }
                }).orElse(stackFrame.scopes.peek().variables.get(ve.name()));
            }
            case FunctionEvaluationExpression fe -> {
                var functionName = fe.name();

                List<Parameter> parameters;
                if (nativeFunctions.containsKey(functionName)) {
                    parameters = nativeFunctions.get(functionName).parameters();
                } else {
                    parameters = runtimeFiles.get(fe.name()).functions.get(fe.name()).parameters();
                }

                var newStackFrame = new StackFrame();
                var scope = newStackFrame.scopes.peek();
                for (int i = 0; i < fe.arguments().size(); i++) {
                    var argument = fe.arguments().get(i);
                    var value = evaluateExpression(argument, stackFrame);
                    scope.variables.put(parameters.get(i).name(), value);
                }
                if (nativeFunctions.containsKey(functionName)) {
                    var nativeFunction = nativeFunctions.get(functionName);
                    yield nativeFunction.call(newStackFrame);
                }
                var function = runtimeFiles.get(fe.name()).functions.get(fe.name());
                yield evaluateExpression(function.body(), newStackFrame);
            }
            case BlockExpression be -> {
                stackFrame.pushScope();
                Value result = null;
                for (var statement : be.expressions()) {
                    result = evaluateExpression(statement, stackFrame);
                }
                stackFrame.popScope();
                yield result;
            }
            default -> throw new RuntimeException("not yet implemented " + expression);
        };
    }

    private RuntimeFile creaRuntimeFile(String name, CompilationUnit compilationUnit) {
        RuntimeFile runtimeFile = new RuntimeFile();
        runtimeFiles.put(name, runtimeFile);

        compilationUnit.dataDefinitions().stream().forEach(dataDefinition -> {
            runtimeFile.dataDefinitions.put(dataDefinition.name(), dataDefinition);
        });
        compilationUnit.functionDeclarations().stream().forEach(functionDeclaration -> {
            runtimeFile.functions.put(functionDeclaration.name(), Function.of(functionDeclaration));
        });
        return runtimeFile;
    }

    static class RuntimeFile {
        private Map<String, Value> variables = new HashMap<>();
        private Map<String, Function> functions = new HashMap<>();
        private Map<String, DataDefinition> dataDefinitions = new HashMap<>();
    }

    static class Scope {
        private Map<String, Value> variables = new HashMap<>();
        public Scope() {}
        public Scope(Scope parent) {
            variables.putAll(parent.variables);
        }
    }

    static class StackFrame {
        private Deque<Scope> scopes = new ArrayDeque<>();
        public StackFrame() {
            scopes.push(new Scope());
        }
        public Scope pushScope() {
            var scope = new Scope(scopes.peek());
            scopes.push(scope);
            return scope;
        }
        public void popScope() {
            scopes.pop();
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
            var args = stackFrame.scopes.peek().variables.get("args");
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
