package com.github.musiKk;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.github.musiKk.Tokenizer.TokenType;

public class Runner {

    Map<Path, RuntimeFile> runtimeFiles = new HashMap<>();

    Map<String, NativeFunction> nativeFunctions = new HashMap<>();

    {
        nativeFunctions.put("_print", new PrintFunction());
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
                var blockFrame = frame.pushScope();
                Value result = null;
                for (var statement : be.statements()) {
                    result = execute(statement, blockFrame);
                }
                frame.popScope();
                yield result;
            }
            case BinaryExpression be -> {
                var left = (NumberValue) evaluateExpression(be.left(), frame);
                var right = (NumberValue) evaluateExpression(be.right(), frame);
                yield switch (be.operator()) {
                    case TokenType.PLUS -> new NumberValue(left.number() + right.number());
                    case TokenType.MINUS -> new NumberValue(left.number() - right.number());
                    case TokenType.STAR -> new NumberValue(left.number() * right.number());
                    case TokenType.SLASH -> new NumberValue(left.number() / right.number());
                    default -> throw new RuntimeException("not yet implemented " + be.operator());
                };
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
                        sv.scope.putVariable(lhs.name(), new Variable(lhs.name(), rhs));
                    } catch (SymbolNotFoundException e) {
                        frame.putVariable(lhs.name(), new Variable(lhs.name(), rhs));
                    }
                }
                yield rhs;
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
            switch (frame.getVariable(functionName).variable.value) {
                case Function function -> {
                    functionScope = function.scope();
                    parameters = function.parameters();
                }
                case DataCreationFunction dataCreationFunction -> {
                    functionScope = dataCreationFunction.scope();
                    parameters = dataCreationFunction.parameters();
                }
                default -> throw new RuntimeException("not a function " + functionName);
            }
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
            return nativeFunction.call(newStackFrame);
        }

        return switch (frame.getVariable(functionName).variable.value) {
            case DataCreationFunction dataCreationFunction -> {
                yield createData(dataCreationFunction, newStackFrame);
            }
            case Function function -> {
                yield evaluateExpression(function.body(), newStackFrame);
            }
            default -> throw new RuntimeException("not a function " + functionName);
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
                frame.putVariable(vds.name(), new Variable(vds.name(), optValue.orElse(null)));
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

        RuntimeFile runtimeFile = new RuntimeFile(frame.scope);

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
            processDataDefinition(dataDefinition, frame.scope);
        });
        processingResult.functions().forEach((name, function) -> {
            frame.putVariable(name, new Variable(name, Type.FUNCTION, function));
        });

        execute(processingResult.statements(), frame.pushFrame(runtimeFile.scope));

        return runtimeFile;
    }

    static void processDataDefinition(DataDefinition dataDefinition, Scope scope) {
        scope.putVariable(
            dataDefinition.name(),
            new Variable(
                dataDefinition.name(),
                Type.DATA_CREATION_FUNCTION,
                DataCreationFunction.of(dataDefinition, scope)));
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

    record RuntimeFile(Scope scope) {}

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
        @Override
        public void putVariable(String name, Variable variable) {
            variables.put(name, variable);
        }
        @Override
        public void addImportScope(String name, Scope scope) {
            importedScopes.put(name, scope);
        }
    }

    interface Scope {
        ScopedVariable getVariable(String name);
        void putVariable(String name, Variable variable);
        void addImportScope(String name, Scope scope);
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
    record Data(String typeName, Map<String, Value> variables) implements Value {
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
    record DataCreationFunction(String name, List<Parameter> parameters, Scope scope) implements Value {
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
        DATA_CREATION_FUNCTION,
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
                case Data dv -> dv.typeName() + "[...fields...]";
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

    static class SymbolNotFoundException extends RuntimeException {
        public SymbolNotFoundException(String message) {
            super(message);
        }
    }
}
