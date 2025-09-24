package com.github.musiKk;

import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

public class Compiler implements ConfigReader.ConfigTarget {

    @Setter
    private List<String> lookupPath = new ArrayList<>();
    @Setter
    private String target;

    private TypeRegistry typeRegistry = new TypeRegistry();
    private FunctionRegistry functionRegistry = new FunctionRegistry();

    public static void main(String[] args) {
        var compiler = new Compiler();
        ConfigReader.readConfig().applyConfig(compiler);
        var output = compiler.compileProgram("test.tst");
        System.err.println(output);
    }

    /**
     * Compiles an executable starting at {@code pathString}.
     * @param pathString
     */
    public Output compileProgram(String pathString) {
        registerBasicTypes();

        Path.of(target).toFile().mkdirs();
        var resolvedPath = resolvePath(pathString);

        // 1. phase: collect all files needed by following imports
        // perform following phases per file
        // TODO we only parse the main file for now
        var cu = parseCompilationUnit(resolvedPath);

        // 2. phase: collect all types and put into type registry
        collectTypes(cu);
        // 3. phase: collect all functions
        collectFunctions(cu);

        // var output = new Output(Path.of(target, pathString.replace(".tst", ".c")));
        var output = new Output();
        compileCompilationUnit(cu, output);

        try {
            output.emit(Path.of(target, resolvedPath.getFileName().toString()));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

        return output;
    }

    private void registerBasicTypes() {
        typeRegistry.register(new Type.IntType());
    }

    private void collectFunctions(CompilationUnit cu) {
        var fc = new FunctionCollector(functionRegistry, cu);
        fc.run();
    }

    private void collectTypes(CompilationUnit cu) {
        var tc = new TypeCollector(typeRegistry, cu);
        tc.run();
    }

    private CompilationUnit parseCompilationUnit(Path resolvedPath) {
        var parser = new Parser();
        try {
            String fileContent = Files.readString(resolvedPath);
            return parser.parseCompilationUnit(new Tokenizer().tokenize(fileContent));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private Path resolvePath(String pathString) {
        Path resolvedPath = Path.of(pathString);
        if (!resolvedPath.isAbsolute()) {
            resolvedPath = lookupPath.stream()
                    .map(p -> Path.of(p, pathString))
                    .filter(Files::exists)
                    .findFirst()
                    .orElseThrow(() -> new RuntimeException("file not found: " + pathString));
        }
        return resolvedPath;
    }

    private void compileCompilationUnit(CompilationUnit cu, Output output) {
        // 1. compile types
        typeRegistry.map.values().stream()
                .forEach(t -> compileType(t, output));

        // 2. compile functions
        functionRegistry.map.values().stream()
                .forEach(f -> compileFunctionDefinition(f, output));
    }

    private void compileType(Registry.RegistryValue<Type> rv, Output output) {
        switch (rv.parsedStatement()) {
            case Type.DataType dt -> {
                var struct = output.struct(rv.cName());
                dt.dataDefinition().variableDeclarations().stream()
                        .forEach(vd -> {
                            String propertyType = typeRegistry.lookupCName(vd.type().get());
                            String propertyName = vd.name();

                            struct.field(propertyName, propertyType);
                        });
                struct.finish();
            }
            default -> {}
        }
    }

    private void compileFunctionDefinition(Registry.RegistryValue<FunctionDeclaration> rv, Output output) {
        var signature = rv.parsedStatement().signature();
        var function = output.function(rv.cName(), signature.returnType());
        if (signature.receiver() instanceof FunctionReceiverVariable frv) {
            function.parameter("self", typeRegistry.lookupCName(frv.name()));
        }
        signature.parameters().stream()
                .forEach(p -> function.parameter(p.name(), typeRegistry.lookupCName(p.type().get())));
        if (rv.parsedStatement() instanceof UserFunctionDeclaration ufd) {
            var scope = new Scope();
            signature.parameters().stream()
                    .forEach(p -> scope.variables.put(p.name(), typeRegistry.lookupCName(p.type().get())));
            function.body(compileExpression(ufd.body(), scope));
        }
        function.finish();
    }

    class Scope {
        // name -> type cname
        Map<String, String> variables = new HashMap<>();

        int count = 0;

        public String newTemp(String tmpType) {
            while (true) {
                String tmpCand = "tmp_" + (count++);
                if (!variables.containsKey(tmpCand)) {
                    variables.put(tmpCand, tmpType);
                    return tmpCand;
                }
            }
        }
    }

    private Output.Expression compileExpression(Expression parsedExpression, Scope locals) {
        return switch (parsedExpression) {
            case NumberExpression(long n) -> new Output.NumberExpression(n);
            case VariableExpression(var target, String name) when target.isEmpty() -> {
                        if (!locals.variables.containsKey(name)) {
                            throw new RuntimeException("name " + name + " not found");
                        }
                        yield new Output.NameExpression(name);
                    }
            case BinaryExpression be -> {
                var left = compileExpression(be.left(), locals);
                var right = compileExpression(be.right(), locals);
                yield new Output.BinaryExpression(left, be.operator().constantPattern, right);
            }
            case BlockExpression be -> {
                var stmts = be.statements();
                var returnStatement = ((ExpressionStatement) stmts.getLast()).expression();
                List<Output.Expression> compiledStatements = new ArrayList<>();
                for (int i = 0; i <= stmts.size() - 1; i++) {
                    compiledStatements.add(compileExpression(parsedExpression, locals));
                }

                var compiledReturn = compileExpression(returnStatement, locals);
                String tmpType = switch (compiledReturn) {
                    case Output.NumberExpression(long n) -> "Int";
                    case Output.NameExpression(String n) -> locals.variables.get(n);
                    default -> "void*";
                };
                var tmpName = locals.newTemp(tmpType);

                var resultVarDecl = new Output.VariableDeclaration(tmpName, tmpType);
                var resultVarAssign = new Output.Assignment(tmpName, compiledReturn);

                List<Output.Expression> finalStatements = new ArrayList<>();
                finalStatements.add(resultVarDecl);
                finalStatements.addAll(compiledStatements);
                finalStatements.add(resultVarAssign);

                yield new Output.Block(finalStatements);
            }
            default -> throw new RuntimeException(parsedExpression.toString());
        };
    }

    // private void compileExpression(Expression e, Output output) {
    //     switch (e) {
    //         case BlockExpression be -> {
    //             for (var statement : be.statements()) {
    //                 compileStatement(statement, output);
    //             }
    //         }
    //         case NumberExpression ne -> output.write(Long.toString(ne.number()));
    //         case StringExpression se -> output.write("\"", se.string(), "\"");
    //         case VariableExpression ve when ve.target().isEmpty() ->
    //             output.write(ve.name());
    //         case BinaryExpression be -> {
    //             output.write("(");
    //             compileExpression(be.left(), output);
    //             switch (be.operator()) {
    //                 case PLUS -> output.write(" + ");
    //                 case MINUS -> output.write(" - ");
    //                 case STAR -> output.write(" * ");
    //                 case SLASH -> output.write(" / ");
    //                 default -> throw new RuntimeException("unsupported operator " + be.operator());
    //             }
    //             compileExpression(be.right(), output);
    //             output.write(")");
    //         }
    //         case AssignmentExpression ae -> {
    //             compileExpression(ae.target(), output);
    //             compileExpression(ae.value(), output);
    //             output.writeln(";");
    //         }
    //         default -> throw new RuntimeException("unsupported expression " + e);
    //     }
    // }

    // private void compileStatement(Statement s, Output output) {
    //     switch (s) {
    //         case ExpressionStatement es -> compileExpression(es.expression(), output);
    //         case VariableDeclaration vd -> {
    //             output.write(vd.type().get(), " ", vd.name());
    //             if (vd.initializer().isPresent()) {
    //                 output.write(" = ");
    //                 compileExpression(vd.initializer().get(), output);
    //             }
    //             output.writeln(";");
    //         }
    //         default -> throw new RuntimeException("unsupported statement " + s);
    //     }
    // }

    @ToString
    static class Output {
        List<Struct> structs = new ArrayList<>();
        List<Function> functions = new ArrayList<>();

        StructBuilder struct(String cName) {
            return new StructBuilder(cName);
        }

        FunctionBuilder function(String cName, String returnType) {
            return new FunctionBuilder(cName, returnType);
        }

        void emit(Path path) throws Exception {
            try (var oe = new OutputEmitter(this, path)) {
                oe.emit();
            }
        }

        record Function(String cName, String returnType, List<Parameter> parameters, Expression body) {};
        record Parameter(String name, String type) {};

        @RequiredArgsConstructor
        class FunctionBuilder {
            final String cName;
            final String returnType;
            Expression body;
            final List<Parameter> parameters = new ArrayList<>();
            void parameter(String name, String type) {
                parameters.add(new Parameter(name, type));
            }
            void body(Expression expression) {
                body = expression;
            }
            void finish() {
                functions.add(new Function(cName, returnType, parameters, body));
            }
        }

        record Struct(String cName, List<Field> fields) {};
        record Field(String name, String type) {};

        @AllArgsConstructor
        class StructBuilder {
            String cName;
            final List<Field> fields = new ArrayList<>();
            void field(String name, String type) {
                fields.add(new Field(name, type));
            }
            void finish() {
                structs.add(new Struct(cName, fields));
            }
        }

        static interface Expression {}
        record NumberExpression(long l) implements Expression {}
        record NameExpression(String name) implements Expression {}
        record BinaryExpression(Expression left, String op, Expression right) implements Expression {}
        record Block(List<Expression> expressions) implements Expression {}
        record VariableDeclaration(String name, String type) implements Expression {}
        record Assignment(String name, Expression right) implements Expression {}
    }

    static class OutputEmitter implements AutoCloseable {
        Output output;
        Writer writer;
        int indent = 0;
        OutputEmitter(Output output, Path targetPath) throws IOException {
            this.output = output;
            writer = new FileWriter(targetPath.toFile());
        }
        void emit() {
            output.structs.stream()
                    .forEach(this::emitStruct);
            output.functions.stream()
                    .forEach(this::emitPrototype);
            output.functions.stream()
                    .forEach(this::emitFunction);
        }

        void emitFunction(Output.Function function) {

        }

        void emitPrototype(Output.Function function) {
            emitLine(function.returnType + " " + function.cName + "(");
            indent();
            for (int i = 0; i < function.parameters.size(); i++) {
                var p = function.parameters.get(i);
                emitLine(p.type + " " + p.name + (i < function.parameters.size() - 1 ? "," : ""));
            }
            outdent();
            emitLine(");");
            emitLine("");
        }

        void emitStruct(Output.Struct struct) {
            var structName = struct.cName + "__struct";
            emitLine("struct " + structName + " {");
            indent();
            struct.fields.stream()
                    .forEach(f -> emitLine(f.type + " " + f.name + ";"));
            outdent();
            emitLine("};");
            emitLine("typedef struct " + structName + "* " + struct.cName + ";");
            emitLine("");
        }

        void indent() {
            indent++;
        }

        void outdent() {
            indent--;
        }

        void emitLine(String line) {
            try {
                for (int i = 0; i < indent; i++) {
                    writer.append("    ");
                }
                writer.append(line);
                writer.append('\n');
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        @Override
        public void close() throws Exception {
            writer.close();
        }
    }

}

abstract class Registry<T> {
    Map<String, RegistryValue<T>> map = new HashMap<>();

    void register(T parsedStatement) {
        var sourceName = getSourceName(parsedStatement);
        if (map.containsKey(sourceName)) {
            throw new RuntimeException(sourceName + " already defined");
        }
        var cName = generateCName(parsedStatement);
        map.put(sourceName, new RegistryValue<>(cName, parsedStatement));
    }

    String lookupCName(String name) {
        return map.get(name).cName;
    }

    RegistryValue<T> lookupRegistryValue(String name) {
        return map.get(name);
    }

    protected abstract String getSourceName(T parsedStatement);
    protected abstract String generateCName(T parsedStatement);

    record RegistryValue<T>(String cName, T parsedStatement) {};
}

class FunctionRegistry extends Registry<FunctionDeclaration> {

    @Override
    protected String getSourceName(FunctionDeclaration fd) {
        return fd.signature().name();
    }

    @Override
    protected String generateCName(FunctionDeclaration fd) {
        var sig = fd.signature();
        String mappedName = switch (sig.receiver()) {
            case StandaloneFunctionReceiver sfr -> "";
            case FunctionReceiverVariable fv -> fv.name() + "__";
        };
        return mappedName + sig.name();
    }

}

@AllArgsConstructor
class FunctionCollector {
    FunctionRegistry functionRegistry;
    CompilationUnit cu;
    void run() {
        // TODO only top-level functions recognized for now
        for (var stmt : cu.statements()) {
            switch (stmt) {
                case FunctionDeclaration fd ->
                        functionRegistry.register(fd);
                default -> {}
            }
        }
    }
}

@AllArgsConstructor
class TypeCollector {
    TypeRegistry typeRegistry;
    CompilationUnit cu;
    void run() {
        // TODO only top-level types recognized for now
        for (var stmt : cu.statements()) {
            switch (stmt) {
                case DataDefinition dd -> registerType(dd);
                default -> {}
            }
        }
    }
    void registerType(DataDefinition dd) {
        typeRegistry.register(new Type.DataType(dd));
    }
}

class TypeRegistry extends Registry<Type> {

    @Override
    protected String getSourceName(Type type) {
        return type.getName();
    }

    @Override
    protected String generateCName(Type type) {
        return type.getCRepresentation();
    }
}

interface Type {
    String getName();
    String getCRepresentation();
    record IntType() implements Type {
        @Override
        public String getName() {
            return "Int";
        }
        @Override
        public String getCRepresentation() {
            return "int";
        }
    }
    record DataType(DataDefinition dataDefinition) implements Type {
        @Override
        public String getName() {
            return dataDefinition.name();
        }
        @Override
        public String getCRepresentation() {
            return dataDefinition.name();
        }
    }
}
