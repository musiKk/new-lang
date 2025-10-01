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
import java.util.Optional;

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

        var output = new Output();
        compileCompilationUnit(cu, output);

        try {
            output.emit(Path.of(target, resolvedPath.getFileName().toString().replace(".tst", ".c")));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

        return output;
    }

    private void registerBasicTypes() {
        typeRegistry.register(new Type.IntType());
        typeRegistry.register(new Type.StringType());

        functionRegistry.register("print",
                new FunctionRegistry.Function(
                        Optional.empty(),
                        "print",
                        "void",
                        List.of(new FunctionRegistry.Function.Parameter("s", typeRegistry.lookupCName("String")))));
        functionRegistry.register("toString",
                new FunctionRegistry.Function(
                        Optional.of("Int"),
                        "toString",
                        "String",
                        List.of()));
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
        functionRegistry.declaredFunctions.stream()
                .forEach(fd -> compileFunctionDefinition(fd, output));

        compileMain(cu, output);
    }

    private void compileMain(CompilationUnit cu, Output output) {
        var fb = output.function("main", "int");
        cu.statements().stream()
                .filter(s -> !(s instanceof DataDefinition))
                .filter(s -> !(s instanceof FunctionDeclaration))
                .forEach(s -> compileStatement(s, new Compiler.Scope(), fb));
        fb.addElement(new Output.Return(new Output.NumberExpression(0)));
        fb.finish();
    }

    private void compileType(Registry.RegistryValue<Type> rv, Output output) {
        if (!(rv.parsedStatement() instanceof Type.DataType)) return;

        var dt = (Type.DataType) rv.parsedStatement();

        var struct = output.struct(rv.cName());
        dt.dataDefinition().variableDeclarations().stream()
                .forEach(vd -> {
                    String propertyType = typeRegistry.lookupCName(vd.type().get());
                    String propertyName = vd.name();

                    struct.field(propertyName, propertyType);
                });
        struct.finish();

        functionRegistry.register(dt.getName(), new FunctionRegistry.Function(
            Optional.empty(),
            rv.cName() + "__new",
            rv.parsedStatement().getName(),
            dt.dataDefinition().variableDeclarations().stream()
                    .map(vd -> new FunctionRegistry.Function.Parameter(vd.name(), vd.type().get()))
                    .toList()
        ));
        var fb = output.function(rv.cName() + "__new", rv.cName());
        dt.dataDefinition().variableDeclarations().stream()
                    .forEach(vd -> fb.parameter(vd.name(), typeRegistry.lookupCName(vd.type().get())));
        fb.addElement(new Output.VariableDeclaration("ret", rv.cName()));
        fb.addElement(new Output.Assignment("ret", new Output.Allocation(rv.cName())));
        dt.dataDefinition().variableDeclarations().forEach(vd -> {
            fb.addElement(new Output.FieldAssignment(new Output.NameExpression("ret"), vd.name(), new Output.NameExpression(vd.name())));
        });
        fb.addElement(new Output.Return(new Output.NameExpression("ret")));
        fb.finish();
    }

    private void compileFunctionDefinition(FunctionDeclaration fd, Output output) {
        if (!(fd instanceof UserFunctionDeclaration)) {
            return;
        }
        UserFunctionDeclaration ufd = (UserFunctionDeclaration) fd;

        var signature = ufd.signature();
        var function = output.function(functionRegistry.lookupCName(signature.name()), typeRegistry.lookupCName(signature.returnType()));
        var scope = new Scope();
        signature.receiver().map(typeRegistry::lookupCName).ifPresent(receiverType -> {
            function.parameter("self", receiverType);
            scope.variables.put("self", receiverType);
        });
        signature.parameters().stream()
                .forEach(p -> function.parameter(p.name(), typeRegistry.lookupCName(p.type().get())));
        signature.parameters().stream()
                .forEach(p -> scope.variables.put(p.name(), typeRegistry.lookupCName(p.type().get())));

        var result = compileExpression(ufd.body(), scope, function);
        function.addElement(new Output.Return(result));
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

    private void compileStatement(Statement statement, Scope locals, Output.FunctionBuilder fb) {
        switch (statement) {
            case ExpressionStatement es -> fb.addElement(compileExpression(es.expression(), locals, fb));
            case VariableDeclaration vd -> {
                Optional<Output.Expression> initStatement = vd.initializer().map(e -> compileExpression(e, locals, fb));
                locals.variables.put(vd.name(), typeRegistry.lookupCName(vd.type().get()));
                fb.addElement(new Output.VariableDeclaration(vd.name(), typeRegistry.lookupCName(vd.type().get())));
                initStatement.ifPresent(is -> fb.addElement(new Output.Assignment(vd.name(), is)));
            }
            default -> throw new RuntimeException(statement.toString());
        }
    }

    private Output.Expression compileExpression(Expression parsedExpression, Scope locals, Output.FunctionBuilder fb) {
        return switch (parsedExpression) {
            case NumberExpression(long n) -> new Output.NumberExpression(n);
            case StringExpression(String s) -> {
                var stringCType = typeRegistry.lookupCName("String");
                var stringVarName = locals.newTemp(stringCType);
                fb.addElement(new Output.VariableDeclaration(stringVarName, stringCType));
                fb.addElement(new Output.Assignment(stringVarName, new Output.FunctionEvaluation("String__native_new_copy", List.of(new Output.StringLiteral(s)))));

                yield new Output.NameExpression(stringVarName);
            }
            case VariableExpression(var target, String name) when target.isEmpty() -> {
                if (!locals.variables.containsKey(name)) {
                    throw new RuntimeException("name " + name + " not found");
                }
                yield new Output.NameExpression(name);
            }
            case VariableExpression(var target, String name) when target.isPresent() -> {
                var compiledTarget = compileExpression(target.get(), locals, fb);
                yield new Output.FieldAccess(compiledTarget, name);
            }
            case BinaryExpression be -> {
                var left = compileExpression(be.left(), locals, fb);
                var right = compileExpression(be.right(), locals, fb);
                yield new Output.BinaryExpression(left, be.operator().constantPattern, right);
            }
            case BlockExpression be -> {
                var stmts = be.statements();
                var returnStatement = stmts.getLast();
                if (!(returnStatement instanceof ExpressionStatement)) {
                    throw new RuntimeException("last statement in block must be an expression: " + be);
                }
                for (int i = 0; i <= stmts.size() - 1; i++) {
                    var stmt = stmts.get(i);
                    compileStatement(stmt, locals, fb);
                }

                var returnExpression = ((ExpressionStatement) returnStatement).expression();
                var compiledReturn = compileExpression(returnExpression, locals, fb);
                String tmpType = switch (compiledReturn) {
                    case Output.NumberExpression(long n) -> "Int";
                    case Output.NameExpression(String n) -> locals.variables.get(n);
                    default -> "void*";
                };
                var tmpName = locals.newTemp(tmpType);

                var resultVarDecl = new Output.VariableDeclaration(tmpName, tmpType);
                var resultVarAssign = new Output.Assignment(tmpName, compiledReturn);

                fb.addElement(resultVarDecl);
                fb.addElement(resultVarAssign);

                yield new Output.NameExpression(tmpName);
            }
            case FunctionEvaluationExpression fee -> {
                List<Output.Expression> args = new ArrayList<>();
                fee.target()
                        .map(e -> compileExpression(e, locals, fb))
                        .ifPresent(args::add);
                fee.arguments().stream()
                        .map(e -> compileExpression(e, locals, fb))
                        .forEach(args::add);
                var fName = functionRegistry.lookupCName(fee.name());

                yield new Output.FunctionEvaluation(fName, args);
            }
            default -> throw new RuntimeException(parsedExpression.toString());
        };
    }

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

        record Function(String cName, String returnType, List<Parameter> parameters, List<Output.Element> body) {};
        record Parameter(String name, String type) {};

        @RequiredArgsConstructor
        class FunctionBuilder {
            final String cName;
            final String returnType;
            final List<Parameter> parameters = new ArrayList<>();
            final List<Element> body = new ArrayList<>();
            void parameter(String name, String type) {
                parameters.add(new Parameter(name, type));
            }
            void addElement(Output.Element e) {
                body.add(e);
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

        interface Element {}
        interface Expression extends Element {}
        interface Statement extends Element {}
        record NumberExpression(long l) implements Expression {}
        record StringLiteral(String s) implements Expression {}
        record NameExpression(String name) implements Expression {}
        record BinaryExpression(Expression left, String op, Expression right) implements Expression {}
        record Block(List<Expression> expressions) implements Expression {}
        record FieldAccess(Expression target, String field) implements Expression {}
        record Allocation(String type) implements Expression {}
        record FunctionEvaluation(String name, List<Expression> arguments) implements Expression {}

        record VariableDeclaration(String name, String type) implements Statement {}
        record Assignment(String name, Expression right) implements Statement {}
        record FieldAssignment(Expression target, String field, Expression right) implements Statement {}
        record Return(Expression retval) implements Statement {}
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
            emitLineNl("#include \"rt.h\"", true);
            emitLineNl("#include \"rt_io.h\"", true);
            emitLineNl("#include \"rt_primitives.h\"", true);

            emitLineNl("", true);

            output.structs.stream()
                    .forEach(this::emitStruct);
            output.functions.stream()
                    .forEach(this::emitPrototype);
            output.functions.stream()
                    .forEach(this::emitFunction);
        }

        void emitFunction(Output.Function function) {
            emitLineNl(function.returnType + " " + function.cName + "(", true);
            indent();
            for (int i = 0; i < function.parameters.size(); i++) {
                var p = function.parameters.get(i);
                emitLineNl(p.type + " " + p.name + (i < function.parameters.size() - 1 ? "," : ""), true);
            }
            outdent();
            emitLineNl(") {", true);
            indent();
            for (Output.Element e : function.body) {
                emitExpression(e, true);
                emitLineNl(";", false);
            }
            outdent();
            emitLineNl("}", true);
        }

        void emitExpression(Output.Element e, boolean doIndent) {
            switch (e) {
                case Output.NumberExpression n -> emitLine(Long.toString(n.l), doIndent);
                case Output.StringLiteral s -> emitLine("\""  + s.s + "\"", doIndent);
                case Output.NameExpression n -> emitLine(n.name, doIndent);
                case Output.VariableDeclaration vd -> {
                    emitLine(vd.type + " " + vd.name, true);
                }
                case Output.Assignment a -> {
                    emitLine(a.name + " = ", true);
                    emitExpression(a.right, false);
                }
                case Output.FieldAssignment fa -> {
                    emitExpression(fa.target, true);
                    emitLine(" -> " + fa.field + " = ", false);
                    emitExpression(fa.right, false);
                }
                case Output.FieldAccess fa -> {
                    emitExpression(fa.target, false);
                    emitLine(" -> " + fa.field, false);
                }
                case Output.Allocation a -> {
                    emitLine(String.format("NEW(%s)", a.type), doIndent);
                }
                case Output.Return r -> {
                    emitLine("return ", true);
                    emitExpression(r.retval, false);
                }
                case Output.BinaryExpression be -> {
                    emitExpression(be.left, doIndent);
                    emit(" " + be.op + " ");
                    emitExpression(be.right, false);
                }
                case Output.FunctionEvaluation fe -> {
                    emitLine(fe.name + "(", doIndent);
                    int argc = fe.arguments.size();
                    for (int i = 0; i < argc - 1; i++) {
                        emitExpression(fe.arguments.get(i), false);
                        emit(", ");
                    }
                    if (argc > 0) emitExpression(fe.arguments.getLast(), false);
                    emit(")");
                }
                default -> throw new RuntimeException("not implemented: " + e);
            }
        }

        void emitPrototype(Output.Function function) {
            emitLineNl(function.returnType + " " + function.cName + "(", true);
            indent();
            for (int i = 0; i < function.parameters.size(); i++) {
                var p = function.parameters.get(i);
                emitLineNl(p.type + " " + p.name + (i < function.parameters.size() - 1 ? "," : ""), true);
            }
            outdent();
            emitLineNl(");", true);
        }

        void emitStruct(Output.Struct struct) {
            var structName = struct.cName + "__struct";
            emitLineNl("struct " + structName + " {", true);
            indent();
            struct.fields.stream()
                    .forEach(f -> emitLineNl(f.type + " " + f.name + ";", true));
            outdent();
            emitLineNl("};", true);
            emitLineNl("typedef struct " + structName + "* " + struct.cName + ";", true);
        }

        void indent() {
            indent++;
        }

        void outdent() {
            indent--;
        }

        void emitLineNl(String line, boolean doIndent) {
            emitLine(line, doIndent);
            emit("\n");
        }

        void emitLine(String line, boolean doIndent) {
            for (int i = 0; i < indent && doIndent; i++) {
                emit("  ");
            }
                emit(line);
        }

        void emit(String text) {
            try {
                writer.append(text);
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
        register(sourceName, parsedStatement);
    }

    void register(String sourceName, T parsedStatement) {
        if (map.containsKey(sourceName)) {
            throw new RuntimeException(sourceName + " already defined");
        }
        map.put(sourceName, new RegistryValue<>(generateCName(parsedStatement), parsedStatement));
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

class FunctionRegistry extends Registry<FunctionRegistry.Function> {

    List<FunctionDeclaration> declaredFunctions = new ArrayList<>();

    @Override
    protected String getSourceName(FunctionRegistry.Function f) {
        return f.name();
    }

    @Override
    protected String generateCName(FunctionRegistry.Function f) {
        return f.target.map(n -> n + "__").orElse("") + f.name;
    }

    static record Function(
            Optional<String> target,
            String name,
            String type,
            List<Parameter> parameters) {
        record Parameter(String name, String type) {}
    }

}

@AllArgsConstructor
class FunctionCollector {
    FunctionRegistry functionRegistry;
    CompilationUnit cu;
    void run() {
        // TODO only top-level functions recognized for now
        List<FunctionDeclaration> functionDeclarations = new ArrayList<>();
        for (var stmt : cu.statements()) {
            switch (stmt) {
                case FunctionDeclaration fd -> {
                    functionRegistry.register(toFunction(fd));
                    functionDeclarations.add(fd);
                }
                default -> {}
            }
        }
        functionRegistry.declaredFunctions.addAll(functionDeclarations);
    }
    static FunctionRegistry.Function toFunction(FunctionDeclaration fd) {
        var sig = fd.signature();
        return new FunctionRegistry.Function(
                sig.receiver(),
                sig.name(),
                sig.returnType(),
                sig.parameters().stream()
                        .map(p -> new FunctionRegistry.Function.Parameter(p.name(), p.type().get()))
                        .toList());
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
    record StringType() implements Type {
        @Override
        public String getName() {
            return "String";
        }

        @Override
        public String getCRepresentation() {
            return "String";
        }

    }
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
