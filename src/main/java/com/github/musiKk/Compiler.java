package com.github.musiKk;

import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.github.musiKk.parser.AstTyper;
import com.github.musiKk.parser.CompilationUnit;
import com.github.musiKk.parser.CompilationUnit.FunctionSignature;
import com.github.musiKk.parser.CompilationUnit.VariableDeclaration;
import com.github.musiKk.parser.Parser;
import com.github.musiKk.parser.TCompilationUnit;
import com.github.musiKk.parser.TCompilationUnit.TBinaryExpression;
import com.github.musiKk.parser.TCompilationUnit.TBlockExpression;
import com.github.musiKk.parser.TCompilationUnit.TDataDefinition;
import com.github.musiKk.parser.TCompilationUnit.TExpression;
import com.github.musiKk.parser.TCompilationUnit.TExpressionStatement;
import com.github.musiKk.parser.TCompilationUnit.TFunctionEvaluationExpression;
import com.github.musiKk.parser.TCompilationUnit.TNativeFunctionDeclaration;
import com.github.musiKk.parser.TCompilationUnit.TNumberExpression;
import com.github.musiKk.parser.TCompilationUnit.TStatement;
import com.github.musiKk.parser.TCompilationUnit.TStringExpression;
import com.github.musiKk.parser.TCompilationUnit.TUserFunctionDeclaration;
import com.github.musiKk.parser.TCompilationUnit.TVariableDeclaration;
import com.github.musiKk.parser.TCompilationUnit.TVariableExpression;
import com.github.musiKk.parser.Type;

import lombok.AllArgsConstructor;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

public class Compiler implements ConfigReader.ConfigTarget {

    @Setter
    private List<String> lookupPath = new ArrayList<>();
    @Setter
    private String target;

    private TypeNameMapper typeNameMapper = new TypeNameMapper();
    private FunctionNameMapper functionNameMapper = new FunctionNameMapper();

    public static void main(String[] args) {
        var compiler = new Compiler();
        ConfigReader.readConfig().applyConfig(compiler);
        compiler.compileProgram("test.tst");
    }

    /**
     * Compiles an executable starting at {@code pathString}.
     * @param pathString
     */
    public void compileProgram(String pathString) {
        Path.of(target).toFile().mkdirs();
        var resolvedPath = resolvePath(pathString);

        // TODO we only parse the main file for now
        var cu = parseCompilationUnit(resolvedPath);
        var tcu = typeCompilationUnit(cu);

        var output = new Output();
        compileCompilationUnit(tcu, output);

        try {
            output.emit(Path.of(target, resolvedPath.getFileName().toString().replace(".tst", ".c")));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private TCompilationUnit typeCompilationUnit(CompilationUnit cu) {
        var typer = new AstTyper(cu);
        // XXX this is a hack; the typer should be able to resolve this on its own
        typer.addPrototype(
            new FunctionSignature(
                Optional.empty(),
                "print",
                List.of(new VariableDeclaration("", "String")),
                "Void"
            )
        );
        typer.addPrototype(
            new FunctionSignature(
                Optional.of("Int"),
                "toString",
                Collections.emptyList(),
                "String"
            )
        );
        return typer.resolveTypes();
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

    private void compileCompilationUnit(TCompilationUnit cu, Output output) {
        var statementCollection = new Object() {
            List<TDataDefinition> dds = new ArrayList<>();
            List<TUserFunctionDeclaration> ufds = new ArrayList<>();
            List<TStatement> stmts = new ArrayList<>();
        };

        cu.statements().stream()
                .forEach(stmt -> {
                    switch (stmt) {
                        case TDataDefinition dd -> statementCollection.dds.add(dd);
                        case TUserFunctionDeclaration ufd -> statementCollection.ufds.add(ufd);
                        case TNativeFunctionDeclaration _ -> {}
                        default -> statementCollection.stmts.add(stmt);
                    }
                });

        statementCollection.dds.stream()
                .forEach(dd -> compileType(dd, output));
        statementCollection.ufds.stream()
                .forEach(ufd -> compileFunctionDefinition(ufd, output));

        compileMain(statementCollection.stmts, output);
    }

    private void compileMain(List<TStatement> statements, Output output) {
        var mainScope = new Compiler.Scope();
        var fb = output.function("main", "int");
        statements.stream()
                .forEach(s -> compileStatement(s, mainScope, fb));
        fb.addElement(new Output.Return(new Output.NumberExpression(0)));
        fb.finish();
    }

    private void compileType(TDataDefinition dd, Output output) {
        var typeCName = typeNameMapper.getCName(Type.of(dd.name()));

        var struct = output.struct(typeCName);
        dd.variableDeclarations().stream()
                .forEach(vd -> {
                    String propertyType = typeNameMapper.getCName(vd.type());
                    String propertyName = vd.name();

                    struct.field(propertyName, propertyType);
                });
        struct.finish();

        var fb = output.function(typeCName + "__new", typeCName);
        dd.variableDeclarations().stream()
                    .forEach(vd -> fb.parameter(vd.name(), typeNameMapper.getCName(vd.type())));
        fb.addElement(new Output.VariableDeclaration("ret", typeCName));
        fb.addElement(new Output.Assignment("ret", new Output.Allocation(typeCName)));
        dd.variableDeclarations().forEach(vd -> {
            fb.addElement(new Output.FieldAssignment(new Output.NameExpression("ret"), vd.name(), new Output.NameExpression(vd.name())));
        });
        fb.addElement(new Output.Return(new Output.NameExpression("ret")));
        fb.finish();

        functionNameMapper.add(typeCName, typeCName + "__new");
    }

    private void compileFunctionDefinition(TUserFunctionDeclaration ufd, Output output) {
        var signature = ufd.signature();

        var functionName = functionNameMapper.getCName(signature.receiver(), signature.name());
        var function = output.function(functionName, typeNameMapper.getCName(signature.returnType()));
        var scope = new Scope();
        signature.parameters().stream()
                .forEach(p -> {
                    function.parameter(p.name(), typeNameMapper.getCName(p.type()));
                    scope.variables.put(p.name(), typeNameMapper.getCName(p.type()));
                });

        var result = compileExpression(ufd.body(), scope, function);
        function.addElement(new Output.Return(result));
        function.finish();
    }

    class Scope {
        // name -> type cname
        Map<String, String> variables = new HashMap<>();

        int count = 0;

        public String newTemp(String type) {
            while (true) {
                String tmpCand = "tmp_" + (count++);
                if (!variables.containsKey(tmpCand)) {
                    variables.put(tmpCand, type);
                    return tmpCand;
                }
            }
        }
    }

    private void compileStatement(TStatement statement, Scope locals, Output.FunctionBuilder fb) {
        switch (statement) {
            case TExpressionStatement es -> fb.addElement(compileExpression(es.expression(), locals, fb));
            case TVariableDeclaration vd -> {
                Optional<Output.Expression> initStatement = vd.initializer().map(e -> compileExpression(e, locals, fb));
                locals.variables.put(vd.name(), typeNameMapper.getCName(vd.type()));
                fb.addElement(new Output.VariableDeclaration(vd.name(), typeNameMapper.getCName(vd.type())));
                initStatement.ifPresent(is -> fb.addElement(new Output.Assignment(vd.name(), is)));
            }
            default -> throw new RuntimeException(statement.toString());
        }
    }

    private Output.Expression compileExpression(TExpression parsedExpression, Scope locals, Output.FunctionBuilder fb) {
        return switch (parsedExpression) {
            case TNumberExpression(long n) -> new Output.NumberExpression(n);
            case TStringExpression se -> {
                var stringCType = typeNameMapper.getCName(se.type());
                var stringVarName = locals.newTemp(stringCType);
                fb.addElement(new Output.VariableDeclaration(stringVarName, stringCType));
                fb.addElement(new Output.Assignment(stringVarName, new Output.FunctionEvaluation("String__native_new_copy", List.of(new Output.StringLiteral(se.string())))));

                yield new Output.NameExpression(stringVarName);
            }
            case TVariableExpression ve when ve.target().isEmpty() -> {
                if (!locals.variables.containsKey(ve.name())) {
                    throw new RuntimeException("name " + ve.name() + " not found");
                }
                yield new Output.NameExpression(ve.name());
            }
            case TVariableExpression ve when ve.target().isPresent() -> {
                var compiledTarget = compileExpression(ve.target().get(), locals, fb);
                // TODO this could be module access in the future
                yield new Output.FieldAccess(compiledTarget, ve.name());
            }
            case TBinaryExpression be -> {
                var left = compileExpression(be.left(), locals, fb);
                var right = compileExpression(be.right(), locals, fb);
                yield new Output.BinaryExpression(left, be.operator().constantPattern, right);
            }
            case TBlockExpression be -> {
                var stmts = be.statements();
                var returnStatement = stmts.getLast();
                if (!(returnStatement instanceof TExpressionStatement)) {
                    throw new RuntimeException("last statement in block must be an expression: " + be);
                }
                for (int i = 0; i <= stmts.size() - 1; i++) {
                    var stmt = stmts.get(i);
                    compileStatement(stmt, locals, fb);
                }

                var returnExpression = ((TExpressionStatement) returnStatement).expression();
                var compiledReturn = compileExpression(returnExpression, locals, fb);
                var tmpName = locals.newTemp(typeNameMapper.getCName(returnExpression.type()));

                var resultVarDecl = new Output.VariableDeclaration(tmpName, typeNameMapper.getCName(returnExpression.type()));
                var resultVarAssign = new Output.Assignment(tmpName, compiledReturn);

                fb.addElement(resultVarDecl);
                fb.addElement(resultVarAssign);

                yield new Output.NameExpression(tmpName);
            }
            case TFunctionEvaluationExpression fee -> {
                List<Output.Expression> args = new ArrayList<>();
                fee.target()
                        .map(e -> compileExpression(e, locals, fb))
                        .ifPresent(args::add);
                fee.arguments().stream()
                        .map(e -> compileExpression(e, locals, fb))
                        .forEach(args::add);
                var fName = functionNameMapper.getCName(fee.target().map(TExpression::type), fee.name());

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

    class TypeNameMapper {
        String getCName(Type type) {
            return type.name();
        }
    }

    class FunctionNameMapper {
        final Map<String, String> map = new HashMap<>();
        String getCName(Optional<Type> target, String name) {
            var candName = target.map(t -> t.name() + "__").orElse("") + name;
            if (map.containsKey(candName)) {
                return map.get(candName);
            }
            return candName;
        }

        public void add(String sourceName, String cName) {
            if (map.containsKey(sourceName)) {
                throw new RuntimeException("duplicate function name " + sourceName);
            }
            map.put(sourceName, cName);
        }
    }

}
