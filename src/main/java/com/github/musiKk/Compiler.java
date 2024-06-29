package com.github.musiKk;

import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class Compiler implements ConfigReader.ConfigTarget {

    private List<String> lookupPath = new ArrayList<>();
    private String target;

    @Override
    public void setLookupPath(List<String> lookupPath) {
        this.lookupPath = lookupPath;
    }

    @Override
    public void setTarget(String target) {
        this.target = target;
    }

    public Compiler() {
    }

    public static void main(String[] args) {
        var compiler = new Compiler();
        ConfigReader.readConfig().applyConfig(compiler);
        compiler.compile("test.tst");
    }

    public void compile(String pathString) {
        Path.of(target).toFile().mkdirs();
        var resolvedPath = Path.of(pathString);
        if (!resolvedPath.isAbsolute()) {
            resolvedPath = lookupPath.stream()
                    .map(p -> Path.of(p, pathString))
                    .filter(Files::exists)
                    .findFirst()
                    .orElseThrow(() -> new RuntimeException("file not found: " + pathString));
        }

        var parser = new Parser();
        String fileContent;
        try {
            fileContent = Files.readString(resolvedPath);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        var cu = parser.parseCompilationUnit(new Tokenizer().tokenize(fileContent));
        try (var output = new Output(Path.of(target, pathString.replace(".tst", ".c")))) {
            compileCompilationUnit(cu, output);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private void compileCompilationUnit(CompilationUnit cu, Output output) {
        cu.statements().stream()
                .filter(s -> s instanceof FunctionDeclaration)
                .map(s -> (FunctionDeclaration) s)
                .forEach(s -> compileFunctionDeclaration(s, output));
    }

    private void compileFunctionDeclaration(FunctionDeclaration fd, Output output) {
        var signature = fd.signature();
        output.write(signature.returnType(), " ", signature.name(), "(");
        for (int i = 0; i < signature.parameters().size(); i++) {
            var parameter = signature.parameters().get(i);
            output.write(parameter.type().get(), " ", parameter.name());
            if (i < signature.parameters().size() - 1) {
                output.write(", ");
            }
        }
        output.writeln(") {");

        switch (fd) {
            case UserFunctionDeclaration ufd -> {
                compileExpression(ufd.body(), output);
            }
            case NativeFunctionDeclaration nfd -> throw new RuntimeException("native functions not supported");
        }
        output.writeln("}");
    }

    private void compileExpression(Expression e, Output output) {
        switch (e) {
            case BlockExpression be -> {
                for (var statement : be.statements()) {
                    compileStatement(statement, output);
                }
            }
            case NumberExpression ne -> output.write(Long.toString(ne.number()));
            case StringExpression se -> output.write("\"", se.string(), "\"");
            case VariableExpression ve when ve.target().isEmpty() ->
                output.write(ve.name());
            case BinaryExpression be -> {
                output.write("(");
                compileExpression(be.left(), output);
                switch (be.operator()) {
                    case PLUS -> output.write(" + ");
                    case MINUS -> output.write(" - ");
                    case STAR -> output.write(" * ");
                    case SLASH -> output.write(" / ");
                    default -> throw new RuntimeException("unsupported operator " + be.operator());
                }
                compileExpression(be.right(), output);
                output.write(")");
            }
            case AssignmentExpression ae -> {
                compileExpression(ae.target(), output);
                compileExpression(ae.value(), output);
                output.writeln(";");
            }
            default -> throw new RuntimeException("unsupported expression " + e);
        }
    }

    private void compileStatement(Statement s, Output output) {
        switch (s) {
            case ExpressionStatement es -> compileExpression(es.expression(), output);
            case VariableDeclaration vd -> {
                output.write(vd.type().get(), " ", vd.name());
                if (vd.initializer().isPresent()) {
                    output.write(" = ");
                    compileExpression(vd.initializer().get(), output);
                }
                output.writeln(";");
            }
            default -> throw new RuntimeException("unsupported statement " + s);
        }
    }

    static class Output implements AutoCloseable {
        Writer output;
        public Output(Path path) {
            try {
                output = Files.newBufferedWriter(path);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        void write(String... ss) {
            try {
                for (var s : ss) {
                    output.write(s);
                }
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        void writeln(String... ss) {
            write(ss);
            write("\n");
        }
        @Override
        public void close() throws Exception {
            output.close();
        }
    }

}
