package com.github.musiKk.parser;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import java.util.Optional;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.github.musiKk.Tokenizer;
import com.github.musiKk.Tokenizer.TokenType;
import com.github.musiKk.parser.CompilationUnit.ArrayCreationExpression;
import com.github.musiKk.parser.CompilationUnit.ArrayLookupExpression;
import com.github.musiKk.parser.CompilationUnit.AssignmentExpression;
import com.github.musiKk.parser.CompilationUnit.BinaryExpression;
import com.github.musiKk.parser.CompilationUnit.BlockExpression;
import com.github.musiKk.parser.CompilationUnit.DataDefinition;
import com.github.musiKk.parser.CompilationUnit.Expression;
import com.github.musiKk.parser.CompilationUnit.FunctionEvaluationExpression;
import com.github.musiKk.parser.CompilationUnit.FunctionSignature;
import com.github.musiKk.parser.CompilationUnit.NumberExpression;
import com.github.musiKk.parser.CompilationUnit.Statement;
import com.github.musiKk.parser.CompilationUnit.StringExpression;
import com.github.musiKk.parser.CompilationUnit.TraitDefinition;
import com.github.musiKk.parser.CompilationUnit.UserFunctionDeclaration;
import com.github.musiKk.parser.CompilationUnit.VariableDeclaration;
import com.github.musiKk.parser.CompilationUnit.VariableExpression;

public class ParserTest {

    @ParameterizedTest
    @MethodSource("statements")
    public void testStatementParse(String code, Statement expected) {
        var tokens = new Tokenizer().tokenize(code);
        var parsed = new Parser().parseStatement(tokens);
        assertEquals(expected, parsed);
    }

    @ParameterizedTest
    @MethodSource("expressions")
    public void testExpressionParse(String code, Expression expected) {
        var tokens = new Tokenizer().tokenize(code);
        var parsed = new Parser().parseExpression(tokens);
        assertEquals(expected, parsed);
    }

    private static Object[][] statements() {
        return new Object[][] {
            {
                "data Foo { x: int }",
                new DataDefinition("Foo", List.of(new VariableDeclaration("x", "int")))
            }, {
                "var x = 1",
                new VariableDeclaration("x", new NumberExpression(1))
            }, {
                "def foo() = {}",
                new UserFunctionDeclaration(new FunctionSignature(Optional.empty(), "foo", List.of(), "void"), new BlockExpression(List.of()))
            }, {
                "def foo(i) = {}",
                new UserFunctionDeclaration(new FunctionSignature(Optional.empty(), "foo", List.of(new VariableDeclaration("i")), "void"), new BlockExpression(List.of()))
            }, {
                "def Foo.foo() = {}",
                new UserFunctionDeclaration(new FunctionSignature(Optional.of("Foo"), "foo", List.of(), "void"), new BlockExpression(List.of()))
            }, {
                "trait Dog { def bark() }",
                new TraitDefinition("Dog", List.of(new FunctionSignature(Optional.empty(), "bark", List.of(), "void")))
            }
        };
    }

    private static Object[][] expressions() {
        return new Object[][] {
            {
                "foo()",
                new FunctionEvaluationExpression("foo", List.of())
            }, {
                "foo(1)",
                new FunctionEvaluationExpression("foo", List.of(new NumberExpression(1)))
            }, {
                "foo(\"foo\")",
                new FunctionEvaluationExpression("foo", List.of(new StringExpression("foo")))
            }, {
                "foo.bar",
                new VariableExpression(new VariableExpression("foo"), "bar")
            }, {
                "foo.bar()",
                new FunctionEvaluationExpression(new VariableExpression("foo"), "bar", List.of())
            }, {
                "a + b",
                new BinaryExpression(new VariableExpression("a"), TokenType.PLUS, new VariableExpression("b"))
            }, {
                "a + b * c",
                new BinaryExpression(
                    new VariableExpression("a"),
                    TokenType.PLUS,
                    new BinaryExpression(
                        new VariableExpression("b"),
                        TokenType.STAR,
                        new VariableExpression("c")))
            }, {
                "a + b.c",
                new BinaryExpression(
                    new VariableExpression("a"),
                    TokenType.PLUS,
                    new VariableExpression(new VariableExpression("b"), "c"))
            }, {
                "a + b.c()",
                new BinaryExpression(
                    new VariableExpression("a"),
                    TokenType.PLUS,
                    new FunctionEvaluationExpression(new VariableExpression("b"), "c", List.of()))
            }, {
                "b.c() + a",
                new BinaryExpression(
                    new FunctionEvaluationExpression(new VariableExpression("b"), "c", List.of()),
                    TokenType.PLUS,
                    new VariableExpression("a"))
            }, {
                "a.b().c.d()",
                new FunctionEvaluationExpression(
                    new VariableExpression(new FunctionEvaluationExpression(new VariableExpression("a"), "b", List.of()), "c"),
                    "d",
                    List.of())
            }, {
                "a.b.c",
                new VariableExpression(new VariableExpression(new VariableExpression("a"), "b"), "c")
            }, {
                "a = b",
                new AssignmentExpression(new VariableExpression("a"), new VariableExpression("b"))
            }, {
                "a.b = c",
                new AssignmentExpression(new VariableExpression(new VariableExpression("a"), "b"), new VariableExpression("c"))
            }, {
                "a = b == c",
                new AssignmentExpression(
                    new VariableExpression("a"),
                    new BinaryExpression(
                        new VariableExpression("b"),
                        TokenType.EQUALS_EQUALS,
                        new VariableExpression("c")))
            }, {
                "a",
                new VariableExpression("a"),
            }, {
                "a = b = c",
                new AssignmentExpression(
                    new VariableExpression("a"),
                    new AssignmentExpression(
                        new VariableExpression("b"),
                        new VariableExpression("c")))
            }, {
                "[]int()",
                new ArrayCreationExpression("int", Optional.empty(), List.of())
            }, {
                "[1]int()",
                new ArrayCreationExpression("int", Optional.of(new NumberExpression(1)), List.of())
            }, {
                "[arr.len]int()",
                new ArrayCreationExpression("int", Optional.of(new VariableExpression(new VariableExpression("arr"), "len")), List.of())
            }, {
                "[]int(2, 3)",
                new ArrayCreationExpression("int", Optional.empty(), List.of(new NumberExpression(2), new NumberExpression(3)))
            }, {
                "arr[1]",
                new ArrayLookupExpression(new VariableExpression("arr"), new NumberExpression(1))
            }, {
                "arr[1][2]",
                new ArrayLookupExpression(
                    new ArrayLookupExpression(new VariableExpression("arr"), new NumberExpression(1)),
                    new NumberExpression(2))
            }, {
                "arr[1].foo",
                new VariableExpression(
                    new ArrayLookupExpression(new VariableExpression("arr"), new NumberExpression(1)),
                    "foo")
            }, {
                "arr[1].foo()",
                new FunctionEvaluationExpression(
                    new ArrayLookupExpression(new VariableExpression("arr"), new NumberExpression(1)),
                    "foo",
                    List.of())
            }, {
                "foo()[1]",
                new ArrayLookupExpression(
                    new FunctionEvaluationExpression("foo", List.of()),
                    new NumberExpression(1))}
        };
    }

}
