package com.github.musiKk;

public class Test {

    public static void main(String[] args) {
        Statement statement = new ExpressionStatement(new StringExpression("foo"));
        var string = evalStatement(statement);
        System.err.println(string);
    }

    private static String evalStatement(Statement s) {
        return switch (s) {
            case ExpressionStatement(Expression e) -> evalExpression(e);
            default -> throw new IllegalArgumentException();
        };
    }

    private static String evalExpression(Expression e) {
        return switch (e) {
            case StringExpression(String s) -> s;
            default -> throw new IllegalArgumentException();
        };
    }

}
