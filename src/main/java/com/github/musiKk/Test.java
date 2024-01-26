package com.github.musiKk;

public class Test {

    public static void main(String[] args) {
        Object o = makeObj();
        switch (o) {
            case Data d when d.b -> System.out.println("yes");
            case Data d when !d.b -> System.out.println("no");
            case null -> System.err.println("it's null");
            default -> System.out.println("default");
        };
    }

    static Object makeObj() {
        return null;
    }

    record Data(boolean b) {}

}
