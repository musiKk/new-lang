package com.github.musiKk.parser;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.ToString;
import lombok.experimental.Accessors;

@ToString
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class Type {

    public static class Builtin {
        public static final Type ANY = Type.of("core", "Any");
        public static final Type INT = Type.of("core", "Int");
        public static final Type BOOL = Type.of("core", "Bool");
        public static final Type STRING = Type.of("core", "String");
    }

    @Accessors(fluent = true)
    @Getter
    private final String module;
    @Accessors(fluent = true)
    @Getter
    private final String name;

    public static Type of(String module, String name) {
        return CACHE.computeIfAbsent(new Key(module, name), k -> new Type(k.module, k.name));
    }
    public static Type.FunctionType of(String module, Optional<Type> receiver, String name, Type returnType, List<Type> parameters) {
        return FCACHE.computeIfAbsent(
                new TargetAndName(module, receiver, name),
                k -> new FunctionType(k.module, receiver, k.name, returnType, parameters));
    }

    private static final Map<Key, Type> CACHE = new HashMap<>();
    private static final Map<TargetAndName, FunctionType> FCACHE = new HashMap<>();

    private record Key(String module, String name) {}
    private record TargetAndName(String module, Optional<Type> target, String name) {}

    @ToString
    static class FunctionType extends Type {
        @Accessors(fluent = true)
        @Getter
        private final Optional<Type> receiver;
        @Accessors(fluent = true)
        @Getter
        private final Type returnType;
        @Accessors(fluent = true)
        @Getter
        private final List<Type> parameters;
        FunctionType(String module, Optional<Type> receiver, String name, Type returnType, List<Type> parameters) {
            super(module, name);
            this.receiver = receiver;
            this.returnType = returnType;
            this.parameters = parameters;
        }
    }
}
