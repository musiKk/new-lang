package com.github.musiKk.parser;

import java.util.HashMap;
import java.util.Map;

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

    private static final Map<Key, Type> CACHE = new HashMap<>();

    private record Key(String module, String name) {}
}
