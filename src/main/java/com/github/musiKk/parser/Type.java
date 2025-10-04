package com.github.musiKk.parser;

import java.util.HashMap;
import java.util.Map;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.ToString;

@ToString
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class Type {

    private final String name;

    public static Type of(String name) {
        return CACHE.computeIfAbsent(name, Type::new);
    }

    private static final Map<String, Type> CACHE = new HashMap<>();
}
