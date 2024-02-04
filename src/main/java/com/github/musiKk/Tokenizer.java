package com.github.musiKk;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class Tokenizer {

    List<Pattern> patterns = new ArrayList<>();

    {
        for (var tokenType : TokenType.values()) {
            if (tokenType.constantPattern != null) {
                patterns.add(new StaticPattern(tokenType.constantPattern, tokenType));
            }
        }

        patterns.add(new IdentifierPattern());
        patterns.add(new NumberPattern());
        patterns.add(new StringPattern());
        patterns.add(new CommentPattern());

        patterns.sort(Comparator.comparingInt(
            p -> switch (p) {
                case CommentPattern __ -> Integer.MAX_VALUE;
                case StaticPattern sp -> sp.pattern.length();
                default -> Integer.MIN_VALUE;
            }).reversed());
    }

    public static void main(String[] args) throws Exception {
        String programString = """
            foo("abc")
        """;
        var tokens = new Tokenizer().tokenize(programString);
        var out = tokens.tokens.stream().map(Token::image).collect(Collectors.joining(" "));
        System.err.println(out);
    }

    public Tokens tokenize(String programString) {
        List<Token> tokens = new ArrayList<>();

        int index = 0;
        while (index < programString.length()) {
            if (Character.isWhitespace(programString.charAt(index))) {
                index++;
                continue;
            }

            boolean gotMatch = false;
            for (var pattern : patterns) {
                var result = pattern.match(programString, index);
                if (result.isPresent()) {
                    tokens.add(result.get());
                    index = result.get().end();
                    gotMatch = true;
                    break;
                }
            }
            if (!gotMatch) {
                throw new RuntimeException("no match for >>>" + programString.substring(index) + "<<<");
            }
        }

        tokens.add(new Token(TokenType.EOF, "", index, index));

        return new Tokens(tokens);
    }

    interface Pattern {
        Optional<Token> match(String programString, int index);
    }

    static class StaticPattern implements Pattern {
        String pattern;
        TokenType tokenType;

        public StaticPattern(String pattern, TokenType tokenType) {
            this.pattern = pattern;
            this.tokenType = tokenType;
        }

        @Override
        public Optional<Token> match(String programString, int index) {
            if (programString.startsWith(pattern, index)) {
                return Optional.of(new Token(tokenType, pattern, index, index + pattern.length()));
            } else {
                return Optional.empty();
            }
        }
    }

    static class NumberPattern implements Pattern {
        @Override
        public Optional<Token> match(String programString, int index) {
            if (Character.isDigit(programString.charAt(index))) {
                int start = index;
                while (index < programString.length() && Character.isDigit(programString.charAt(index))) {
                    index++;
                }
                return Optional.of(new Token(TokenType.NUMBER, programString.substring(start, index), start, index));
            } else {
                return Optional.empty();
            }
        }
    }

    static class IdentifierPattern implements Pattern {
        @Override
        public Optional<Token> match(String programString, int index) {
            if (Character.isJavaIdentifierStart(programString.charAt(index))) {
                int start = index;
                while (index < programString.length() && Character.isJavaIdentifierPart(programString.charAt(index))) {
                    index++;
                }
                return Optional.of(new Token(TokenType.IDENTIFIER, programString.substring(start, index), start, index));
            } else {
                return Optional.empty();
            }
        }
    }

    static class StringPattern implements Pattern {
        @Override
        public Optional<Token> match(String programString, int index) {
            if (programString.charAt(index) == '"') {
                int start = index;
                index++;
                char prev = 0;
                while (index < programString.length()) {
                    char cur = programString.charAt(index);
                    if (cur == '"' && prev != '\\') {
                        break;
                    }
                    prev = cur;
                    index++;
                }
                if (index == programString.length()) {
                    throw new RuntimeException("unterminated string literal");
                }
                index += 1;
                return Optional.of(new Token(TokenType.STRING, programString.substring(start + 1, index - 1), start, index));
            } else {
                return Optional.empty();
            }
        }
    }

    static class CommentPattern implements Pattern {
        @Override
        public Optional<Token> match(String programString, int index) {
            if (programString.startsWith("//", index)) {
                int start = index;
                index += 2;
                while (index < programString.length() && programString.charAt(index) != '\n') {
                    index++;
                }
                return Optional.of(new Token(TokenType.COMMENT, programString.substring(start, index), start, index));
            } else {
                return Optional.empty();
            }
        }
    }

    record Token(TokenType type, String image, int start, int end) {
        int length() {
            return end - start;
        }
    }
    enum TokenType {
        IMPORT("import"),
        DATA("data"),
        DEF("def"),
        VAR("var"),

        NUMBER,
        STRING,

        EQUALS_EQUALS("=="), NOT_EQUALS("!="),
        PLUS("+"), MINUS("-"),
        STAR("*"), SLASH("/"),

        COMMENT,

        LBRACE("{"),
        RBRACE("}"),
        LPAREN("("),
        RPAREN(")"),

        COLON(":"),
        EQUALS("="),
        IDENTIFIER,
        DOT("."),
        COMMA(","),
        EOF;

        String constantPattern;

        private TokenType() {
            this(null);
        }
        private TokenType(String constantPattern) {
            this.constantPattern = constantPattern;
        }
    }

    static class Tokens {
        final List<Token> tokens;
        int index;

        Tokens(List<Token> tokens) {
            this.tokens = tokens;
        }

        public Token next() {
            while (tokens.get(index).type() == TokenType.COMMENT) {
                index++;
            }
            return tokens.get(index++);
        }

        public Token peek() {
            while (tokens.get(index).type() == TokenType.COMMENT) {
                index++;
            }
            return tokens.get(index);
        }

        public boolean matches(TokenType... types) {
            TokenType peekType = peek().type();
            for (var type : types) {
                if (peekType == type) {
                    return true;
                }
            }
            return false;
        }

        public Token peek(TokenType type) {
            var token = peek();
            if (token.type() != type) {
                throw new RuntimeException("expected " + type + " but got " + token);
            }
            return token;
        }

        public Token next(TokenType type) {
            var token = next();
            if (token.type() != type) {
                throw new RuntimeException("expected " + type + " but got " + token);
            }
            return token;
        }
    }

}
