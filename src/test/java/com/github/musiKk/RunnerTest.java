package com.github.musiKk;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.DynamicContainer;
import org.junit.jupiter.api.DynamicNode;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;

public class RunnerTest {

    @TestFactory
    public DynamicNode testFactory() {
        String basePathString = "src/test/resources/runner-tests/dynamic";
        Path basePath = Paths.get(basePathString);

        var testFiles = basePath.toFile().listFiles((dir, name) -> name.endsWith(".test.tst"));
        var tests = Arrays.stream(testFiles)
            .map(this::createTest).toList();

        return DynamicContainer.dynamicContainer("Runner tests", tests);
    }

    private DynamicNode createTest(File testFile) {
        var testName = testFile.getName().substring(0, testFile.getName().indexOf('.'));
        return DynamicTest.dynamicTest("run " + testName, () -> {
            var baos = new ByteArrayOutputStream();

            var previousOut = System.out;
            try {
                System.setOut(new PrintStream(baos));

                var runner = new Runner();
                runner.setLookupPath(List.of(testFile.getParent()));
                runner.runFile(testFile.getName());

                String expectedOutput;
                try (var s = Files.lines(testFile.toPath())) {
                    expectedOutput = s.dropWhile(l -> !l.equals("// EXPECTED-OUTPUT")).skip(1)
                        .map(l -> l.substring(2).trim()).reduce("", (a, b) -> a + b + "\n");
                }

                assertEquals(expectedOutput, baos.toString());
            } finally {
                System.setOut(previousOut);
            }
        });
    }

}
