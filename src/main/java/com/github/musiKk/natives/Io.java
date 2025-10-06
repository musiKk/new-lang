package com.github.musiKk.natives;

import java.util.Arrays;
import java.util.stream.Collectors;

import com.github.musiKk.Runner;
import com.github.musiKk.Runner.ArrayValue;
import com.github.musiKk.Runner.BooleanValue;
import com.github.musiKk.Runner.Data;
import com.github.musiKk.Runner.NullValue;
import com.github.musiKk.Runner.NumberValue;
import com.github.musiKk.Runner.StringValue;
import com.github.musiKk.Runner.Value;

public class Io {

    public Runner.Value _print(Runner.StackFrame stackFrame) {
        var args = stackFrame.getVariable("s");
        String representation = stringify(args.variable().value());
        System.out.println(representation);
        return null;
    }
    public static String stringify(Value value) {
        return switch (value) {
            case NullValue _ -> "null";
            case NumberValue nv -> String.valueOf(nv.number());
            case BooleanValue bv -> String.valueOf(bv.value());
            case StringValue sv -> sv.string();
            case Data dv -> {
                var sb = new StringBuilder(dv.typeName() + "[");
                sb.append(dv.variables().entrySet().stream().map(e ->
                    e.getKey() + "=" + stringify(e.getValue())
                ).collect(Collectors.joining(", ")));
                sb.append("]");
                yield sb.toString();
            }
            case ArrayValue(Value[] values, String type) ->
                "[" + values.length + "]" + type + "(" + Arrays.stream(values)
                        .map(Io::stringify)
                        .collect(Collectors.joining(", "))
                + ")";
            default -> value.toString();
        };
    }

}
