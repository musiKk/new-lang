package com.github.musiKk.natives;

import com.github.musiKk.Runner;

public class Time {

    public Runner.Value _time(Runner.StackFrame stackFrame) {
        return new Runner.NumberValue(System.currentTimeMillis());
    }

}
