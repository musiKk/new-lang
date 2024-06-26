package com.github.musiKk;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

public class ConfigReader {

    static Config readConfig() {
        var config = new Config();
        Properties properties = new Properties();
        try {
            properties.load(new FileInputStream("new-lang.cfg"));

            Arrays.stream(properties.getProperty("lookupPath", "").split(",")).forEach(config.lookupPath::add);
            config.target = properties.getProperty("target");

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return config;
    }

    static class Config {
        List<String> lookupPath = new ArrayList<>();
        String target;

        public void applyConfig(ConfigTarget ct) {
            ct.setLookupPath(lookupPath);
            ct.setTarget(target);
        }
    }

    interface ConfigTarget {
        void setLookupPath(List<String> lookupPath);
        void setTarget(String target);
    }

}
