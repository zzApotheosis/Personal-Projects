#!/bin/sh

java --class-path target/my-app-1.0-SNAPSHOT.jar io.example.app.App $@

# Run this if you want to simply execute the JAR directly (needs MANIFEST file)
#java -jar target/my-app-1.0-SNAPSHOT-jar-with-dependencies.jar $@
