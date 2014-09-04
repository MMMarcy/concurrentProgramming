#! /bin/sh

rm ../bin/*.class
javac  -d ../bin/ *.java
../bin/2 "tsim ../assignment1/Lab1.map.txt" "java -cp ../bin Lab1 10 10 20"
