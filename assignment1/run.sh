#! /bin/sh

rm ../bin/*.class
javac  -d ../bin/ *.java
../bin/2 "tsim --priority 15 --speed 5 ../assignment1/Lab1.map" "java -cp ../bin Lab1 15 3  5"
