#! /bin/sh

rm ../bin/*.class
javac  -d ../bin/ *.java
../bin/2 "tsim --priority 15 --speed 20 ../assignment1/Lab1.map.txt" "java -cp ../bin Lab1 0 10 20"
