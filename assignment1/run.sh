#! /bin/sh

rm ../bin/*.class
javac  -d ../bin/ *.java
../bin/2 "tsim --priority 15 --speed 10 ../assignment1/Lab1.map.txt" "java -cp ../bin Lab1 22 22  20"
