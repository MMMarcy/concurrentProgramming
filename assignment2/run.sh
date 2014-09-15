#! /bin/sh

rm ../bin/*.class
javac  -d ../bin/ *.java
../bin/2 "tsim --priority 15 --speed 5 ../assignment2/Lab2.map" "java -cp ../bin Lab2 22 22  20"
