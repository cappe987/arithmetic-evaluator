# Arithmetic-evaluator
Evaluates simple expressions using Shunting-Yard algorithm to handle infix. 
Pass an expression as argument when running the program or leave it empty to start a REPL. 
Currently does not handle negative numbers as input.

# Operators
+, -, *, /, **, %

# Dependencies
dotnet-core
FParsec

# How-to
## dotnet-core
```
cd bin/Debug/netcoreapp2.2
```
```
dotnet parsec.dll
```
## Mono
```
sudo gacutil -i DLLs/FParsec.dll
sudo gacutil -i DLLs/FParsecCS.dll
```
```
mono Program.exe
```

# Installing FParsec
```
dotnet add package FParsec --version 1.0.3
```