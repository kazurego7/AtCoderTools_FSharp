@echo off
if exist %1 (
    echo "%1 has exist. use other name."
) else (
mkdir %1
cd %1
dotnet new acfs -o A
dotnet new acfs -o B
dotnet new acfs -o C
dotnet new acfs -o D
dotnet new acfs -o E
dotnet new acfs -o F
cd ../
)