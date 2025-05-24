{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.gcc               # pulls in g++ for compiling C++
  ];

  # Set some sensible defaults when you enter the shell:
  shellHook = ''
    export CXXFLAGS="-O2 -std=c++17"
    echo "Welcome to the C++ dev shell!"
    echo "Compile with: g++ \$CXXFLAGS main.cpp -o main"
  '';
}

