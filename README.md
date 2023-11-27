# Instant
To build the compilers use `make`, via either `make` or `make all`. To clean files created via compilation use `make clean.
## Structure
Standard cabal project structure:
- `app` contains sources for executables
- `src` contains sources for library used by these executables
## Dependencies
All used Haskell packages are outlined in `Latte.cabal` file. There is one external dependency - sources inside `src/Latte/Grammar` were created using BNFC from `src/Latte/Grammar/Latte.cf` file
