app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    sexp: "../src/main.roc",
    }

import pf.Stdout
import sexp.Sexp

main =
    s = Sexp.number 5
    dbg s


    Stdout.line! "Nothing here yet: $(Inspect.toStr s)"
