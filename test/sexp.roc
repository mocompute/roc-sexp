app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    sexp: "../src/main.roc",
}

import sexp.Sexp


import "sexp.data.txt" as source : List U8

main =
    res = List.range {start: At 0, end: Length 1000000}
        |> List.map \_ ->
            when Sexp.parse source is
                Ok (sexp) -> Sexp.toString sexp
                Err err -> "An error occured: $(Inspect.toStr err)"
    Task.ok {}
