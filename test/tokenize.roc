app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    sexp: "../src/main.roc",
}

import pf.Stdout
import pf.Arg
import sexp.Token

usage = "Usage: tokenize <string>"

loop : Token.State -> Task [Step Token.State, Done Token.State] _
loop = \state ->
    when Token.next state is
        Ok (tok, nextState) ->
            Stdout.line! (Inspect.toStr tok)
            Task.ok (Step nextState)
        Err err ->
            Stdout.line! (Inspect.toStr err)
            Task.ok (Done state)

main =
    args = Arg.list! {}
    dbg args

    if List.len args < 2 then
        Task.err (Exit 1 usage)
    else
        source = List.get args 1 |> Result.withDefault "?"
        s = Token.stateInit (Str.toUtf8 source)
        Task.loop s loop |> Task.map \_ -> {}
