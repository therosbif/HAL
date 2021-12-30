# HAL

Haskell LISP Interpreter written from scratch

## Requirements

[Haskell Stack](https://docs.haskellstack.org/en/stable/README/) version 2.1.3 at least

## Usage

### Clone the repo:

```bash
git clone git@github.com:therosbif/HAL.git

cd HAL
```

### Run the interpreter:

To launch the REPL run `stack run` or `stack run -- -i`

To load files enter the files' paths as arguments. (ex: `stack run -- ./lisp/stdlib.scm`)

The file paths can be used in combination with the `-i` flag to load files into the REPL.

The `load` command can also be used to load files from within the REPL. (ex: `> (load "./lisp/stdlib.scm")`)
