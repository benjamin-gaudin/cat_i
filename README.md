# 🐱 Classical Academic λ-Terms Interpreter

An interpreter for λ calculus with some extensions.
Use of a parser, some files in `test` directory show a bit of the syntax.

## Usage

`cat_i exec [FILE]...`

## Extensions and characteristics :
- de Bruijn index
- Call by value
- Ascription
- Fixpoint
- Bool (no equality)
- Natural
- List
- Record / Tuple
- Variant
  + match on variant
- If then else construct
  + 'If'       (bool)
  + 'If nil'   (list)
  + 'If zero'  (nat)

## ⚖️  License

Distributed under the [Mozilla Public License Version 2.0](./LICENSE).
