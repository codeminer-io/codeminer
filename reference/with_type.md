# Set code type context

`with_type()` temporarily sets the active code type so that query
functions inside `code` use it without needing an explicit `type =`
argument.

Convenience wrappers (`icd10()`, `sct()`, etc.) call `with_type()` for
each supported code type.

## Usage

``` r
with_type(type, code)

icd10(code)

icd9(code)

read3(code)

read2(code)

sct(code)

opcs4(code)

phecode(code)

read2_drugs(code)

bnf(code)

dmd(code)

data_coding_3(code)

data_coding_4(code)

data_coding_5(code)

data_coding_6(code)
```

## Arguments

- type:

  A string naming the code type (e.g. `"icd10"`, `"sct"`). This
  corresponds to the `type` argument accepted by query functions such as
  [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md),
  [`CHILDREN()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md),
  and
  [`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md).

- code:

  The expression to evaluate within the code type context.

## Value

The result of evaluating `code`.

## Examples

``` r
if (FALSE) { # \dontrun{
with_type("icd10", DESCRIPTION("diabetes"))
icd10(DESCRIPTION("diabetes"))
bnf(CODES("0204 << Beta-Adrenoceptor Blocking Drugs >>"))
sct(
  CHILDREN(
    "770765001 << Proliferative retinopathy of right eye due to diabetes mellitus (disorder) >>"
  )
)
} # }
```
