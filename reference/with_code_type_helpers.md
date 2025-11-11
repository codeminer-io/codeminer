# With Code Type Functions

These functions allow users to specify a code type and execute CodeMiner
code within that context.

## Usage

``` r
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

medcode_aurum(code)

prodcode_aurum(code)
```

## Arguments

- code:

  The code to be processed.

## Value

The result of executing the code within the specified `code_type`
context.

## Details

Available functions:

- `icd10()`

- `icd9()`

- `read3()`

- `read2()`

- `sct()`

- `opcs4()`

- `phecode()`

- `read2_drugs()`

- `bnf()`

- `dmd()`

- `data_coding_3()`

- `data_coding_4()`

- `data_coding_5()`

- `data_coding_6()`

- `medcode_aurum()`

- `prodcode_aurum()`

## Examples

``` r
if (FALSE) { # \dontrun{
icd10(DESCRIPTION("diabetes"))
bnf(CODES("0204 << Beta-Adrenoceptor Blocking Drugs >>"))
sct(
  CHILDREN(
    "770765001 << Proliferative retinopathy of right eye due to diabetes mellitus (disorder) >>"
  )
)
} # }
```
