# Set code type context

`with_type()` temporarily sets the active code type so that query
functions inside `code` use it without needing an explicit `type =`
argument. It works with any code type present in the database.

## Usage

``` r
with_type(type, code)
```

## Arguments

- type:

  A string naming the code type (e.g. `"ICD-10"`, `"SNOMED CT"`). This
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
with_type("ICD-10", DESCRIPTION("diabetes"))
with_type("BNF", CODES("0204 << Beta-Adrenoceptor Blocking Drugs >>"))
with_type(
  "SNOMED CT",
  CHILDREN(
    "770765001 << Proliferative retinopathy of right eye due to diabetes mellitus (disorder) >>"
  )
)
} # }
```
