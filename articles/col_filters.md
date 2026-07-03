# Column filters

``` r

library(codeminer)

create_dummy_database()
#> ✔ Dummy database ready to use!
```

Some tables contain rows that should usually be excluded — inactive
SNOMED CT concepts, unassured code mappings, retired descriptions.
Column filters (`col_filters`) let table authors declare which columns
are filterable, which values are available, and which are selected by
default; and let you override those defaults per call, per session, or
per scope.

The whole model fits in one sentence:

> **For each table a query touches: the call’s `col_filters` entry, else
> the session pin, else the metadata defaults — first match wins,
> replacing the whole set; `NA` means unfiltered at whatever level you
> apply it.**

## Motivation: filtering every table a query touches

A single query often reads more than one table.
[`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md)
reads a mapping table *and* the target lookup;
[`CHILDREN()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md)
reads a relationship table *and* the lookup that describes the results.
The `col_filters` argument is table-keyed, so one argument addresses
each table by its own name:

``` r

# `MAP()` reads the Read v3 -> ICD-10 mapping table. Keep only exact + general
# ("E", "G") mappings. The same table-keyed argument can also address the
# *target lookup* by name (demonstrated with the `Demo` lookup below).
MAP(
  "all",
  from = "Read v3",
  to = "ICD-10",
  col_filters = list(
    mapping = list("Read v3 > ICD-10" = list(mapping_status = c("E", "G")))
  )
) |>
  head()
#> ℹ Using "UKB v4" as the latest mapping version for
#> "Read v3 > ICD-10".
#> # A tibble: 0 × 8
#> # ℹ 8 variables: from <chr>, to <chr>, mapping_status <chr>, refine_flag <chr>,
#> #   add_code_flag <chr>, element_num <chr>, block_num <chr>,
#> #   icd10_dagger_asterisk <chr>
```

The top-level names are the table types (`lookup`, `relationship`,
`mapping`); the keys are code types, or `"from > to"` pairs for
mappings; the leaves are `column = c(values)` selections. Entries for
tables the query does not use are simply ignored, so one filter object
can be reused across different queries.

## Where filters come from

Filters are declared in table metadata, per column, with `values` (all
valid options) and `defaults` (applied when nothing overrides them). A
column may also carry two optional documentation fields, so filters are
self-describing:

- `description`: a single string explaining what the column means.
- `value_labels`: a named character vector mapping values to
  human-readable labels (its names must be a subset of `values`;
  labelling may be partial).

`defaults` is the value set applied by default. To **include every
value** by default, list them all — the column is then unrestricted by
default but still available to narrow. To **exclude everything** by
default, use an empty `defaults` (rarely wanted).

The rest of this vignette uses a small lookup registered on the dummy
database. `status` is filtered to active codes by default; `module`
lists both of its values as `defaults`, so all modules are included by
default:

``` r

demo_lookup <- data.frame(
  code = c("A", "B", "C"),
  description = c("apple", "banana", "cherry"),
  status = c("1", "1", "0"),
  module = c("core", "ext", "core")
)

add_lookup_table(
  demo_lookup,
  lookup_metadata(
    "Demo",
    col_filters = list(
      status = list(
        values = c("0", "1"),
        defaults = "1",
        description = "Whether the code is active.",
        value_labels = c("1" = "Active", "0" = "Inactive")
      ),
      module = list(
        values = c("core", "ext"),
        defaults = c("core", "ext"), # all values -> all included by default
        description = "Which module the code belongs to."
      )
    )
  )
)
#> ✔ Lookup table Demo_v0 added successfully.
```

Filters on an existing table can be changed without re-adding the data
via
[`update_lookup_metadata()`](https://codeminer-io.github.io/codeminer/reference/update_lookup_metadata.md)
(and the mapping/relationship equivalents).

## The `col_filters` argument

Every query function accepts `col_filters`:

- **`"default"`** (the default): session pins where set, else metadata
  defaults.
- **`NULL`** (or `NA`): no filtering for any table this query touches.
- **A table-keyed list** (as above): each entry **replaces** that
  table’s pinned/default filters wholesale; unnamed tables keep their
  pins/defaults. `NA` as a table entry un-filters that one table.

``` r

# The dummy Read v3 > ICD-10 mapping declares a mapping_status filter with
# defaults E, G, D. Restrict this call to exact ("E") mappings only:
MAP(
  "all",
  from = "Read v3",
  to = "ICD-10",
  col_filters = list(
    mapping = list("Read v3 > ICD-10" = list(mapping_status = "E"))
  )
) |>
  head()
#> # A tibble: 0 × 8
#> # ℹ 8 variables: from <chr>, to <chr>, mapping_status <chr>, refine_flag <chr>,
#> #   add_code_flag <chr>, element_num <chr>, block_num <chr>,
#> #   icd10_dagger_asterisk <chr>

# No filtering at all for this call:
MAP("all", from = "Read v3", to = "ICD-10", col_filters = NULL) |>
  head()
#> # A tibble: 6 × 8
#>   from  to    mapping_status refine_flag add_code_flag element_num block_num
#>   <chr> <chr> <chr>          <chr>       <chr>         <chr>       <chr>    
#> 1 X40J4 E109  D              C           P             0           0        
#> 2 X40J4 E10   A              M           P             0           0        
#> 3 X40J4 O240  R              C           C             0           0        
#> 4 C10.. E149  D              C           C             0           0        
#> 5 C10.. E14   A              M           P             0           0        
#> 6 C10.. E109  R              C           C             0           0        
#> # ℹ 1 more variable: icd10_dagger_asterisk <chr>
```

Because an entry replaces the whole set, “tweak one column, keep the
rest” is done in data space: take the current filters, amend, pass back
(see [the round-trip
workflow](#amending-filters-the-round-trip-workflow)).

Typos don’t fail silently: an entry that matches no registered table, a
column absent from its table, or a value outside a column’s registered
`values` each trigger a warning naming what *is* available.

### What a filter value means

The distinction that most often surprises people is **empty selection**
versus **unfiltered** — they are deliberately different. Reading from
the inside out:

**Per column** (inside a table entry):

| You set a column to | Meaning |
|----|----|
| `c("a", "b")` | keep rows where the column is `"a"` or `"b"` (a whitelist) |
| `character(0)` (JSON `[]`) | keep **no** rows — an empty allow-set excludes everything |
| `NA` (JSON `null`) | do **not** filter this column (unfiltered) |
| *column omitted* | not filtered — an overlay only constrains the columns it names |

**Per table** (a key under `lookup` / `mapping` / `relationship`):

| Table entry | Meaning |
|----|----|
| a named list of column filters | **replaces** that table’s defaults wholesale |
| `NA` | un-filter the whole table |
| *table omitted* | keeps its pins / metadata defaults |

**Whole argument** (`col_filters =`):

| Value              | Meaning                                        |
|--------------------|------------------------------------------------|
| `"default"`        | session pins where set, else metadata defaults |
| `NULL` / `NA`      | no filtering for any table this query touches  |
| a table-keyed list | as in the tables above                         |

So an empty selection is a real, useful state (“exclude everything,
return nothing”), and it is **not** the same as removing the constraint.
To stop filtering a column, set it to `NA` or omit it — not
`character(0)`. On the `Demo` table:

``` r

# Default: status filtered to "1"; module lists all values, so all included
CODES("all", type = "Demo")$code
#> ℹ Using "v0" as the latest lookup version for
#> "Demo".
#> [1] "A" "B"

# Empty selection on status excludes everything
CODES(
  "all",
  type = "Demo",
  col_filters = list(lookup = list(Demo = list(status = character(0))))
) |>
  nrow()
#> [1] 0

# NA leaves status unfiltered (all statuses returned)
CODES(
  "all",
  type = "Demo",
  col_filters = list(lookup = list(Demo = list(status = NA)))
)$code
#> [1] "A" "B" "C"
```

### The round-trip guarantee

Because each column’s default is its full applied value set — including
columns that are unrestricted by default (they simply list all their
values) —
[`get_col_filters()`](https://codeminer-io.github.io/codeminer/reference/get_col_filters.md)
output passed straight back reproduces the default query exactly:

``` r

identical(
  CODES("all", type = "Demo")$code,
  CODES("all", type = "Demo", col_filters = get_col_filters())$code
)
#> [1] TRUE

# Narrow a column by assigning a subset:
cf <- get_col_filters()
cf$lookup$Demo$module <- "core"
CODES("all", type = "Demo", col_filters = cf)$code
#> [1] "A"
```

Opt-in columns are still fully visible under
`get_col_filters(defaults_only = FALSE)` for discovery — they are only
absent from the applied-defaults shape.

## Session pins and scoped overrides

[`codeminer_set_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_col_filters.md)
pins filters for the session — the same shape, the same replacement
rule, applied whenever a query runs with `col_filters = "default"`:

``` r

codeminer_set_col_filters(
  mapping = list("Read v3 > ICD-10" = list(mapping_status = "E"))
)
#> ℹ Pin for mapping "Read v3 > ICD-10" replaces its default filters on
#>   refine_flag, element_num, and block_num.
#> ℹ To keep defaults, amend `get_col_filters()` output and pin that.

# The pin now applies to every query that touches this mapping table
nrow(MAP("all", from = "Read v3", to = "ICD-10"))
#> [1] 0

codeminer_clear_col_filters()
nrow(MAP("all", from = "Read v3", to = "ICD-10"))
#> [1] 5
```

Because a pin replaces a table’s defaults wholesale, the setter tells
you when a pin drops registered default columns.
`codeminer_set_col_filters(NA)` disables all filtering for the session;
[`codeminer_clear_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_col_filters.md)
returns to the metadata defaults.
[`codeminer_status()`](https://codeminer-io.github.io/codeminer/reference/codeminer_status.md)
shows the active state.

`with_col_filters(col_filters, code)` applies filters for the duration
of an expression, then restores the previous state (even on error):

``` r

with_col_filters(
  list(mapping = list("Read v3 > ICD-10" = list(mapping_status = "E"))),
  MAP("all", from = "Read v3", to = "ICD-10")
) |>
  nrow()
#> [1] 0
```

The scope is the *expression*: every codeminer read made while it
evaluates resolves against the temporary filters, so a multi-step
pipeline can be wrapped as a whole, or step by step:

``` r

# One scope for both steps: CHILDREN()'s lookup reads are filtered too
with_col_filters(
  list(lookup = list("SNOMED CT" = list(moduleId_concept = "999..."))),
  {
    mapped <- MAP(bnf_codes, from = "BNF", to = "SNOMED CT")
    CHILDREN(mapped)
  }
)

# Or move the boundary so only MAP() is filtered
with_col_filters(
  list(lookup = list("SNOMED CT" = list(moduleId_concept = "999..."))),
  MAP(bnf_codes, from = "BNF", to = "SNOMED CT")
) |>
  CHILDREN()
```

One subtlety: the scope covers query *construction*. Filters are
attached to a lazy table inside the read call, so a lazy
[`get_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/get_lookup_table.md)
result built inside a scope keeps its filters even if you
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html)
it after the scope has ended.

## Amending filters: the round-trip workflow

[`get_col_filters()`](https://codeminer-io.github.io/codeminer/reference/get_col_filters.md)
returns the currently registered default filters for every table, in
exactly the shape the `col_filters` argument (and the pins) accept.
Amend it with plain assignment and pass it back:

``` r

cf <- get_col_filters()
print(cf)
#> <codeminer_col_filters>
#> ├─lookup
#> │ └─Demo
#> │   ├─status: 1
#> │   └─module: core, ext
#> └─mapping
#>   ├─Read v2 > ICD-10
#>   │ └─icd10_code_def: 1, 15, 3, 5, 7, 8
#>   ├─Read v2 > Read v3
#>   │ └─IS_ASSURED: 1
#>   ├─Read v3 > ICD-10
#>   │ ├─mapping_status: E, G, D
#>   │ ├─refine_flag: C, P
#>   │ ├─element_num: 0
#>   │ └─block_num: 0
#>   └─Read v3 > Read v2
#>     └─IS_ASSURED: 1

# Change one column's selection while keeping everything else
cf$mapping[["Read v3 > ICD-10"]]$mapping_status <- c("E", "G")

nrow(MAP("all", from = "Read v3", to = "ICD-10", col_filters = cf))
#> [1] 0
```

The same object works with `codeminer_set_col_filters(col_filters = cf)`
and `with_col_filters(cf, ...)`. Deleting a column from the object
(`cf$mapping[["Read v3 > ICD-10"]]$refine_flag <- NULL`) removes that
override, and setting a table’s entry to `NA` un-filters the table.
Columns need not have a registered filter specification — any real
column of the table can be filtered (like `moduleId_concept` in the
motivating example).

## Where should filters live?

- **Defaults that should always apply** belong in the database metadata
  (`update_*_metadata()`): they travel with the database file, so every
  R process — including fresh parallel workers — gets them on connect.
- **Session-wide tweaks** are pins, set where you connect. Fresh R
  processes start with no pins; the place you connect is the place you
  pin.
- **Per-call or per-scope deviations** use the `col_filters` argument or
  [`with_col_filters()`](https://codeminer-io.github.io/codeminer/reference/with_col_filters.md).

## Discovering available filters

`get_col_filters(defaults_only = FALSE)` returns the full specification
— every filterable column with all its `values` and the `defaults`, plus
any `description` and `value_labels` the filter declares — which is what
a UI needs to render labelled filter choices:

``` r

get_col_filters(defaults_only = FALSE)$mapping[["Read v3 > ICD-10"]]
#> $mapping_status
#> $mapping_status$values
#> [1] "E" "G" "D" "R" "A" "U"
#> 
#> $mapping_status$defaults
#> [1] "E" "G" "D"
#> 
#> $mapping_status$description
#> [1] "Nature of the Read v3 to ICD-10 mapping."
#> 
#> $mapping_status$value_labels
#>                             E                             G 
#>    "Exact one-to-one mapping" "Target concept more general" 
#>                             D                             R 
#>             "Default mapping"           "Requires checking" 
#>                             A                             U 
#>         "Alternative mapping"      "Unspecified (not used)" 
#> 
#> 
#> $refine_flag
#> $refine_flag$values
#> [1] "C" "P" "M"
#> 
#> $refine_flag$defaults
#> [1] "C" "P"
#> 
#> $refine_flag$description
#> [1] "Whether the target ICD-10 code is refined enough to be acceptable."
#> 
#> $refine_flag$value_labels
#>                                              C 
#>                           "Completely refined" 
#>                                              P 
#> "Possible but not mandatory to refine further" 
#>                                              M 
#>                  "Mandatory to refine further" 
#> 
#> 
#> $element_num
#> $element_num$values
#> [1] "0" "1" "2" "3"
#> 
#> $element_num$defaults
#> [1] "0"
#> 
#> $element_num$description
#> [1] "Element number grouping alternative target codes; starts at 0."
#> 
#> 
#> $block_num
#> $block_num$values
#>  [1] "0"  "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11" "12" "13" "14"
#> 
#> $block_num$defaults
#> [1] "0"
#> 
#> $block_num$description
#> [1] "Block number identifying a complete set of target codes; numbered from 0."
```
