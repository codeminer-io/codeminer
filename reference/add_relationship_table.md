# Add a relationship table to the database

Add a relationship table to the database together with its metadata.
Note that it is not possible to overwrite an existing relationship
table.

## Usage

``` r
add_relationship_table(table, metadata)
```

## Arguments

- table:

  The relationship table to add, should be coercible to a `data.frame`

- metadata:

  The relationship metadata, as specified by
  [`relationship_metadata()`](https://codeminer-io.github.io/codeminer/reference/relationship_metadata.md).

## Value

`TRUE` invisibly if successful, `FALSE` invisibly if the relationship
table already exists.

## Details

Relationship tables are indexed by their `code_type` and
`relationship_version`, specified in
[`relationship_metadata()`](https://codeminer-io.github.io/codeminer/reference/relationship_metadata.md).
This index needs to be unique and is used to identify the relationship
table in the database. If a relationship table with the same `code_type`
and `relationship_version` already exists, the function will emit a
warning and return `FALSE` (invisibly) without any effect. Use a
different `relationship_version` to add a new version of the
relationship table for the given `code_type`.

## See also

[`relationship_metadata()`](https://codeminer-io.github.io/codeminer/reference/relationship_metadata.md)
for the specification of the metadata.

## Examples

``` r
relationship_table <- data.frame(
  source = c("A", "B", "C"),
  target = c("B", "C", "D"),
  type = "child"
)
relationship_table
#>   source target  type
#> 1      A      B child
#> 2      B      C child
#> 3      C      D child

# Using a temporary database
Sys.setenv(CODEMINER_DB_PATH = tempfile())
build_database()
#> Creating new database at /tmp/Rtmpqz7mVJ/file1b1367d926f4
add_relationship_table(
  relationship_table,
  relationship_metadata(
    "test",
    relationship_version = "v1",
    from_col = "source",
    to_col = "target",
    type_col = "type",
    child_parent_relationship_code = "child"
  )
)
#> ✔ Relationship table test_relationship_v1 added successfully.
```
