# Migrate the codeminer database to the current schema version

Walks the registered migration chain from the database's stamped schema
version up to `current_schema_version()`. If the database has no stamp
(built before the stamping migration landed) it is treated as schema
version 0 and migrated forward from there.

## Usage

``` r
migrate_database(dry_run = FALSE)
```

## Arguments

- dry_run:

  If `TRUE`, prints the migrations that would run and returns without
  modifying the database.

## Value

The new schema version, invisibly. `NULL` if nothing to do.

## See also

Other Database management:
[`update_lookup_metadata()`](https://codeminer-io.github.io/codeminer/reference/update_lookup_metadata.md),
[`update_mapping_metadata()`](https://codeminer-io.github.io/codeminer/reference/update_mapping_metadata.md),
[`update_relationship_metadata()`](https://codeminer-io.github.io/codeminer/reference/update_relationship_metadata.md)
