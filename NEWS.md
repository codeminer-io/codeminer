# codeminer 0.2.0

* New `codeminer.col_filters` option: a recipe-level column-filter setting that
  every query applies when its `col_filters` argument is `"default"`. It takes
  the same shapes as the argument, for example
  `options(codeminer.col_filters = list(relationship = list("SNOMED CT" = NA)))`
  to include inactive SNOMED CT relationships throughout a script, or `NA` to
  turn filtering off. An explicit `col_filters` argument takes priority over
  the option; nothing changes when it is unset. `codeminer_status()` shows it.
  A value left in `.Rprofile` from the pre-2026 flat form of this option is
  now an error at the first query.

* `codeminer_set_col_filters()` and `codeminer_clear_col_filters()` are
  removed: the option is the one session-wide setting, so there is no second
  layer to shadow it. `with_col_filters()` now works by setting the option
  for the duration of its expression, merging its entries into the current
  value by table key, so the precedence is the `col_filters` argument, then
  the option (or an enclosing `with_col_filters()`), then the metadata
  defaults.

* New vignette `vignette("configure-queries")`: a task-led guide to setting
  table versions and column filters per call, per session and per script, with
  SNOMED CT scenarios (inactive concepts, inactive relationships, one module,
  inactive map rows).
