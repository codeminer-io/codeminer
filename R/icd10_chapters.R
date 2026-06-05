# Static chapter ranges for ICD-10. TRUD does not ship a chapter column
# (TREE_DESCRIPTION is a sparse leaf-level display label, not a chapter), so
# the mapping is carried in the package. Ranges are stable across recent
# ICD-10 editions; short labels follow the convention from the #124 AC
# ("E10" -> "Endocrine").

icd10_chapters <- function() {
  tibble::tribble(
    ~start , ~end  , ~chapter           ,
    "A00"  , "B99" , "Infectious"       ,
    "C00"  , "D48" , "Neoplasms"        ,
    "D50"  , "D89" , "Blood"            ,
    "E00"  , "E89" , "Endocrine"        ,
    "F00"  , "F99" , "Mental"           ,
    "G00"  , "G99" , "Nervous"          ,
    "H00"  , "H59" , "Eye"              ,
    "H60"  , "H95" , "Ear"              ,
    "I00"  , "I99" , "Circulatory"      ,
    "J00"  , "J99" , "Respiratory"      ,
    "K00"  , "K93" , "Digestive"        ,
    "L00"  , "L99" , "Skin"             ,
    "M00"  , "M99" , "Musculoskeletal"  ,
    "N00"  , "N99" , "Genitourinary"    ,
    "O00"  , "O99" , "Pregnancy"        ,
    "P00"  , "P96" , "Perinatal"        ,
    "Q00"  , "Q99" , "Congenital"       ,
    "R00"  , "R99" , "Symptoms"         ,
    "S00"  , "T98" , "Injury"           ,
    "V01"  , "Y98" , "External causes"  ,
    "Z00"  , "Z99" , "Health status"    ,
    "U00"  , "U85" , "Special purposes"
  )
}

# Enumerate every 3-character ICD-10 prefix in the inclusive range
# [start, end]. Ranges may span letters (e.g. A00-B99, S00-T98), in which case
# every intermediate letter is filled out 00-99.
expand_icd10_range <- function(start, end) {
  start_letter <- stringr::str_sub(start, 1, 1)
  end_letter <- stringr::str_sub(end, 1, 1)
  start_num <- as.integer(stringr::str_sub(start, 2, 3))
  end_num <- as.integer(stringr::str_sub(end, 2, 3))

  letter_range <- LETTERS[
    which(LETTERS == start_letter):which(LETTERS == end_letter)
  ]

  purrr::map(letter_range, function(letter) {
    lo <- if (letter == start_letter) start_num else 0L
    hi <- if (letter == end_letter) end_num else 99L
    sprintf("%s%02d", letter, lo:hi)
  }) |>
    purrr::list_c()
}

# Expanded chapter lookup: one row per 3-char prefix -> chapter. Cheap to
# regenerate (~2000 rows) so not memoised.
icd10_chapter_lookup <- function() {
  icd10_chapters() |>
    dplyr::mutate(
      prefix = purrr::map2(.data$start, .data$end, expand_icd10_range)
    ) |>
    tidyr::unnest("prefix") |>
    dplyr::select("prefix", "chapter")
}
