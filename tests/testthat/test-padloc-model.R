test_that("Construction of a padloc_model works", {
  # Construct a generic system
  generic_model <- padloc_model(
    force_strand = FALSE,
    maximum_separation = 3,
    minimum_core = 3,
    minimum_total = 4,
    core_genes = c("GenA", "GenB", "GenC"),
    secondary_genes = c("GenD", "GenE"),
    neutral_genes = c("GenF"),
    prohibited_genes = c("GenG")
  )
  expect_true(valid_padloc_model(generic_model))
})

test_that("minimum_core is valid", {
  # Construct a generic system
  broken_model <- padloc_model(
    force_strand = FALSE,
    maximum_separation = 3,
    minimum_core = 3,
    minimum_total = 3,
    core_genes = c("GenA", "GenB"),
    secondary_genes = c("GenC"),
    neutral_genes = "NA",
    prohibited_genes = "NA"
  )
  expect_error(valid_padloc_model(broken_model))
})

test_that("minimum_total is valid", {
  # Construct a generic system
  broken_model <- padloc_model(
    force_strand = FALSE,
    maximum_separation = 3,
    minimum_core = 2,
    minimum_total = 4,
    core_genes = c("GenA", "GenB"),
    secondary_genes = c("GenC"),
    neutral_genes = "NA",
    prohibited_genes = "NA"
  )
  expect_error(valid_padloc_model(broken_model))
})

# test_that("NA is not a valid value for core_genes", {
#   expect_error(
#     molecular_system(core_genes = c("NA")),
#     "core_genes can not contain the special value \"NA\"."
#   )
#   expect_error(
#     molecular_system(core_genes = "GenA"),
#     NA
#   )
# })
#
# test_that("Genes can't overlap", {
#   expect_error(
#     molecular_system(core_genes = "GenA", secondary_genes = "GenA", prohibited_genes = "GenA"),
#     paste(
#       "core_genes should not overlap with secondary_genes. Overlapping genes include: GenA.",
#       "core_genes should not overlap with prohibited_genes. Overlapping genes include: GenA.",
#       "secondary_genes should not overlap with prohibited_genes. Overlapping genes include: GenA.",
#       sep = "\n"
#     )
#   )
# })
