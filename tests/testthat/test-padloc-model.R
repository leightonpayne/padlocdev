test_that("Construction of a padloc_model works", {
  # Construct a generic system
  generic_model <- padloc_model(
    maximum_separation = 3,
    minimum_core = 3,
    minimum_total = 4,
    core_genes = c("GenA", "GenB", "GenC"),
    optional_genes = c("GenD", "GenE"),
    prohibited_genes = c("GenF")
  )
  expect_true(valid_padloc_model(generic_model))
})

test_that("minimum_core is valid", {
  # Construct a generic system
  broken_model <- padloc_model(
    maximum_separation = 3,
    minimum_core = 3,
    minimum_total = 3,
    core_genes = c("GenA", "GenB"),
    optional_genes = c("GenC"),
    prohibited_genes = "NA"
  )
  expect_error(valid_padloc_model(broken_model))
})

test_that("minimum_total is valid", {
  # Construct a generic system
  broken_model <- padloc_model(
    maximum_separation = 3,
    minimum_core = 2,
    minimum_total = 4,
    core_genes = c("GenA", "GenB"),
    optional_genes = c("GenC"),
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
#     molecular_system(core_genes = "GenA", optional_genes = "GenA", prohibited_genes = "GenA"),
#     paste(
#       "core_genes should not overlap with optional_genes. Overlapping genes include: GenA.",
#       "core_genes should not overlap with prohibited_genes. Overlapping genes include: GenA.",
#       "optional_genes should not overlap with prohibited_genes. Overlapping genes include: GenA.",
#       sep = "\n"
#     )
#   )
# })
