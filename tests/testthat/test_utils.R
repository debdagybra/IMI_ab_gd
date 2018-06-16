context("check the utils functions")

# correct_paths ----
test_that("correct_paths works only with character vector.", {
  expect_error(correct_paths(c(1,2,3)),"`x` must be a character vector.")
})
test_that("correct_paths works with vector of length > 1.", {
  expect_equal(correct_paths(c("1","2","3")),c("1","2","3"))
})
test_that("correct_paths changes `\` into `/`.", {
  expect_equal(correct_paths("\\\\test\\package\\"),"//test/package/")
  expect_equal(correct_paths("\\\\test/package\\"),"//test/package/")
})
test_that("correct_paths adds `/` at the end when missing only if it's an existing directory.", {
  expect_equal(correct_paths("package"),"package")
  expect_equal(correct_paths("D:/Maria/Maria_Analysis/mixcr-2.1.7"),"D:/Maria/Maria_Analysis/mixcr-2.1.7/")
  expect_equal(correct_paths("D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar"),"D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar")
})
test_that("correct_paths work with path style ./xxx/", {
  expect_equal(correct_paths("./data"),"D:/Projets R/IMIabgd/data/")
})

# check_path_program ----
test_that("check_path_program works with directory", {
  expect_equal(check_path_program("D:/Maria/Maria_Analysis/vdjtools-1.1.7", "vdjtools"),
               "D:/Maria/Maria_Analysis/vdjtools-1.1.7/vdjtools-1.1.7.jar")
  expect_equal(check_path_program("D:/Maria/Maria_Analysis/mixcr-2.1.7", "mixcr"),
               "D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar")
})
test_that("check_path_program gives an error with directory that doesn't exists or doesn't contains `program`", {
  expect_error(check_path_program("D:/Jeux/mixcr-2.1.7", "vdjtools"),
               paste0("File D:/Jeux/mixcr-2.1.7 is not valid.","\n",
                      "Please enter a valid directory or fullpath for `vdjtools.jar`"))
  expect_error(check_path_program("D:/Jeux/mixcr-2.1.7", "mixcr"),
               paste0("File D:/Jeux/mixcr-2.1.7 is not valid.","\n",
                      "Please enter a valid directory or fullpath for `mixcr.jar`"))
  expect_error(check_path_program("D:/Maria/", "vdjtools"))
})
test_that("check_path_program works with fullpath to file (only `program.jar`)", {
  expect_equal(check_path_program("D:/Maria/Maria_Analysis/vdjtools-1.1.7/vdjtools-1.1.7.jar", "vdjtools"),
               "D:/Maria/Maria_Analysis/vdjtools-1.1.7/vdjtools-1.1.7.jar", "vdjtools")
  expect_equal(check_path_program("D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar", "mixcr"),
               "D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar")
})
test_that("check_path_program gives an error with files that doesn't exists or aren't `program.jar`", {
  expect_error(check_path_program("D:/Jeux/vdjtools-1.1.7/vdjtools-1.1.7.jar", "vdjtools"))
  expect_error(check_path_program("D:/Psycho/icone.jpg", "vdjtools"))
})
test_that("check_path_program gives a suggestion when the correct file is in the same directory", {
  expect_error(check_path_program("D:/Maria/Maria_Analysis/vdjtools-1.1.7/vdjtools-1.1.6.jar", "vdjtools"),
               paste0('File D:/Maria/Maria_Analysis/vdjtools-1.1.7/vdjtools-1.1.6.jar is not valid.','\n',
                      'Did you mean: `D:/Maria/Maria_Analysis/vdjtools-1.1.7/vdjtools-1.1.7.jar`.'))
})


# # convert_decimal ----
# test_that("convert_decimal convert correctly decimals", {
#   # convert_decimal(path="D:/Maria/Treemaps et barplot/test/")
# })
#
# # txt_to_excel ----
# test_that("txt_to_excel convert correctly to .xlsx", {
#   # txt_to_excel(path="D:/Maria/Treemaps et barplot/test/")
#   # txt_to_excel(path="D:/Maria/Treemaps et barplot/",subfolder = "test")
# })
