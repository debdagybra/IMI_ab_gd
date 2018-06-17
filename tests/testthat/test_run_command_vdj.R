
context("check the run_command_vdj function")

# inital_wd <- getwd()
# setwd(paste0(getwd(),"/test/testthat/"))

# run_command_vdj with basic commands ----
test_that("run_command_vdj gives correct command to console with user's specific command.", {
  expect_equal(
    run_command_vdj(
      java_param = "no java",
      path_vdjtools = "",
      path_mixcr = "",
      input_dir = "D:/",
      input_filename = "somefile",
      input_ext = ".dd",
      check_input_filename = FALSE,
      output_dir = "D:/",
      output_filename = "res_somefile",
      output_ext = ".rdd",
      command_vdj = "my own command",
      report_name = "",
      replace = FALSE,
      string_only = TRUE
    ),
    noquote(paste0("no java   my own command   somefile \"D:/res_somefile.rdd \""))
  )
})

test_that("run_command_vdj gives correct command to console with align.", {
  expect_equal(
    run_command_vdj(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_vdjtools = "",
      path_mixcr = "./mixcr-2.1.7/mixcr.jar",
      input_dir = "./testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
      input_filename = "70-SR13-Gamma_S70_L001_R1_001.fastq.gz 70-SR13-Gamma_S70_L001_R2_001.fastq.gz",
      input_ext = ".fastq.gz",
      check_input_filename = FALSE,
      output_filename = "70-SR13-Gamma_S70_L001_R1R2_001",
      output_ext = ".vdjca",
      command_vdj = "align -OvParameters.geneFeatureToAlign=VTranscript",
      report_name = "alignmentReport",
      replace = FALSE,
      fullpath = FALSE,
      string_only = TRUE
    ),
    noquote(paste0(
      "java -Xmx4g -Xms3g -jar  ", getwd(), "/mixcr-2.1.7/mixcr.jar ",
      "align -OvParameters.geneFeatureToAlign=VTranscript --report alignmentReport.log  ",
      "70-SR13-Gamma_S70_L001_R1_001.fastq.gz 70-SR13-Gamma_S70_L001_R2_001.fastq.gz ",
      "\"70-SR13-Gamma_S70_L001_R1R2_001.vdjca \""
    ))
  )
})

test_that("run_command_vdj gives correct command to console with assemble.", {
  expect_equal(
    run_command_vdj(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_vdjtools = "",
      path_mixcr = "./mixcr-2.1.7/mixcr.jar",
      input_dir = "./testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
      input_filename = "70-SR13-Gamma_S70_L001_R1R2_001",
      input_ext = ".vdjca",
      check_input_filename = TRUE,
      output_filename = "70-SR13-G",
      output_ext = ".clns",
      command_vdj = "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10",
      report_name = "assembleReport",
      replace = FALSE,
      string_only = TRUE
    ),
    noquote(paste0(
      "java -Xmx4g -Xms3g -jar  ", getwd(), "/mixcr-2.1.7/mixcr.jar ",
      "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10 ",
      "--report assembleReport.log  ",
      "\"", getwd(), "/testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/70-SR13-Gamma_S70_L001_R1R2_001.vdjca\" ",
      "\"", getwd(), "/testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/70-SR13-G.clns \""
    ))
  )
})

test_that("run_command_vdj gives correct command to console with exportClones.", {
  expect_equal(
    run_command_vdj(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_vdjtools = "",
      path_mixcr = "./mixcr-2.1.7/mixcr.jar",
      input_dir = "./testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
      input_filename = "70-SR13-G",
      input_ext = ".clns",
      check_input_filename = TRUE,
      output_filename = "70-SR13-G_clonesMin10",
      output_ext = ".txt",
      command_vdj = "exportClones --chains TRG",
      report_name = "",
      replace = FALSE,
      string_only = TRUE
    ),
    noquote(paste0(
      "java -Xmx4g -Xms3g -jar  ", getwd(), "/mixcr-2.1.7/mixcr.jar ",
      "exportClones --chains TRG   ",
      "\"", getwd(), "/testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/70-SR13-G.clns\" ",
      "\"", getwd(), "/testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/70-SR13-G_clonesMin10.txt \""
    ))
  )
})


# run_command_vdj: check arguments ----
test_that("run_command_vdj gives correct command to console when input_filename is `auto`", {
  expect_equal(
    run_command_vdj(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_vdjtools = "",
      path_mixcr = "./mixcr-2.1.7/mixcr.jar",
      input_dir = "./testdata/Gamma_SR11-20/",
      input_filename = "auto",
      input_ext = ".vdjca",
      check_input_filename = TRUE,
      output_filename = "auto",
      output_ext = ".clns",
      command_vdj = "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10",
      report_name = "assembleReport",
      replace = FALSE,
      string_only = TRUE
    ),
    noquote(c(
      paste0(
        "java -Xmx4g -Xms3g -jar  ", getwd(), "/mixcr-2.1.7/mixcr.jar ",
        "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10 ",
        "--report assembleReport.log  ",
        "\"", getwd(), "/testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/70-SR13-Gamma_S70_L001_R1R2_001.vdjca\" ",
        "\"", getwd(), "/testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/70-SR13-G.clns \""
      ),
      paste0(
        "java -Xmx4g -Xms3g -jar  ", getwd(), "/mixcr-2.1.7/mixcr.jar ",
        "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10 ",
        "--report assembleReport.log  ",
        "\"", getwd(), "/testdata/Gamma_SR11-20/71-SR14-Gamma_S71-516541r2vrf541v/71-SR14-Gamma_S71_L001_R1R2_001.vdjca\" ",
        "\"", getwd(), "/testdata/Gamma_SR11-20/71-SR14-Gamma_S71-516541r2vrf541v/71-SR14-G.clns \""
      )
    ))
  )
})

test_that("run_command_vdj gives correct command to console when input_filename is a vector of length > 1.", {
  expect_equal(
    run_command_vdj(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_vdjtools = "",
      path_mixcr = "./mixcr-2.1.7/mixcr.jar",
      input_dir = c(
        "./testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
        "./testdata/Gamma_SR11-20/71-SR14-Gamma_S71-516541r2vrf541v/"
      ),
      input_filename = c("70-SR13-Gamma_S70_L001_R1R2_001", "71-SR14-Gamma_S71_L001_R1R2_001"),
      input_ext = ".vdjca",
      check_input_filename = TRUE,
      output_filename = "auto",
      output_ext = ".clns",
      command_vdj = "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10",
      report_name = "assembleReport",
      replace = FALSE,
      string_only = TRUE
    ),
    noquote(c(
      paste0(
        "java -Xmx4g -Xms3g -jar  ", getwd(), "/mixcr-2.1.7/mixcr.jar ",
        "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10 ",
        "--report assembleReport.log  ",
        "\"", getwd(), "/testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/70-SR13-Gamma_S70_L001_R1R2_001.vdjca\" ",
        "\"", getwd(), "/testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/70-SR13-G.clns \""
      ),
      paste0(
        "java -Xmx4g -Xms3g -jar  ", getwd(), "/mixcr-2.1.7/mixcr.jar ",
        "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10 ",
        "--report assembleReport.log  ",
        "\"", getwd(), "/testdata/Gamma_SR11-20/71-SR14-Gamma_S71-516541r2vrf541v/71-SR14-Gamma_S71_L001_R1R2_001.vdjca\" ",
        "\"", getwd(), "/testdata/Gamma_SR11-20/71-SR14-Gamma_S71-516541r2vrf541v/71-SR14-G.clns \""
      )
    ))
  )
})

test_that("run_command_vdj works when output_dir is different from input_dir.", {
  expect_equal(
    run_command_vdj(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_vdjtools = "",
      path_mixcr = "./mixcr-2.1.7/mixcr.jar",
      input_dir = "./testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
      input_filename = "70-SR13-Gamma_S70_L001_R1R2_001",
      input_ext = ".vdjca",
      check_input_filename = TRUE,
      output_dir = "./testdata/Gamma_SR11-20/",
      output_filename = "70-SR13-G",
      output_ext = ".clns",
      command_vdj = "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10",
      report_name = "assembleReport",
      replace = TRUE,
      string_only = TRUE
    ),
    noquote(paste0(
      "java -Xmx4g -Xms3g -jar  ", getwd(), "/mixcr-2.1.7/mixcr.jar ",
      "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10 ",
      "--report assembleReport.log -f ",
      "\"", getwd(), "/testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/70-SR13-Gamma_S70_L001_R1R2_001.vdjca\" ",
      "\"", getwd(), "/testdata/Gamma_SR11-20/70-SR13-G.clns \""
    ))
  )
})

test_that("run_command_vdj gives correct command to console with replace=TRUE.", {
  expect_equal(
    run_command_vdj(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_vdjtools = "",
      path_mixcr = "./mixcr-2.1.7/mixcr.jar",
      input_dir = "./testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
      input_filename = "70-SR13-Gamma_S70_L001_R1R2_001",
      input_ext = ".vdjca",
      check_input_filename = TRUE,
      output_filename = "70-SR13-G",
      output_ext = ".clns",
      command_vdj = "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10",
      report_name = "assembleReport",
      replace = TRUE,
      string_only = TRUE
    ),
    noquote(paste0(
      "java -Xmx4g -Xms3g -jar  ", getwd(), "/mixcr-2.1.7/mixcr.jar ",
      "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10 ",
      "--report assembleReport.log -f ",
      "\"", getwd(), "/testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/70-SR13-Gamma_S70_L001_R1R2_001.vdjca\" ",
      "\"", getwd(), "/testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/70-SR13-G.clns \""
    ))
  )
})

test_that("run_command_vdj gives correct command to console with specific `report_name`.", {
  expect_equal(
    run_command_vdj(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_vdjtools = "",
      path_mixcr = "./mixcr-2.1.7/mixcr.jar",
      input_dir = "./testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
      input_filename = "70-SR13-Gamma_S70_L001_R1R2_001",
      input_ext = ".vdjca",
      check_input_filename = TRUE,
      output_filename = "70-SR13-G",
      output_ext = ".clns",
      command_vdj = "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10",
      report_name = "testReport",
      replace = FALSE,
      string_only = TRUE
    ),
    noquote(paste0(
      "java -Xmx4g -Xms3g -jar  ", getwd(), "/mixcr-2.1.7/mixcr.jar ",
      "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10 ",
      "--report testReport.log  ",
      "\"", getwd(), "/testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/70-SR13-Gamma_S70_L001_R1R2_001.vdjca\" ",
      "\"", getwd(), "/testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/70-SR13-G.clns \""
    ))
  )
  # same but with the extension in the argument `report_name`
  expect_equal(
    run_command_vdj(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_vdjtools = "",
      path_mixcr = "./mixcr-2.1.7/mixcr.jar",
      input_dir = "./testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
      input_filename = "70-SR13-Gamma_S70_L001_R1R2_001",
      input_ext = ".vdjca",
      check_input_filename = TRUE,
      output_filename = "70-SR13-G",
      output_ext = ".clns",
      command_vdj = "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10",
      report_name = "testReport.log",
      replace = FALSE,
      string_only = TRUE
    ),
    noquote(paste0(
      "java -Xmx4g -Xms3g -jar  ", getwd(), "/mixcr-2.1.7/mixcr.jar ",
      "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10 ",
      "--report testReport.log  ",
      "\"", getwd(), "/testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/70-SR13-Gamma_S70_L001_R1R2_001.vdjca\" ",
      "\"", getwd(), "/testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/70-SR13-G.clns \""
    ))
  )
})

test_that("align_fastq_to_vdjca gives correct command to console with no report.log.", {
  expect_equal(
    run_command_vdj(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_vdjtools = "",
      path_mixcr = "./mixcr-2.1.7/mixcr.jar",
      input_dir = "./testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
      input_filename = "70-SR13-Gamma_S70_L001_R1R2_001",
      input_ext = ".vdjca",
      check_input_filename = TRUE,
      output_filename = "70-SR13-G",
      output_ext = ".clns",
      command_vdj = "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10",
      report_name = "",
      replace = FALSE,
      string_only = TRUE
    ),
    noquote(paste0(
      "java -Xmx4g -Xms3g -jar  ", getwd(), "/mixcr-2.1.7/mixcr.jar ",
      "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10 ",
      "  \"", getwd(), "/testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/70-SR13-Gamma_S70_L001_R1R2_001.vdjca\" ",
      "\"", getwd(), "/testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/70-SR13-G.clns \""
    ))
  )
})

test_that("run_command_vdj gives an error if `input_dir` doesn't exist.", {
  expect_error(
    run_command_vdj(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_vdjtools = "",
      path_mixcr = "./mixcr-2.1.7/mixcr.jar",
      input_dir = "D:/doenst_exist/",
      input_filename = "doenst_exist",
      input_ext = ".vdjca",
      check_input_filename = TRUE,
      output_filename = "70-SR13-G",
      output_ext = ".clns",
      command_vdj = "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10",
      report_name = "alignmentReport",
      replace = FALSE,
      string_only = TRUE
    ),
    "`input_dir` must be a valid directory."
  )
})

test_that("run_command_vdj gives an error when length(`input_dir`) !=  length(`input_file`)", {
  expect_error(
    run_command_vdj(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_vdjtools = "",
      path_mixcr = "./mixcr-2.1.7/mixcr.jar",
      input_dir = "./testdata/Gamma_SR11-20/",
      input_filename = c("70-SR13-Gamma_S70_L001_R1R2_001", "71-SR14-Gamma_S71_L001_R1R2_001"),
      input_ext = ".vdjca",
      check_input_filename = TRUE,
      output_filename = "auto",
      output_ext = ".clns",
      command_vdj = "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10",
      report_name = "assembleReport",
      replace = FALSE,
      string_only = TRUE
    ),
    paste0(
      "`input_dir` must be a character vector of length 1 or equal to length of input_filename.", "\n",
      "Length of `input_dir`: 1", "\n",
      "Length of `input_filename`: 2"
    )
  )
})

test_that("run_command_vdj gives an error if `input_filename` doesn't exist in `input_dir`.", {
  expect_error(
    run_command_vdj(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_vdjtools = "",
      path_mixcr = "./mixcr-2.1.7/mixcr.jar",
      input_dir = "./testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
      input_filename = "doesnt_exist",
      input_ext = ".vdjca",
      check_input_filename = TRUE,
      output_filename = "70-SR13-G",
      output_ext = ".clns",
      command_vdj = "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10",
      report_name = "alignmentReport",
      replace = FALSE,
      string_only = TRUE
    )
  )
})

test_that("run_command_vdj gives an error when the command has status 1 (=error)", {
  skip("Skip because the error messages from console go into testthat.")
  # the command doens't exist.
  expect_error(run_command_vdj(
    java_param = "java -Xmx4g -Xms3g -jar",
    path_vdjtools = "",
    path_mixcr = "./mixcr-2.1.7/mixcr.jar",
    input_dir = "./testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
    input_filename = "70-SR13-Gamma_S70_L001_R1R2_001",
    input_ext = ".vdjca",
    check_input_filename = TRUE,
    output_filename = "70-SR13-G",
    output_ext = ".clns",
    command_vdj = "command that doesn't exist",
    report_name = "alignmentReport",
    replace = TRUE,
    string_only = FALSE
  ))

  # an argument is missing.
  expect_error(run_command_vdj(
    java_param = "java -Xmx4g -Xms3g -jar",
    path_vdjtools = "",
    path_mixcr = "./mixcr-2.1.7/mixcr.jar",
    input_dir = "./testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
    input_filename = "70-SR13-Gamma_S70_L001_R1R2_001",
    input_ext = ".vdjca",
    check_input_filename = TRUE,
    output_filename = "",
    output_ext = "",
    command_vdj = "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10",
    report_name = "alignmentReport",
    replace = TRUE,
    string_only = FALSE
  ))

  # an argument is incorrect
  expect_error(run_command_vdj(
    java_param = "java -Xmx4g -Xms3g -jar",
    path_vdjtools = "",
    path_mixcr = "./mixcr-2.1.7/mixcr.jar",
    input_dir = "./testdata/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
    input_filename = "70-SR13-Gamma_S70_L001_R1_001",
    input_ext = ".fastq.gz", # incorrect extension
    check_input_filename = TRUE,
    output_filename = "70-SR13-G",
    output_ext = ".clns",
    command_vdj = "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10",
    report_name = "alignmentReport",
    replace = TRUE,
    string_only = FALSE
  ))
})

# setwd(inital_wd)
