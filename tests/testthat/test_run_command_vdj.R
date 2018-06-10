
context("check the run_command_vdj function")

# run_command_vdj with basic commands ----
test_that("run_command_vdj gives correct command to console with align.", {
  expect_equal(
    run_command_vdj(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_vdjtools = "",
      path_mixcr = "D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar",
      input_dir = "D:/Projets R/IMIabgd/data/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
      input_filename = "70-SR13-Gamma_S70_L001_R1_001.fastq.gz 70-SR13-Gamma_S70_L001_R2_001.fastq.gz",
      input_ext = ".fastq.gz",
      check_input_filename = FALSE,
      output_filename = "70-SR13-Gamma_S70_L001_R1R2_001",
      output_ext = ".vdjca",
      command_vdj = "align -OvParameters.geneFeatureToAlign=VTranscript",
      report_name = "alignmentReport",
      replace = FALSE,
      string_only = TRUE
    ),
    paste0(
      "java -Xmx4g -Xms3g -jar  D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar ",
      "align -OvParameters.geneFeatureToAlign=VTranscript --report alignmentReport.log  ",
      "70-SR13-Gamma_S70_L001_R1_001.fastq.gz 70-SR13-Gamma_S70_L001_R2_001.fastq.gz ",
      "70-SR13-Gamma_S70_L001_R1R2_001.vdjca"
    )
  )
})

test_that("run_command_vdj gives correct command to console with assemble.", {
  expect_equal(
    run_command_vdj(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_vdjtools = "",
      path_mixcr = "D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar",
      input_dir = "D:/Projets R/IMIabgd/data/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
      input_filename = "70-SR13-Gamma_S70_L001_R1R2_001",
      input_ext = ".vdjca",
      check_input_filename = TRUE,
      output_filename = "70-SR13-G",
      output_ext = ".clns",
      command_vdj = "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10",
      report_name = "alignmentReport",
      replace = FALSE,
      string_only = TRUE
    ),
    paste0(
      "java -Xmx4g -Xms3g -jar  D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar ",
      "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10 ",
      "--report alignmentReport.log  70-SR13-Gamma_S70_L001_R1R2_001.vdjca ",
      "70-SR13-G.clns"
    )
  )
})

# run_command_vdj: check arguments ----
test_that("run_command_vdj gives correct command to console with replace=TRUE.", {
  expect_equal(
    run_command_vdj(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_vdjtools = "",
      path_mixcr = "D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar",
      input_dir = "D:/Projets R/IMIabgd/data/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
      input_filename = "70-SR13-Gamma_S70_L001_R1R2_001",
      input_ext = ".vdjca",
      check_input_filename = TRUE,
      output_filename = "70-SR13-G",
      output_ext = ".clns",
      command_vdj = "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10",
      report_name = "alignmentReport",
      replace = TRUE,
      string_only = TRUE
    ),
    paste0(
      "java -Xmx4g -Xms3g -jar  D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar ",
      "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10 ",
      "--report alignmentReport.log -f 70-SR13-Gamma_S70_L001_R1R2_001.vdjca ",
      "70-SR13-G.clns"
    )
  )
})

test_that("run_command_vdj gives correct command to console with specific `report_name`.", {
  expect_equal(
    run_command_vdj(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_vdjtools = "",
      path_mixcr = "D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar",
      input_dir = "D:/Projets R/IMIabgd/data/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
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
    paste0(
      "java -Xmx4g -Xms3g -jar  D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar ",
      "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10 ",
      "--report testReport.log  70-SR13-Gamma_S70_L001_R1R2_001.vdjca ",
      "70-SR13-G.clns"
    )
  )
  #same but with the extension
  expect_equal(
    run_command_vdj(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_vdjtools = "",
      path_mixcr = "D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar",
      input_dir = "D:/Projets R/IMIabgd/data/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
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
    paste0(
      "java -Xmx4g -Xms3g -jar  D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar ",
      "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10 ",
      "--report testReport.log  70-SR13-Gamma_S70_L001_R1R2_001.vdjca ",
      "70-SR13-G.clns"
    )
  )
})

test_that("align_fastq_to_vdjca gives correct command to console with no report.log.", {
  expect_equal(
    run_command_vdj(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_vdjtools = "",
      path_mixcr = "D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar",
      input_dir = "D:/Projets R/IMIabgd/data/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
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
    paste0(
      "java -Xmx4g -Xms3g -jar  D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar ",
      "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10 ",
      "  70-SR13-Gamma_S70_L001_R1R2_001.vdjca ",
      "70-SR13-G.clns"
    )
  )
})

test_that("run_command_vdj gives an error if `input_dir` doesn't exist.", {
  expect_error(
    run_command_vdj(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_vdjtools = "",
      path_mixcr = "D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar",
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

test_that("run_command_vdj gives an error if `input_filename` doesn't exist in `input_dir`.", {
  expect_error(
    run_command_vdj(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_vdjtools = "",
      path_mixcr = "D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar",
      input_dir = "D:/Projets R/IMIabgd/data/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
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
  # the command doens't exist.
  expect_error(run_command_vdj(
    java_param = "java -Xmx4g -Xms3g -jar",
    path_vdjtools = "",
    path_mixcr = "D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar",
    input_dir = "D:/Projets R/IMIabgd/data/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
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
    path_mixcr = "D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar",
    input_dir = "D:/Projets R/IMIabgd/data/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
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
    path_mixcr = "D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar",
    input_dir = "D:/Projets R/IMIabgd/data/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
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
