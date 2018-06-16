
context("check the align_fastq_to_vdjca function")

# align_fastq_to_vdjca ----
test_that("align_fastq_to_vdjca gives correct command to console with R1_R2", {
  expect_equal(
    align_fastq_to_vdjca(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_mixcr = "D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar",
      input_dir = "D:/Projets R/IMIabgd/data/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
      input_filename = "70-SR13-Gamma_S70_L001_R1_001.fastq.gz",
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

test_that("align_fastq_to_vdjca gives correct command to console with R1 only", {
  skip("align_fastq_to_vdjca gives correct command to console with R1 only")
  # to do with files in data_raw
  # expect_equal(
  #   align_fastq_to_vdjca(
  #     java_param = "java -Xmx4g -Xms3g -jar",
  #     path_mixcr = "D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar",
  #     input_dir = "D:/Projets R/IMIabgd/data/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
  #     input_filename = "70-SR13-Gamma_S70_L001_R1_001.fastq.gz",
  #     report_name = "alignmentReport",
  #     replace = FALSE,
  #     string_only = TRUE
  #   ),
  #   paste0(
  #     "java -Xmx4g -Xms3g -jar  D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar ",
  #     "align -OvParameters.geneFeatureToAlign=VTranscript --report alignmentReport.log  ",
  #     "70-SR13-Gamma_S70_L001_R1_001.fastq.gz 70-SR13-Gamma_S70_L001_R2_001.fastq.gz ",
  #     "70-SR13-Gamma_S70_L001_R1R2_001.vdjca"
  #   )
  # )
})

test_that("align_fastq_to_vdjca gives an error if `input_filename` is only an R2 file", {
  skip("align_fastq_to_vdjca gives an error if `input_filename` is only an R2 file")
  # to do with files in data_raw
  # expect_error(
  #   align_fastq_to_vdjca(
  #     java_param = "java -Xmx4g -Xms3g -jar",
  #     path_mixcr = "D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar",
  #     input_dir = "D:/Projets R/IMIabgd/data/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
  #     input_filename = "doenst_exist.fastq.gz",
  #     report_name = "alignmentReport",
  #     replace = FALSE,
  #     string_only = TRUE
  #   ),
  #   "`input_filename` must be a valid file in `input_dir` directory."
  # )
})

test_that("align_fastq_to_vdjca gives an error if `input_filename` doesn't exist in `input_dir`", {
  expect_error(
    align_fastq_to_vdjca(
      java_param = "java -Xmx4g -Xms3g -jar",
      path_mixcr = "D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar",
      input_dir = "D:/Projets R/IMIabgd/data/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/",
      input_filename = "doenst_exist.fastq.gz",
      report_name = "alignmentReport",
      replace = FALSE,
      string_only = TRUE
    ),
    "`input_filename` must be a valid file in `input_dir` directory."
  )
})
