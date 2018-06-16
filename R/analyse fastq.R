
#' @title analyse_tcells
#' @description execute a list of commmand on all .fastq.gz files in `input_dir`
#' @param java_param a character string containing java parameters
#' @param path_vdjtools the path to vdjtools.jar
#' @param path_mixcr the path to mixcr.jar
#' @param input_dir directory where the files are (also look in sub-directories)
#' @param tcell_type The type of Tcells to analyse (A, B, G or D).
#'                  "auto" to let R detect the type of Tcells automatically.
#' @param replace if TRUE, if the output file already exists, it will be replaced.
#' @param string_only if TRUE, don't execute the command but returns it in a string
#' @param ... Parameters to pass to the called functions (e.g. report_name)
#' @return runs command lines in console. Creates .vdjca files
#'         in the same directory than the fastq.gz files
#' @author DEBOT Damien <damien.debot@@gmail.com>
#' @importFrom magrittr %>%
#' @export
#'
#'

# TEST :
# test <- analyse_tcells(java_param="java -Xmx4g -Xms3g -jar",
#                path_vdjtools="D:/Maria/Maria_Analysis/vdjtools-1.1.7/vdjtools-1.1.7.jar",
#                path_mixcr="D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar",
#                input_dir="D:/Projets R/IMIabgd/data/Gamma_SR11-20/",
#                replace=FALSE,
#                string_only=FALSE)



analyse_tcells <- function(java_param="java -Xmx4g -Xms3g -jar",
                           path_vdjtools,
                           path_mixcr,
                           input_dir,
                           tcell_type="auto",
                           replace=FALSE,
                           string_only=FALSE,
                           ...) {
  starttime <- proc.time()
  # checks args
  # java_param
  if (!is.character(java_param)) {
    stop(
      "`java_param` must be a character vector of length 1.",
      "e.g. `java -Xmx4g -Xms3g -jar`"
    )
  }
  if (length(java_param) > 1) {
    stop(
      "`java_param` must be a character vector of length 1.",
      "e.g. `java -Xmx4g -Xms3g -jar`"
    )
  }
  # path_vdjtools
  path_vdjtools <- check_path_program(path_vdjtools, program = "vdjtools")
  # path_mixcr
  path_mixcr <- check_path_program(path_mixcr, program = "mixcr")
  # input_dir
  if (!is.character(input_dir)) stop("`input_dir` must be a character vector of length 1.")
  if (length(input_dir) > 1) stop("`input_dir` must be a character vector of length 1.")
  input_dir <- correct_paths(input_dir)
  if (!dir.exists(input_dir)) {
    stop("`input_dir` must be a valid directory.")
  }
  # tcell_type
  tcell_type <- match.arg(tcell_type, c("auto", "A", "B", "G", "D"))
  # replace
  if (!is.logical(replace)) stop("`replace` must be logical.")
  # string_only
  if (!is.logical(string_only)) stop("`string_only` must be logical.")


  if (string_only == TRUE) message("string_only is ON. Commands will not be executed.")

  # lists all files '.fatsq.gz' in directory 'input_dir' and all the sub-directories
  files_fastq.gz <- list.files(
    path = input_dir, recursive = TRUE,
    full.names = TRUE, pattern = ".fastq.gz"
  ) %>%
    .[substr(., nchar(.) - 8, nchar(.)) == ".fastq.gz"] %>%
    .[grep("_R1_", .)] %>%
    gsub("//", "/", .) %>%
    unique()

  # execute all commands for each file
  command_txt <- character(0)
  for (f in 1:length(files_fastq.gz)) {
    # erase file extension
    input_filename <- basename(files_fastq.gz[f]) %>%
      gsub(".fastq.gz", "", .)
    # determine the folder of this specific file
    input_dir_file <- paste0(dirname(files_fastq.gz[f]), "/")
    input_parentdir_file <- paste0(dirname(input_dir_file),"/")

    message(paste0(
      "Analyse file (", f, "/", length(files_fastq.gz), ") : ",
      paste0(input_filename, ".fastq.gz")
    ))


    # guests the Tcell type from filename
    short_input_filename <- substr(
      input_filename, 1,
      gregexpr("-", input_filename)[[1]][2] + 1
    )
    if (tcell_type == "auto") {
      tcell_type <- toupper(substr(
        short_input_filename,
        nchar(short_input_filename),
        nchar(short_input_filename)
      ))
      if (!tcell_type %in% c("A", "B", "G", "D")) {
        stop(
          "Unable to autodetect Tcell type for file ",
          input_filename, "."
        )
      }
    }

    # the functions return the name of the output file
    # (or the command in string when string_only=TRUE)
    # command_txt is used to store all the commands in a vector


    # execute the command `align -OvParameters.geneFeatureToAlign=VTranscript`
    lastreturned_value <- align_fastq_to_vdjca(
      java_param = java_param, path_mixcr = path_mixcr,
      input_dir = input_dir_file, input_filename = input_filename,
      # report_name="alignmentReport",
      replace = replace, string_only = string_only, ...
    )
    command_txt <- c(command_txt, lastreturned_value)


    # execute the command `assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10`
    lastreturned_value <- assemble_vdjca_to_clns(
      java_param = java_param, path_mixcr = path_mixcr,
      input_dir = input_dir_file, input_filename = basename(lastreturned_value),
      output_filename = short_input_filename,
      replace = replace, string_only = string_only, ...
    )
    command_txt <- c(command_txt, lastreturned_value)


    # execute the command `exportClones TR` & Tcell_type
    lastreturned_value <- export_clones(
      java_param = java_param, path_mixcr = path_mixcr,
      input_dir = input_dir_file, input_filename = basename(lastreturned_value),
      #output_dir = input_parentdir_file,
      output_filename = paste0(short_input_filename,"_clonesMin10)"),
      tcell_type = tcell_type,
      replace = replace, string_only = string_only, ...
    )
    command_txt <- c(command_txt, lastreturned_value)


    command_txt <- c(command_txt, "")
  }

  # endtime
  message(paste0("Done in ", round((proc.time() - starttime)[[3]], 2), " seconds"))

  if (string_only == TRUE) return(command_txt)
  invisible()
}

#' @title align_fastq_to_vdjca
#' @description execute the command : 'align -OvParameters.geneFeatureToAlign=VTranscript
#             --report alignmentReport.log input_R1.fastq input_R2.fastq alignments.vdjca'
#' @param java_param a character string containing java parameters
#' @param path_mixcr the path to mixcr.jar
#' @param input_dir where the files are
#' @param input_filename file name (type R1.fastq.gz), (with or without the extension)
#' @param report_name the name of the file alignmentReport. (without the extension)
#'                    Set to NULL to not create the file.
#' @param replace if TRUE, if the output file already exists, it will be replaced.
#' @param string_only if TRUE, don't execute the command but returns it in a string
#' @return run command line in console. Creates .vdjca files
#'         in the same directory than the fastq.gz files
#'         Returns the name of the outputfile if string_only=FALSE
#'         Returns command if string_only=TRUE
#' @author DEBOT Damien <damien.debot@@gmail.com>
#' @importFrom magrittr %>%
#' @export
align_fastq_to_vdjca <- function(java_param="java -Xmx4g -Xms3g -jar",
                                 path_mixcr,
                                 input_dir,
                                 input_filename,
                                 output_filename="auto",
                                 report_name="alignmentReport",
                                 replace=FALSE,
                                 string_only=FALSE) {


  # TEST : java_param <- "java -Xmx4g -Xms3g -jar" ; path_mixcr <- "D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar"
  # input_dir <- 'D:/Projets R/IMIabgd/data/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/'
  # input_filename <- "70-SR13-Gamma_S70_L001_R1_001"
  # report_name <- "alignmentReport" ; replace <- FALSE ; string_only <- TRUE

  # check args
  {
    # input_dir
    if (!is.character(input_dir)) stop("`input_dir` must be a character vector of length 1.")
    if (length(input_dir) > 1) stop("`input_dir` must be a character vector of length 1.")
    input_dir <- correct_paths(input_dir)
    if (!dir.exists(input_dir)) {
      stop("`input_dir` must be a valid directory.")
    }
    # input_filename
    if (!is.character(input_filename)) stop("`input_filename` must be a character vector of length 1.")
    if (length(input_filename) > 1) stop("`input_filename` must be a character vector of length 1.")
    input_filename <- correct_paths(input_filename)
    input_filename <- gsub(".fastq.gz", "", input_filename) # erase file extension
    if (!file.exists(paste0(input_dir, input_filename, ".fastq.gz"))) {
      stop("`input_filename` must be a valid file in `input_dir` directory.")
    }
  }
  # end check args
  input_ext <- ".fastq.gz"
  output_ext <- ".vdjca"

  # From input_filename
  #   check if there are 2 files R1,R2
  #   define the name of the output file
  input_1 <- paste0(gsub("_R2_", "_R1_", input_filename), input_ext)
  if (!file.exists(paste0(input_dir, input_1))) {
    stop(
      input_dir, " doesn't contains an R1 file. \n",
      "Expected file: ", input_1
    )
  }
  input_2 <- paste0(gsub("_R1_", "_R2_", input_filename), input_ext)
  if (!file.exists(paste0(input_dir, input_1))) {
    warning(
      input_dir, " doesn't contains an R2 file. \n",
      "The analyse is done only with the R1 file. \n",
      "Expected file: ", input_2
    )
    input_2 <- ""
    if (output_filename == "auto") output_filename <- gsub("_R2_", "_R1_", input_filename)
  } else {
    if (output_filename == "auto") {
      output_filename <- gsub("_R2_", "_R1R2_", input_filename) %>%
        gsub("_R1_", "_R1R2_", .)
    }
  }
  # merge the R1 & R2 input files
  input_filename <- paste(input_1, input_2, collapse = " ")

  run_command_vdj(
    java_param = java_param,
    path_vdjtools = "",
    path_mixcr = path_mixcr,
    input_dir = input_dir,
    input_filename = input_filename,
    input_ext = ".fastq.gz",
    check_input_filename = FALSE,
    output_filename = output_filename,
    output_ext = ".vdjca",
    command_vdj = "align -OvParameters.geneFeatureToAlign=VTranscript",
    report_name = report_name,
    replace = replace,
    fullpath=FALSE,
    string_only = string_only
  )
}




#' @title assemble_vdjca_to_clns
#' @description execute the command : 'assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10
#               --report assembleReport.log alignments.vdjca clones.clns'
#' @param java_param a character string containing java parameters
#' @param path_mixcr the path to mixcr.jar
#' @param input_dir where the files are
#' @param input_filename file name (type .vdjca), (with or without the extension)
#' @param output_filename the name of the output file (without extension)
#' @param report_name the name of the file assembleReport (without the extension)
#'                    Set to NULL to not create the file.
#' @param replace if TRUE, if the output file already exists, it will be replaced.
#' @param string_only if TRUE, don't execute the command but returns it in a string
#' @return run command line in console. Creates .clns files
#'         in the same directory than the fastq.gz files
#'         Returns the name of the outputfile if string_only=FALSE
#'         Returns command if string_only=TRUE
#' @author DEBOT Damien <damien.debot@@gmail.be>
#' @export
assemble_vdjca_to_clns <- function(java_param="java -Xmx4g -Xms3g -jar",
                                   path_mixcr,
                                   input_dir,
                                   input_filename,
                                   output_filename,
                                   report_name="assembleReport",
                                   replace=FALSE,
                                   string_only=FALSE) {

  # TO DO add Tcells_type ----
  # to choose between Min10 or regular
  run_command_vdj(
    java_param = java_param,
    path_vdjtools = "",
    path_mixcr = path_mixcr,
    input_dir = input_dir,
    input_filename = input_filename,
    input_ext = ".vdjca",
    output_filename = output_filename,
    output_ext = ".clns",
    command_vdj = "assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10",
    report_name = report_name,
    replace = replace,
    fullpath=FALSE,
    string_only = string_only
  )
}


#' @title export_clones
#' @description execute the command : 'assemble -OcloneFactoryParameters.dParameters.absoluteMinScore=10
#               --report assembleReport.log alignments.vdjca clones.clns'
#' @param java_param a character string containing java parameters
#' @param path_mixcr the path to mixcr.jar
#' @param input_dir where the files are
#' @param input_filename file name (type .vdjca), (with or without the extension)
#' @param output_filename the name of the output file (without extension)
#' @param report_name the name of the file assembleReport (without the extension)
#'                    Set to NULL to not create the file.
#' @param replace if TRUE, if the output file already exists, it will be replaced.
#' @param string_only if TRUE, don't execute the command but returns it in a string
#' @return run command line in console. Creates .clns files
#'         in the same directory than the fastq.gz files
#'         Returns the name of the outputfile if string_only=FALSE
#'         Returns command if string_only=TRUE
#' @author DEBOT Damien <damien.debot@@gmail.be>
#' @export
export_clones <- function(java_param="java -Xmx4g -Xms3g -jar",
                          path_mixcr,
                          input_dir,
                          input_filename,
                          output_filename,
                          tcell_type="auto",
                          report_name="",
                          replace=FALSE,
                          string_only=FALSE){

  #   #java -Xmx4g -Xms3g -jar path\mixcr.jar exportClones --chains TRG clones.clns TRGclones.txt
  #   #print("Cmd exportClones TRD/G")
  #   if ( !(file.exists(paste0(input_parentdir,substr(input_filename,1,Length_id),"_TR",toupper(substr(input_filename,Length_id,Length_id)),"_clonesMin10.txt")) & replace_if_existing==FALSE) ) {
  #     # system(paste(java.param,path_mixcr, paste0("exportClones --chains TR",toupper(substr(input_filename,Length_id,Length_id))),
  #     #              string_f, paste0(input_parentdir,substr(input_filename,1,Length_id),"_clones.clns"),
  #     #              paste0(input_parentdir,substr(input_filename,1,Length_id),"_TR",toupper(substr(input_filename,Length_id,Length_id)),"_clones.txt")))
  #     system(paste(java.param,path_mixcr,
  # paste0("exportClones --chains TR",toupper(substr(input_filename,Length_id,Length_id))),
  #                  string_f,
#   paste0('"',input_dir,substr(input_filename,1,Length_id),'_clonesMin10.clns"'),
  #
  # paste0('"',input_parentdir,substr(input_filename,1,Length_id),"_TR",toupper(substr(input_filename,Length_id,Length_id)),'_clonesMin10.txt"')))
  #   } else message(paste0("File '",paste0(path,substr(input_filename,1,Length_id),"_TR",toupper(substr(input_filename,Length_id,Length_id)),"_clonesMin10.txt"),"' already exists"))
  #

  # tcell_type
  tcell_type <- match.arg(tcell_type, c("auto", "A", "B", "G", "D"))

  run_command_vdj(
    java_param = java_param,
    path_vdjtools = "",
    path_mixcr = path_mixcr,
    input_dir = input_dir,
    input_filename = input_filename,
    input_ext = ".clns",
    output_filename = output_filename,
    output_ext = ".txt",
    command_vdj = paste0("exportClones --chains TR",tcell_type," "),
    report_name = report_name,
    replace = replace,
    fullpath=FALSE,
    string_only = string_only
  )

}


#' @title run_command_vdj
#' @description execute a command in the windows console.
#' @param java_param a character string containing java parameters
#' @param path_vdjtools the path to vdjtools.jar
#' @param path_mixcr the path to mixcr.jar
#' @param input_dir where the files are
#' @param input_filename file name (without the extension). If empty, loads all files in `input_dir`
#' @param input_ext the extension of the `input_filename` (e.g. `.vdjca`)
#' @param output_dir where the output files will be. If empty, the default value is the same as `input_dir`.
#'                   Warning: It's not always possible to choose the `output_dir` in mixcr/vdjtools.
#'                   Warning: You 'll need probably to set fullpath=TRUE.
#' @param output_filename the name of the output file (without extension)
#' @param output_ext the extension of the `output_filename` (e.g. `.txt`)
#' @param command_vdj the command to run
#' @param report_name the name of the file assembleReport (without the extension)
#'                    Set to NULL to not create the file.
#' @param replace if TRUE, if the output file already exists, it will be replaced.
#' @param fullpath if TRUE, the fullpath of `input_filename` and `output_filename` will be written in command.
#' @param string_only if TRUE, don't execute the command but returns it in a string
#' @details execute a command in the windows console.
#'          The scruture follows the order:
#'          java_param + path_vdjtools + path_mixcr + report_name +
#'          command_vdj + input_filename + output_filename
#'          if an argument is NULL, it will be ignored
#' @return run command line in console. Creates .clns files
#'         in the same directory than the fastq.gz files
#'         Returns the name of the outputfile if string_only=FALSE
#'         Returns command if string_only=TRUE
#' @author DEBOT Damien <damien.debot@@gmail.be>
#' @importFrom magrittr %>%
#' @export
run_command_vdj <- function(java_param="java -Xmx4g -Xms3g -jar",
                            path_vdjtools="",
                            path_mixcr="",
                            input_dir,
                            input_filename="auto",
                            input_ext,
                            check_input_filename=TRUE,
                            output_dir,
                            output_filename="auto",
                            output_ext,
                            command_vdj,
                            report_name="",
                            replace=FALSE,
                            fullpath = FALSE,
                            string_only=FALSE,
                            ...) {


  # TEST : java_param <- "java -Xmx4g -Xms3g -jar" ; path_mixcr <- "D:/Maria/Maria_Analysis/mixcr-2.1.7/mixcr.jar"
  # input_dir <- 'D:/Projets R/IMIabgd/data/Gamma_SR11-20/70-SR13-Gamma_S70-6542654654221/'
  # input_filename <- "70-SR13-Gamma_S70_L001_R1R2_001"
  # report_name <- "assembleReport" ; replace <- FALSE ; string_only <- FALSE


  # checks args ----
    # java_param
    if (!is.character(java_param)) {
      stop(
        "`java_param` must be a character vector of length 1.",
        "e.g. `java -Xmx4g -Xms3g -jar`"
      )
    }
    if (length(java_param) > 1) {
      stop(
        "`java_param` must be a character vector of length 1.",
        "e.g. `java -Xmx4g -Xms3g -jar`"
      )
    }
    # path_vdjtools
    if (path_vdjtools != "") path_vdjtools <- check_path_program(path_vdjtools, program = "vdjtools")
    # path_mixcr
    if (path_mixcr != "") path_mixcr <- check_path_program(path_mixcr, program = "mixcr")
    # input_dir
    if (!is.character(input_dir)) stop("`input_dir` must be a character vector of length 1.")
    if (length(input_filename) > 1 & length(input_dir) != length(input_filename)) {
      stop(
        "`input_dir` must be a character vector of length 1 or equal to length of input_filename.", "\n",
        "Length of `input_dir`: ", length(input_dir), "\n",
        "Length of `input_filename`: ", length(input_filename)
      )
    }
    input_dir <- correct_paths(input_dir)
    if (any(!dir.exists(input_dir))) {
      stop(
        "`input_dir` must be a valid directory.", "\n",
        "Invalid: ", "\n",
        paste(input_dir[!dir.exists(input_dir)], collapse = "\n")
      )
    }
    # check_input_filename
    if (!is.logical(check_input_filename)) stop("`check_input_filename` must be TRUE or FALSE")
    # input_ext
    if (missing(input_ext)) {
      stop("`input_ext` is missing.")
    }
    if (!is.character(input_ext)) {
      stop("`input_ext` must be a character vector of length 1, starting with a dot.")
    }
    if (nchar(input_ext) == 0) {
      stop("`input_ext` must be a non-empty character vector of length 1, starting with a dot.")
    }
    if (substr(input_ext, 1, 1) != ".") {
      stop(
        "`input_ext` must be a character vector of length 1, starting with a dot.", "\n",
        "Did you mean: `.", input_ext, "` ?"
      )
    }
    # input_filename
    if (!is.character(input_filename)) stop("`input_filename` must be a character vector.")
    # if (length(input_filename) > 1) stop("`input_filename` must be a character vector.")
    if (all(input_filename %in% c("", "auto"))) {
      input_filename <- list.files(
        path = input_dir, recursive = TRUE,
        full.names = TRUE, pattern = input_ext
      ) %>%
        .[substr(., nchar(.) - nchar(input_ext) + 1, nchar(.)) == input_ext] %>%
        gsub("//", "/", .) %>%
        unique()
      # input_dir for each file
      input_dir <- paste0(dirname(input_filename), "/")
      input_filename <- basename(input_filename)
    }

    input_filename <- correct_paths(input_filename)
    if (check_input_filename == TRUE) input_filename <- gsub(input_ext, "", input_filename) # erase file extension
    if (any(!file.exists(paste0(input_dir, input_filename, input_ext)) & check_input_filename == TRUE)) {
      stop(
        "`input_filename`+`input_ext` must be a valid file in `input_dir` directory.", "\n",
        "Your file: ", input_filename, input_ext, " doesn't exist in ", input_dir, "."
      )
    }
    # outputfilename
    if (all(output_filename == "auto")) {
      if (any(is.na(gregexpr("-", input_filename)[[1]][2]))) {
        stop(
          "`output_filename` can't be determined automatically because ",
          "at least one of the files doensn't contain `-` in his title.", "\n",
          "Your file(s): ", "\n", paste(paste0(input_filename, input_ext), collapse = "\n")
        )
      } else {
        output_filename <- substr(input_filename, 1, gregexpr("-", input_filename)[[1]][2] + 1)
      }
    }
    if (length(output_filename) == 1 & length(input_filename) > 1) {
      output_filename <- rep(output_filename, length(input_filename))
    }
    if (length(output_filename) != length(input_filename)) {
      stop("`output_ext` must be a character vector with the same length than `input_filename`.")
    }
    # output_ext
    if (!is.character(output_ext)) {
      stop("`output_ext` must be a character vector of length 1, starting with a dot.")
    }
    if (any(nchar(output_ext) == 0 & nchar(output_filename) > 0)) {
      stop("`output_ext` can't be empty if `output_filename` isn't also empty.")
    }
    if (nchar(output_ext) > 0 && substr(output_ext, 1, 1) != ".") {
      stop(
        "`output_ext` must be a character vector of length 1, starting with a dot.", "\n",
        "Did you mean: `.", output_ext, "` ?"
      )
    }
    # output_dir
    if (base::missing(output_dir)) output_dir <- input_dir
    if (!is.character(output_dir)) {
      stop("`output_dir` must be a character vector of length 1 or same length than `output_filename`")
    }
    if (length(output_filename) > 1 & length(output_dir) != length(output_filename)) {
      stop(
        "`output_dir` must be a character vector of length 1 or equal to length of output_filename", "\n",
        "Length of `output_dir`: ", length(output_dir), "\n",
        "Length of `output_filename`: ", length(output_filename)
      )
    }
    output_dir <- correct_paths(output_dir)
    if (any(!dir.exists(output_dir))) {
      stop(
        "`output_dir` must be a valid directory.", "\n",
        "Invalid: ", "\n",
        paste(output_dir[!dir.exists(output_dir)], collapse = "\n")
      )
    }
    # command_vdj
    if (!is.character(command_vdj)) stop("`command_vdj` must be a character vector of length 1.")
    # report_name
    if (!is.character(report_name)) stop("`report_name` must be a character vector of length 1 or NULL.")
    report_name <- gsub(".log", "", report_name) # erase file extension
    # replace
    if (!is.logical(replace)) stop("`replace` must be logical.")
    # string_only
    if (!is.logical(string_only)) stop("`string_only` must be logical.")
    # ...
    if( any(names(list(...)) %in% "Tcell_type")) {
      # tcell_type
      tcell_type <- match.arg(list(...)$tcell_type, c("auto", "A", "B", "G", "D"))
      if (tcell_type == "auto") {
        tcell_type <- toupper(substr(
          input_filename,
          nchar(input_filename),
          nchar(input_filename)
        ))
        if (!tcell_type %in% c("A", "B", "G", "D")) {
          stop(
            "Unable to autodetect Tcell type for file ",
            input_filename, "."
          )
        }
        command_vdj <- gsub("auto",tcell_type,command_vdj)
      }
    }
  # end checks args

  # create command ----
  command_request <- character(0)
  for (i in 1:length(input_filename)) {
    # write the command(s)
    command_request <- c(command_request, paste(
      java_param, # e.g.  'java -Xmx4g -Xms3g -jar'
      path_vdjtools,
      path_mixcr,
      command_vdj,
      if (report_name == "") {
        report_name
      } else {
        paste0("--report ", report_name, ".log") # e.g.  '--report alignmentReport.log'
      },
      if (replace == TRUE) "-f" else "",         # e.g.  '-f'
      paste0(                                    # e.g.  'input_filename.vdjca'
        if (fullpath==TRUE & check_input_filename == TRUE) input_dir[i] else "",
        input_filename[i],
        if (check_input_filename == TRUE) input_ext else ""
      ),
      paste0(if (fullpath==TRUE) output_dir[i] else "",
             output_filename[i], output_ext), # e.g.  'output_file.clns
      sep = " "
    ))
  }
  if (string_only == TRUE) return(command_request)

  # execute the command in the console ----
  # but only if replace = TRUE or the output file doesn't exist yet
  for (i in 1:length(input_filename)) {
    if (length(input_filename) > 1) {
      message(paste0(
        "Processing file (", i, "/", length(input_filename), ") : ",
        paste0(input_filename[i], input_ext)
      ))
    }
    if (!(file.exists(paste0(output_dir[i], output_filename[i], output_ext)) & replace == FALSE)) {
      inital_wd <- getwd()
      setwd(input_dir[i]) # R active directory -> set cd in console
      # execute command
      # status: 0 = success / 1 = error => replaced by the error message
      status <- suppressMessages(suppressWarnings(tryCatch(system(command_request[i], intern = TRUE),
        warning = function(w) list(
            rvalue = system(command_request[i], intern = TRUE),
            warn = conditionMessage(w)
          ),
        error = function(e) list(
            rvalue = system(command_request[i], intern = TRUE),
            err = conditionMessage(e)
          )
      )))
      setwd(inital_wd)
      if (is.list(status) && attributes(status[[1]])$status != 0) {
        stop(
          "Your command line ended with an error.", "\n",
          "The reason might be that:", "\n",
          " - the command does not exist or contains an error ", "\n",
          " - a required argument is missing or incorrect ", "\n",
          " - the `fullpath` argument should be different (now: ",!fullpath,").","\n","\n",
          status[[2]],"\n",
          status[[1]]
        )
      } else {
        cat(paste0(paste(status, collapse="\n"),"\n"))
      }

      # check that the file is created
      if (!file.exists(paste0(output_dir[i], output_filename[i], output_ext))) {
        warning("Output file: `", paste0(output_dir[i], output_filename[i], output_ext), "` not found.")
      }
    } else {
      message(paste0("File '", paste0(output_dir[i], output_filename[i], output_ext), "' already exists and is not replaced."))
    }
  }

  #return names of created files
  paste0(input_dir, output_filename, output_ext)
}
