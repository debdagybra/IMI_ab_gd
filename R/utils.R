
#' @title correct_paths
#' @description change '\' into '/' and add '/' at the end if missing.
#' @param x a character string
#' @return the string adapted
#' @author DEBOT Damien <damien.debot@@gmail.com>
#' @export
correct_paths <- function(x) {
  if (!is.character(x)) stop("`x` must be a character vector.")

  x <- gsub("([/\\])", "/", x)
  # add / at the end only if it's an existing directory
  x <- ifelse(dir.exists(x) & substr(x, nchar(x), nchar(x)) != "/",
    yes = paste0(x, "/"),
    no = x
  )
  x
}


#' @title check_path_program
#' @description check if the path to a program (.jar) is valid
#' @param path the emplacement of the program on the disk (directory or fullpath)
#' @param program the name of the program. Can be "mixcr" or "vdjtools"
#' @return the path to the program
#' @author DEBOT Damien <damien.debot@@gmail.com>
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_detect
#' @export
check_path_program <- function(path, program) {
  # checks args
  if (!is.character(path)) stop("`path` must be a character vector of length 1.")
  if (length(path) > 1) stop("`path` must be a character vector of length 1.")
  path <- correct_paths(path)

  if (!is.character(program)) stop("`program` must be a character vector of length 1.")
  if (length(program) > 1) stop("`program` must be a character vector of length 1.")
  program <- match.arg(program, c("mixcr", "vdjtools"))

  # run if path is a valid directory
  # find automatically the fullpath
  if (dir.exists(path)) {
    listfiles_program <- list.files(path = path) %>%
      .[grep(program, .)] %>%
      .[toupper(substr(., nchar(.) - 3, nchar(.))) == ".JAR"]

    if (length(listfiles_program) == 0) {
      stop(
        "File ", program, ".jar doens't exist in ", path, "\n",
        "Please enter a valid path for ", program
      )
    }
    if (length(listfiles_program) > 1) {
      warning(
        "Several valid files are found in ", path, "\n",
        paste(listfiles_program, collapse = "\n"), "\n",
        "The first one will be used."
      )
    }
    path_program <- paste0(path, listfiles_program[1])
    return(path_program)
  }

  # run if path is a file (valid or not)
  # check if the path is valid or throw an error
  usr_dirname_program <- dirname(path)
  listfiles_program <- list.files(path = usr_dirname_program) %>%
    .[grep(program, .)] %>%
    .[toupper(substr(., nchar(.) - 3, nchar(.))) == ".JAR"]

  usr_filename_program <- substr(path, nchar(usr_dirname_program) + 2, nchar(path))

  if (length(listfiles_program) == 0) {
    stop(
      "File ", path, " is not valid.", "\n",
      "Please enter a valid directory or fullpath for `", program, ".jar`"
    )
  }

  # path is valid but not the correct file
  if (!any(toupper(usr_filename_program) %in% toupper(listfiles_program))) {
    stop(
      "File ", path, " is not valid.", "\n",
      "Did you mean: `", correct_paths(usr_dirname_program), listfiles_program[1], "`."
    )
  }
  path
}


#' @title convert_decimal
#' @description change decimalmark to `,` in .txt files.
#' @param path the path of a .txt file
#' @param decimal the decilmark you want to use (default= ,)
#' @return same .txt files with a new decimal separator
#' @author DEBOT Damien <damien.debot@@gmail.com>
#' @importFrom readr read_delim
#' @export
convert_decimal <- function(path, decimal=",") {
  path <- "D:/Maria/Treemaps et barplot/test/"

  # checks args
  if (!is.character(path)) stop("`path` must be a character vector of length 1.")
  if (length(path) > 1) stop("`path` must be a character vector of length 1.")
  path <- correct_paths(path)

  decimal <- match.arg(decimal, c(",", "."))

  # run if path is a valid directory
  if (dir.exists(path)) {
    list_fichiers_fullpath <- list.files(path = path, pattern = ".txt", full.names = TRUE)
    list_fichiers <- list.files(path = path, pattern = ".txt", full.names = FALSE)
    for (i in 1:length(list_fichiers)) {
      # load data and save in the same .txt
      message(paste0(
        "convert decimal format to `", decimal, "`: ",
        list_fichiers[i], "(", i, "/", length(list_fichiers), ")."
      ))
      data <- suppressMessages(readr::read_delim(list_fichiers_fullpath[i], delim = "\t"))
      write.table(data,
        file = list_fichiers_fullpath[i], sep = "\t",
        dec = decimal, row.names = FALSE
      )
    }
  } else { # if path is a file
    if (file.exists(path) & substr(path, nchar(path) - 3, nchar(path)) == ".txt") {
      # load data and save in the same .txt
      data <- suppressMessages(readr::read_delim(path, delim = "\t"))
      write.table(data, file = path, sep = "\t", dec = decimal, row.names = FALSE)
    } else {
      warning(path, " is not a valid file or directory.")
    }
  }
  invisible()
}


#' @title txt_to_excel
#' @description convert .txt file to .xlsx file and change decimalmark to `,`.
#' @param path a directory containing .txt files
#' @param subfolder a sub-directory of path where you want to save the files
#' @return .xlsx files
#' @author DEBOT Damien <damien.debot@@gmail.com>
#' @import xlsx
#' @importFrom readr read_delim
#' @export
txt_to_excel <- function(path, subfolder=NULL) {
  # checks args
  if (!is.character(path)) stop("`path` must be a character vector of length 1.")
  if (length(path) > 1) stop("`path` must be a character vector of length 1.")
  path <- correct_paths(path)

  if (is.null(subfolder)) {
    output_path <- path
  } else {
    if (!is.character(subfolder)) stop("`subfolder` must be a character vector of length 1.")
    if (length(subfolder) > 1) stop("`subfolder` must be a character vector of length 1.")
    # create subfolder if necessary and check if exists
    suppressWarnings(dir.create(path = paste0(path, subfolder)))
    output_path <- correct_paths(paste0(path, subfolder))
    if (!dir.exists(output_path)) {
      stop("`output_path` must be a valid directory.")
    }
  }

  if (dir.exists(path)) {
    list_fichiers_fullpath <- list.files(path = path, pattern = ".txt", full.names = TRUE)
    list_fichiers <- list.files(path = path, pattern = ".txt", full.names = FALSE)
    for (i in 1:length(list_fichiers)) {
      message(paste0(
        "convert file (", i, "/", length(list_fichiers), ") : ",
        list_fichiers[i], " to .xlsx"
      ))
      # load data
      data <- suppressMessages(readr::read_delim(list_fichiers_fullpath[i], delim = "\t"))
      # save as xlsx
      wb <- createWorkbook(type = "xlsx")
      sheet <- createSheet(wb, sheetName = "Sheet1")
      addDataFrame(data, sheet, startRow = 1, startColumn = 1)
      saveWorkbook(wb, gsub(".txt", ".xlsx", paste0(output_path, list_fichiers[i])))
    }
  } else {
    if (file.exists(path) & substr(path, nchar(path) - 3, nchar(path)) == ".txt") {
      # load data
      data <- suppressMessages(readr::read_delim(path, delim = "\t"))
      # save as xlsx
      wb <- createWorkbook(type = "xlsx")
      sheet <- createSheet(wb, sheetName = "Sheet1")
      addDataFrame(data, sheet, startRow = 1, startColumn = 1)
      saveWorkbook(wb, gsub(".txt", ".xlsx", paste0(output_path, list_fichiers[i])))
    } else {
      warning(path, " is not a valid file or directory.")
    }
  }
  invisible()
}
