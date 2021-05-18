#' Build the Uncalibrated Dataset for further use in SizeExctractR
#'
#' @param path A Directory path within which holds the text files outputted from SizeExtractR imageJ tools and protocol. The directory path should be given in double quotes in the same format as that returned by the function getwd()
#' @param var.names A vector object of variable names which relate to the directory structure of the directory given by the 'path' argument. This vector should be set using SizeExtractR::CheckSet_DirecVars VEctor length should be equal to the number of subdirectory levels within the directory given by the 'path' paramter.
#'
#' @return Returns a dataframe object of all regions of interest (ROIs) from all text files within the directory given by the 'path' argument. Directory strings are recoded into dataframe variables with title referring to the 'var.names' parameter.
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
#' @examples
#' Database = Build_Uncalibrated_Dataset(mypath, varnames)
#'
Build_Uncalibrated_Dataset = function(path, var.names){
  if(substr(path,nchar(path),nchar(path)) == "/"){
    path = substr(path,1,nchar(path)-1)
  }

  # Load in the data
  txt_files_ls = list.files(path=path, pattern="*.txt", recursive = TRUE)
  txt_files_ls = as.vector(txt_files_ls)
  txt_files_df <- lapply(txt_files_ls, function(x) {utils::read.delim(file = paste(path, "/",x,sep=""))})
  combined_df <- do.call("bind_rows", lapply(txt_files_df, as.data.frame))
  dummy = as.vector(c(1:length(txt_files_ls)))
  for (i in 1:length(txt_files_ls)){
    dummy[[i]] <- length(txt_files_df[[i]]$X)
  }
  Directory = rep(txt_files_ls, times = dummy)
  Directory
  Photo.Order = factor(rep (1:length(txt_files_ls),times = dummy))
  DB = data.frame(Photo.Order,Directory,combined_df)
  colnames(DB)[which(colnames(DB) == "Length")] = c("Cal.Length")
  DB = DB[-which(colnames(DB) == "X.1")]
  DB = DB[-which(colnames(DB) == "X")]
  DB = DB[-which(colnames(DB) == "Y")]

  #Split columns to make new variables
  # Ignore Warnings in these lines " Expected 2 [or 3] pieces...."
  DB = tidyr::separate(DB, .data$Label, c("Photo.Name", "ROI"), ":")
  DB = suppressWarnings(tidyr::separate(DB, .data$ROI, into = c("ROI.Code", "ROI.RepLab"), # "?<=" means look behind the split point, for any uppr or lower case letters ("A-Za-z")
                                 sep = "(?<=[A-Za-z])(?=[0-9])"))         # "?=" means look ahead after the cursor

  DB = suppressWarnings(tidyr::separate(DB, .data$ROI.RepLab, into = c("ROI.Rep", "ROI.Label"),# https://stackoverflow.com/questions/9756360/split-character-data-into-numbers-and-letters
                                 sep = "(?<=[0-9])(?=[A-Za-z])"))
  DB = suppressWarnings(tidyr::separate(DB, .data$Directory, var.names, "/"))
  fact.cols = colnames(DB)[2:(length(var.names)+5)]
  DB[fact.cols] <- lapply(DB[fact.cols],
                          factor)

  DB = DB %>%
    dplyr::group_by_at(var.names) %>%
    dplyr::mutate(Photo.Rep = as.factor(as.numeric(as.character(.data$Photo.Order)) - min(as.numeric(as.character(.data$Photo.Order))) + 1))
  colnames(DB)
  insert.ind = which(colnames(DB) == var.names[length(var.names)])
  DB = DB[c(1:insert.ind, ncol(DB), (insert.ind + 1):(ncol(DB)-1))]
  return(DB)
}
