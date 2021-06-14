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
#' # load in the output of CheckSet_DirecVars
#' mypath = paste0(.libPaths()[1],"/SizeExtractR/TextFiles")
#' print(mypath)
#' data(varnames)
#'
#' # Run the function
#' Database = Build_Uncalibrated_Dataset(mypath, varnames)
#' head(Database)
#'
Build_Uncalibrated_Dataset = function(path, var.names){
  if(substr(path,nchar(path),nchar(path)) == "/"){
    path = substr(path,1,nchar(path)-1)
  }

  # Load in the data
  txt_files_ls = list.files(path=path, pattern="*.txt", recursive = TRUE)
  txt_files_ls = as.vector(txt_files_ls)
  txt_files_df <- lapply(txt_files_ls, function(x) {as.data.frame(utils::read.delim(file = paste(path, "/",x,sep="")))})
  # Remove Row Number
  txt_files_df <- lapply(txt_files_df,  function(x) {if(colnames(x)[1] == "X.1" | colnames(x)[1] == "X"){x[-1]} else {x}})
  #combined_df <- dplyr::bind_rows(lapply(txt_files_df, as.data.frame))
  combined_df <- dplyr::bind_rows(txt_files_df)
  dummy = as.vector(c(1:length(txt_files_ls)))
  for (i in 1:length(txt_files_ls)){
    dummy[[i]] <- length(txt_files_df[[i]]$X)
  }
  Directory = rep(txt_files_ls, times = dummy)
  Directory = sub("ImageJ_Output/","",Directory)
  Photo.Order = factor(rep (1:length(txt_files_ls),times = dummy))
  DB = data.frame(Photo.Order,Directory,combined_df)
  colnames(DB)[which(colnames(DB) == "Length")] = c("Cal.Length")
  DB = DB[-which(colnames(DB) == "Counter")]
  DB = DB[-which(colnames(DB) == "Count")]

  #Split columns to make new variables
  # Ignore Warnings in these lines " Expected 2 [or 3] pieces...."
  DB = tidyr::separate(DB, .data$Label, c("Photo.Name", "ROI"), ":")
  DB = suppressWarnings(tidyr::separate(DB, .data$ROI, into = c("ROI.Code", "ROI.RepLab"), # "?<=" means look behind the split point, for any uppr or lower case letters ("A-Za-z")
                                 sep = "(?<=[A-Za-z])(?=[0-9])"))         # "?=" means look ahead after the cursor

  DB = suppressWarnings(tidyr::separate(DB, .data$ROI.RepLab, into = c("ROI.Rep", "ROI.Label"),# https://stackoverflow.com/questions/9756360/split-character-data-into-numbers-and-letters
                                 sep = "(?<=[0-9])(?=[A-Za-z])"))
  if(length(var.names) == 1 && var.names == "No_SubFolders_Present"){
    fact.cols = colnames(DB)[2:5]
    DB[fact.cols] <- lapply(DB[fact.cols],
                            factor)

    DB = DB %>%
      dplyr::mutate(Photo.Rep = as.factor(as.numeric(as.character(.data$Photo.Order)) - min(as.numeric(as.character(.data$Photo.Order))) + 1))
    return(DB)

  } else {
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
}
