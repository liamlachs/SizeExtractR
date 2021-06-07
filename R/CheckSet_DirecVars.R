#' Check Directory Structure, and Set Directory-derived Variable Names
#'
#' @param path A Directory path within which holds the text files outputted from SizeExtractR imageJ tools and protocol. The directory path should be given in double quotes in the same format as that returned by the function getwd()
#'
#' @return Returns a vector of Variable Names that will be used later in database formation within SizeExtractR.
#' @return It also provides a user interface to check the Directory structure is suitable for other SizeExtractR functions. If the Directory structure is incorrect this function prompts the user about how to fix the directory structure.
#'
#' @export
#'
#' @examples
#' # Set the path within which all text files (from image analysis) are found
#' mypath = paste0(.libPaths()[1],"/SizeExtractR/TextFiles")
#'
#' # Run the function
#' varnames = CheckSet_DirecVars(mypath)
#' if(length(varnames) > 0){print(varnames)}
#'
CheckSet_DirecVars = function(path){
  if(interactive()){
    if(substr(path,nchar(path),nchar(path)) == "/"){
      path = substr(path,1,nchar(path)-1)
    }

    DS <- fs::dir_ls(path = path, recurse = TRUE, type = "directory") # Check only the folder names within path
    DS = sub(paste(path,"/",sep=""),"",DS)
    DS = unname(strsplit(DS, split = "/")) # split them on the "/" symbol
    DS = lapply(DS, function(x){x = x[x != "ImageJ_Output"]})
    if(length(DS) == 0){
      message("No subfolders present")
      return("No_SubFolders_Present")
    } else {
      if(sum(sapply(DS, length))==0){
        DS=list()
      }
    }
    if(length(DS) == 0){
      message("No subfolders present, except ImageJ_Output")
      return("No_SubFolders_Present")
    } else {
      N.folders <- sapply(DS, length) # number of folders in each directory
      if(length(unique(N.folders)) > 1){
        DS <- as.data.frame(t(sapply(DS, "[", i = 1:max(N.folders))))
      } else if(length(unique(N.folders)) == 1){
        DS <- as.data.frame(t(t(sapply(DS, "[", i = 1:max(N.folders)))))
      }
      colnames(DS) <- paste("Directory","Level",1:ncol(DS))
      utils::head(DS)

      message(paste("\n_________________________________________________________________\n\n",
                    "N.B.\n",
                    "Please check that the following directory structure is correct.\n\n",
                    "    Each 'Directoy Level' should only show the folder names\n",
                    "(i.e., subdirectories) from that particular category, as defined\n",
                    "by the users own experimental design.\n\n",
                    "    For instance, for site surveys repeated over multiple years,\n",
                    "'Directoy Level 1' would show only years,\n",
                    "'Directoy Level 2' would show only Site IDs.\n\n",
                    "    If Directory Structure is wrong, or there are any mis-located\n",
                    "folders, then the folders must be re-organised into the correct\n",
                    "structure manually, and then the SizeExtractR tools can be used",
                    "\n. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .\n"))

      for(i in 1:ncol(DS)){
        levs = levels(as.factor(as.character(DS[,i])))
        levs = data.frame(Folder.Names = levs)
        row.names(levs) = NULL
        message(paste("\n__________________________\n\n    ",
                      colnames(DS)[i],
                      "\n. . . . . . . . . . . . . .\n"))
        print(levs, row.names = FALSE)
      }

      message(paste("\n_____________________________________\n\n",
                    "Is the directory structure correct?\n",
                    "     (see above for details)\n",
                    "_____________________________________\n\n"))

      x = utils::menu(c("Yes", "No"))

      if(x != 1 && x != 2){
        message("Error: must enter either 1 or 2\n\nTry again")
      }

      if(x == 2){
        message(paste("Please make relevant changes to directory structure\n",
                      "For more information see help files", sep = ""))
      }


      if(x == 1){
        message("\nGood - directory is correct - continue.")
        Variables = c()
        # Set the Variable Names
        for(i in 1:ncol(DS)){
          message(paste("\n_______________________________________________\n\n",
                        "Enter a variable name for 'Directory Level ",i,"'.\n",
                        "     (e.g. Year, no spaces allowed),\n",
                        "      see below: 1st few foldernames in this directory level\n",
                        "_______________________________________________\n\n", sep=""))
          levs = levels(as.factor(as.character(DS[,i])))
          levs = data.frame(Folder.Names = levs)
          row.names(levs) = NULL
          print(utils::head(levs), row.names = FALSE)

          Var.temp <- readline("Answer: ")
          if(i == 1){
            Variables[i] = Var.temp
          } else {
            Variables = c(Variables, Var.temp)
          }
        }
        return(Variables)
      }
    }
  } else {
    message("Not in interactive mode")
  }
}
