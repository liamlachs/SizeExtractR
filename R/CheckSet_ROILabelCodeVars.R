#' Check for mistakes in ROI Label Codes from the SizeExctractR ImageJ tools and protocol
#'
#' @param data An object of class dataframe as output directly from SizeExtractR::Build_Uncalibrated_Dataset()
#' @param path A Directory path within which holds the text files outputted from SizeExtractR imageJ tools and protocol. The directory path should be given in double quotes in the same format as that returned by the function getwd(), and should be the same as that used in SizeExtractR::Build_Uncalibrated_Dataset()
#'
#' @return Runs through interactive checks for the user to determine if the ROI Label Codes (i.e., additional notes about each ROI of interest) used in their image analysis are correct, and help to identify any images that had mistakes in the labelling process. If mistakes are found support information is given to the user.
#' @return Prompts the user to set up a Label csv file with the required information needed to create the label translator matix
#' @return Once the csv file has been created and edited propoerly, this function returns a Label Translator Matrix. This can be used to check that the Label codes will translent to the correct Variable names.
#'
#' @export
#'
#' @examples
#' # load in the output of Build_Uncalibrated_Dataset
#' mypath = paste0(.libPaths()[1],"/SizeExtractR/TextFiles")
#' data(Database)
#'
#' # Run the function
#' Label.Translator = CheckSet_ROILabelCodeVars(Database, mypath)
#'
#' if(length(Label.Translator) > 0){
#'   print(Label.Translator, row.names = F)
#' }
#'
CheckSet_ROILabelCodeVars = function(data, path){
  if(interactive()){
    Lab.txt.levs = levels(as.factor(data$ROI.LabelCode))
    if(length(Lab.txt.levs) == 0){
      message("No label codes used - continue.")
      return("No_Label_Codes_Used")
    } else {
      Lab.txt.chars = unique(strsplit(paste(Lab.txt.levs, collapse=""),"")[[1]])
      N.Lab.txt.chars = length(Lab.txt.chars)

      csvDir = paste(path,"/","ROI_Labels.csv",sep="")
      csvTemplate = data.frame(ROI_Label_code = rep("<fill label text here>",N.Lab.txt.chars),
                               Corresponding_Variable_Name = rep("<fill variable name here>",N.Lab.txt.chars))

      if(file.exists(csvDir) == FALSE){
        utils::write.csv(csvTemplate, csvDir, row.names = FALSE)

        message(paste("_________________________________________________________________\n\n",
                      ".csv file 'ROI_Labels.csv' was just created in the path directory\n",
                      "Follow the next 3 steps (outside of R):\n\n",
                      "   1) Open the .csv file\n",
                      "   2) Fill in the ROI label codes relavent to your image analysis\n",
                      "   3) Fill in the corresponding Variable names (no spaces)\n",
                      "   4) Save and close the .csv file\n",
                      "   5) Rerun the CheckSet_ROILabelCodeVars function.\n\n",
                      "   NOTE: do not change the .csv filename or move to a different directory.\n",
                      "         see label characters from text file imports (may contain errors).\n",
                      "_________________________________________________________________\n\n"))
        print(data.frame(Label.Characters = Lab.txt.chars),row.names = F)
      } else {

        # Check the ROI label codes are correct
        Label.csv.data = utils::read.csv(csvDir)

        if(any(Label.csv.data$ROI_Label_code              == "<fill variable name here>" |
               Label.csv.data$Corresponding_Variable_Name == "<fill variable name here>")){
          message(paste("\n________________________________________________________________\n\n",
                        "Error: please revise the ROI Label template csv file. Cell entries\n",
                        "showing '<fill variable name here>' must be removed or amended.",
                        "\n________________________________________________________________\n\n"))
          # Insert an exit function that stops the function here
        } else {

          Label.csv.data$ROI_Label_code = as.factor(Label.csv.data$ROI_Label_code)
          Label.csv.data$Corresponding_Variable_Name = as.factor(Label.csv.data$Corresponding_Variable_Name)

          # Check that label codes from CSV match label codes from the data imported from text files
          Lab.csv.levs = levels(Label.csv.data$ROI_Label_code)
          Lab.csv.chars = unique(strsplit(paste(Lab.csv.levs, collapse=""),"")[[1]])
          N.Lab.csv.chars = length(Lab.csv.chars)

          # If there is a mismatch
          if(all(Lab.csv.chars == Lab.csv.chars) == FALSE){
            message(paste("\n________________________________________________________________\n\n",
                          "Error: characters used in ROI Label Codes do not match between the\n",
                          "csv template, and the data imported from text files"))

            message("\nfrom csv template")
            #print(data.frame(Characters.used = Lab.csv.chars),row.names = F)
            print(Label.csv.data, row.names = F)

            message("\nfrom text files")
            print(data.frame(Characters.used = Lab.txt.chars),row.names = F)

            message(paste("Suggestions:\n",
                          "First check the Label Codes are correct in the csv template.\n",
                          "If the issue persists then check for errors in the image analysis text files.",
                          "\n________________________________________________________________\n\n"))

            # Insert an exit function that stops the function here
          }


          # If all is okay then double check with user
          if(all(Lab.csv.chars == Lab.csv.chars) == TRUE){
            message(paste("\n________________________________________________________________\n\n",
                          "Double check that the ROI label codes correspond correctly to\n",
                          "the Variable Names imported from the csv Template file\n",
                          "\n________________________________________________________________\n\n"))

            print(Label.csv.data, row.names = F)

            message(paste("\n_____________________________________\n\n",
                          "Is the ROI labeling system correct?\n\n",
                          "_____________________________________\n\n"))

            x = utils::menu(c("Yes", "No"))

            if(x != 1 && x != 2){
              message("Error: must enter either 1 or 2\n\nTry again")
              # Insert an exit function that stops the function here
            }

            if(x == 2){
              message(paste("Please make relevant changes to ROI Label csv file\n",
                            "For more information see help files", sep = ""))
              # Insert an exit function that stops the function here
            }

            if(x == 1){
              #message("\nGood - labelling is correct - continue.")

              # Set the Label.Translator dataframe
              Label.Translator = data.frame(ROI.LabelCode = levels(as.factor(data$ROI.LabelCode)))
              Label.Translator[2:(N.Lab.txt.chars+1)] = NA
              colnames(Label.Translator)[2:(N.Lab.txt.chars+1)] <- as.character(Label.csv.data$Corresponding_Variable_Name)

              # For each row in Label Translator calculate which Label Code variables have a TRUE or FALSE
              # This is based on checking whether the string is contained or not
              # Note: if one variable label code is "b" and a second variable label code is "bb"
              #       then the "bb" will show a false positive for "b"
              for(i in 1:nrow(Label.Translator)){
                Label.Translator[i,2:(N.Lab.txt.chars+1)] = rbind(lapply(Label.csv.data$ROI_Label_code,
                                                                         function(x){ grepl(x, Label.Translator$ROI.LabelCode[i], fixed = TRUE)}))
              }

              # To solve the false positive issue for "bb" as stated above, for example,
              #    when one variable label code ("bb") is a repeat of a different variable label code ("b")
              for( i in 1:nrow(Label.csv.data)){
                N.reps = stringr::str_count(Label.Translator$ROI.Label, as.character(Label.csv.data$ROI_Label_code)[i])
                Label.Translator[,1+i] = N.reps == 1
              }

              Label.Translator = rbind(Label.Translator, c(NA,rep(FALSE,ncol(Label.Translator)-1)))
              fact.cols = colnames(Label.Translator)
              Label.Translator[fact.cols] <- lapply(Label.Translator[fact.cols],
                                                    factor)
              Label.Translator$ROI.LabelCode = addNA(Label.Translator$ROI.LabelCode)


              print(Label.Translator, row.names = F)

              message(paste("\n_____________________________________\n\n",
                            "Is the processed ROI labeling system correct?\n",
                            "     Note: the ROI.label <NA> should be FALSE\n",
                            "           for all subsequent variables.\n\n",
                            "_____________________________________\n\n"))

              x = utils::menu(c("Yes", "No"))

              if(x != 1 && x != 2){
                message("Error: must enter either 1 or 2\n\nTry again")
                # Insert an exit function that stops the function here
              }

              if(x == 2){
                message(paste("Please contact authors", sep = ""))
                # Insert an exit function that stops the function here
              }

              if(x == 1){
                message("\nGood - labelling is correct - continue.")
                return(Label.Translator)
              }
            }
          }
        }
      }
    }
  } else {
    message("Not in interactive mode")
  }
}
