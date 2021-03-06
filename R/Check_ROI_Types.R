#' Check for mistakes in ROI Types from the SizeExctractR ImageJ tools and protocol
#'
#' @param data An object of class dataframe as output directly from SizeExtractR::Build_Uncalibrated_Dataset()
#'
#' @return Runs through interactive checks for the user to determine if the ROI Types (i.e., main type of each object i.e. Calibration point, calibration length or ROI of interest) used in their image analysis are correct, and help to identify any images that had mistakes in the labelling process. If mistakes are found support information is given to the user.
#' @return Also returns a character string reading "success", which is not used in further SizeExtractR functions.
#'
#' @export
#'
#' @examples
#' # load in the output of Build_Uncalibrated_Dataset
#' data(Database)
#'
#' # Run the function
#' ROI.Types = Check_ROI_Types(Database)
#' if(length(ROI.Types) > 0){print(ROI.Types)}
Check_ROI_Types = function(data){
  if(interactive()){
    message(paste("\n_________________________________________________________________\n\n",
                  "N.B.\n",
                  "Please check that the ROI Type Names are correct.\n\n",
                  "The Database formation function will not work if there are ny errors.\n\n",
                  "    ROI Type Names refer to the name given to that specific object\n",
                  "during imageJ classification. 'M' and 'Cali_Pts' are the names given\n",
                  "to the calibration points and measurement length object. These will be\n",
                  "used for calibration. Other ROI Type Names are determined by the user.\n",
                  "\n. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .\n"))

    levs = levels(data$ROI.Type)
    levs = data.frame(ROI.Type.Names = levs)
    row.names(levs) = NULL
    message(paste("\n__________________________\n\n    ",
                  "ROI.Type",
                  "\n. . . . . . . . . . . . . .\n"))
    print(levs, row.names = FALSE)

    message(paste("\n_____________________________________\n\n",
                  "Are the ROI Type Names Correct?\n",
                  "     (see above for details)\n\n",
                  "_____________________________________\n\n"))

    x = utils::menu(c("Yes", "No"))

    if(x != 1 && x != 2){
      message("Error: must enter either 1 or 2\n\nTry again")
    }

    if(x == 2){
      print(levs, row.names = FALSE)
      message(paste("\n_____________________________________\n\n",
                    "Which ROI Type Names are incorrect?\n\n",
                    "Input:\n",
                    "   exact Type names,\n",
                    "   case sensitive,\n",
                    "   separated by comma,\n",
                    "   without spaces:\n\n",
                    "_____________________________________\n\n"))
      y <- readline("Input: ")
      if(grepl(y, " ", fixed = TRUE) == TRUE){
        message(paste("\n_____________________________________\n\n",
                      "Error: space used in input. Try again.\n",
                      "_____________________________________\n\n"))
        # Insert an exit function that stops the function here
      }

      y = strsplit(y,",")[[1]]


      ErrorObjects = data[which(is.na(match(data$ROI.Type, y)) == FALSE),c(2:which(colnames(data) == "ROI.Type"))]
      print(ErrorObjects, row.names = FALSE)
      message(paste("\n\nPlease make relevant changes to ROI names by reloading and amending\n",
                    "that specifc image in imageJ.\n",
                    "Then run the script again.\n\n",
                    "See the offending Image location above.\n\n",
                    "Or simply answer 'yes' that the ROI Type is correct.", sep = ""))
      ErrorObjects$Success = "No"
      return(ErrorObjects)
    }


    if(x == 1){
      message("\nGood - ROI Types are correct - continue.")
      levs$Success = "Yes"
      return(levs)
    }
  } else {
    message("Not in interactive mode")
  }
}
