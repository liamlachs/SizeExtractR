#' Run the full SizeExtractR workflow - a series of nested functions to extract all sizes.
#'
#' @param path A Directory path within which holds the text files outputted from SizeExtractR imageJ tools and protocol. The directory path should be given in double quotes in the same format as that returned by the function getwd()
#' @param known.calibration.length a numerical value of the length of calibration lengths from image analyses (in centimeters).
#' @param include.calibrations either TRUE of FALSE, determines whether or not to include the calibration-length ROIs alongside the ROIs of interest.
#'
#' @return Returns a calibrated dataset of size metrics, that includes user defined variables based on 1) the directory structure, and 2) the ROI labels used in image analyses.
#' @return All relevant interactive checks are also undertaken to determine whether the exported database is correct or not.
#'
#' @export
#'
#' @examples
#' # Set directory within which all text files are located
#' mypath = paste0(path.package("SizeExtractR"), "/inst/TextFiles")
#'
#' # Run the function
#'
#' Database.cal.SizeOnly = Full_SizeExtractR_Workflow(mypath, 1, FALSE)
#'
#' # Show dataset
#' if(length(Database.cal.SizeOnly) > 0){
#'   head(Database.cal.SizeOnly)
#'   colnames(Database.cal.SizeOnly)
#'   hist(Database.cal.SizeOnly$Area, xlab = bquote(Egg~Area~(cm^2)), main = "Size Only Dataset")
#' }
#'
Full_SizeExtractR_Workflow = function(path, known.calibration.length, include.calibrations){

  # 1) Set Variable Names
  var.names = CheckSet_DirecVars(path)
  if(length(var.names) == 0){
    message("Aborted rest of workflow")
  }

  # 2) Build Uncalibrated Database
  if(length(var.names) > 0){
    data = Build_Uncalibrated_Dataset(path, var.names)

    # 3) Check ROI Codes are okay
    ROI_Codes = Check_ROI_Codes(data)
    if(ROI_Codes$Success[1] == "No"){
      message("Aborted rest of workflow")
    }

    # 4) Check ROI Labels and make Translator
    if(ROI_Codes$Success[1] == "Yes"){
      label.translator = CheckSet_ROILabelVars(data, path)
      if(nrow(label.translator) == 0){
        message("Aborted rest of workflow")
      }

      # 5) Add ROI Labels to Database
      if(nrow(label.translator) > 0){
        data.ROIlab = Add_ROILabelVars(data, label.translator)

        # 6) Calibrate Database
        data.cal = Calibrate_Database(data.ROIlab, known.calibration.length)

        # 7) Return Size Only Database
        data.sizeonly = SizeOnly_Database(data.cal)

        if(include.calibrations == TRUE){
          message("Success - Calibrated Full Dataset returned to R object")
          return(data.cal)
        }

        if(include.calibrations == FALSE){
          message("Success - Calibrated Size-only Dataset returned to R object")
          return(data.sizeonly)
        }
      }
    }
  }
}
