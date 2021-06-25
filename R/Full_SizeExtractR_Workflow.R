#' Run the full SizeExtractR workflow - a series of nested functions to extract all sizes.
#'
#' @param path A Directory path within which holds the text files outputted from SizeExtractR imageJ tools and protocol. The directory path should be given in double quotes in the same format as that returned by the function getwd()
#' @param known.calibration.length a numerical value of the length of calibration lengths from image analyses (in centimeters).
#' @param include.calibrations logical. Determines whether or not to include the calibration-length ROIs alongside the ROIs of interest.
#'
#' @return Returns a calibrated dataset of size metrics, that includes user defined variables based on 1) the directory structure, and 2) the ROI labels used in image analyses.
#' @return All relevant interactive checks are also undertaken to determine whether the exported database is correct or not.
#'
#' @export
#'
#' @examples
#' # Set directory within which all text files are located
#' mypath = paste0(.libPaths()[1],"/SizeExtractR/TextFiles")
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
  if(interactive()){
    # 1) Set Variable Names
    var.names = CheckSet_DirecVars(path)
    if(length(var.names) == 0){
      message("Aborted rest of workflow")
    }

    # 2) Build Uncalibrated Database
    if(length(var.names) > 0){
      data = Build_Uncalibrated_Dataset(path, var.names)

      # 3) Check ROI Types are okay
      ROI_Types = Check_ROI_Types(data)
      if(ROI_Types$Success[1] == "No"){
        message("Aborted rest of workflow")
        return(ROI_Types)
      }

      # 4) Check user-defined ROI Code and make Translator
      if(ROI_Types$Success[1] == "Yes"){
        label.translator = CheckSet_ROILabelCodeVars(data, path)
        if(is.data.frame(label.translator)){
          if(nrow(label.translator) == 0){
            message("Aborted rest of workflow")
          }
        }

        if(is.data.frame(label.translator)){
          if(nrow(label.translator) > 0){

            # 5) Add user-defined ROI Code Variables to Database
            data.ROIcode = Add_ROILabelCodeVars(data, label.translator)

            # 6) Calibrate Database
            data.cal = Calibrate_Database(data.ROIcode, known.calibration.length)

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
        } else {

          # 5) Add user-defined ROI Code Variables to Database
          data.ROIcode = Add_ROICodeVars(data, label.translator)

          # 6) Calibrate Database
          data.cal = Calibrate_Database(data.ROIcode, known.calibration.length)

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
  } else {
    message("Not in interactive mode")
  }
}
