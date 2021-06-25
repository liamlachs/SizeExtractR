#' Add user-defined ROI Label Code Variables to SizeExractR Database, based on ROI Label Code Translator Matrix
#'
#' @param data An object of class dataframe as output directly from SizeExtractR::Build_Uncalibrated_Dataset()
#' @param label.translator An object of class data.frame as output directly from SizeExtractR::CheckSet_ROILabelCodeVars. See documentation for that function for more information.
#'
#' @return Adds to the SizeExtractR database (and returns this dataframe object) by deconstructing the ROI.LabelCode variable and forming it into new columns showing TRUE or FALSE (i.e. yes or no) for each of the different ROI Label Code variables.
#' @return Note that the user-defined ROI Label Code variables are for ROIs of interest, not the calibration ROIs (ROI.Type = "Cali_Pts" or "M" denoting calibration points and measurement length respectively).
#'
#' @export
#'
#' @examples
#' # load in the output of Build_Uncalibrated_Dataset and CheckSet_ROILabelCodeVars
#' data(Database)
#' data(Label.Translator)
#'
#'
#' # Run the function
#' Database.ROIlabelcode = Add_ROILabelCodeVars(Database, Label.Translator)
#'
#' # Dataframe withour ROI codes
#' head(Database[(ncol(Database)-5):ncol(Database)])
#'
#' # DataframeWith ROI codes
#' head(Database.ROIlabelcode[(ncol(Database.ROIlabelcode)-5):ncol(Database.ROIlabelcode)])
#'
Add_ROILabelCodeVars = function(data, label.translator){

  if(length(label.translator) == 1 && label.translator == "No_Label_Codes_Used"){
    message("No label codes used - returning same dataframe as input dataframe")
    return(data)
  } else {
    data$ROI.LabelCode = addNA(data$ROI.LabelCode)
    datalab = as.data.frame(matrix(NA,nrow = nrow(data), ncol = ncol(label.translator)-1))
    colnames(datalab) = colnames(label.translator)[-1]
    for(j in 1:ncol(datalab)){
      for(i in 1:nrow(label.translator)){
        datalab[data$ROI.LabelCode == label.translator$ROI.Label[i],j] = as.character(label.translator[i,j+1])
      }
    }
    datalab.cols = colnames(datalab)
    datalab[datalab.cols] <- lapply(datalab[datalab.cols],
                                    factor)
    datalab = cbind(data, datalab)
    return(datalab)
  }

}
