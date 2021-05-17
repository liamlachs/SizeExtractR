#' Add Variables to SizeExractR Database, based on ROI Label Translator Matrix
#'
#' @param data An object of class dataframe as output directly from SizeExtractR::Build_Uncalibrated_Dataset()
#' @param label.translator An object of class data.frame as output directly from SizeExtractR::CheckSet_ROILabelVars. See documentation for that function for more information.
#'
#' @return Adds to the SizeExtractR database (and returns this dataframe object) by deconstructing the ROI.Label variable and forming it into new columns showing TRUE or FALSE (i.e. yes or no) for each of the different labelling variables.
#' @return Note that the labelling variables are for ROIs of interest, not the calibration ROIs (ROI.Code = "Cali_Pts" or "M" denoting calibration points and measurement length respectively).
#'
#' @export
#'
#' @examples
#' Database.ROILab = Add_ROILabelVars(Database, Label.Translator)
#'
Add_ROILabelVars = function(data, label.translator){

  data$ROI.Label = addNA(data$ROI.Label)
  datalab = as.data.frame(matrix(NA,nrow = nrow(data), ncol = ncol(Label.Translator)-1))
  colnames(datalab) = colnames(Label.Translator)[-1]
  for(j in 1:ncol(datalab)){
    for(i in 1:nrow(Label.Translator)){
      datalab[data$ROI.Label == Label.Translator$ROI.Label[i],j] = as.character(Label.Translator[i,j+1])
    }
  }
  datalab.cols = colnames(datalab)
  datalab[datalab.cols] <- lapply(datalab[datalab.cols],
                                  factor)
  datalab = cbind(data, datalab)
  return(datalab)
}
