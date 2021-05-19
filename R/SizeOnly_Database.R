#' Reduce the calibrated SzeExtractR database by removing all calibration ROIs
#'
#' @param datacal An object of class dataframe as output directly from SizeExtractR::Calibrate_Database()
#'
#' @return Returns a calibrated SizeExtractR database (i.e., dataframe object) without points and lengths used in calibrations. THis is helpful when it comes to indexing the dataset for plotting using ggplot or other packages.
#'
#' @export
#'
#' @examples
#' # load in the output of Add_ROILabelVars
#' load(paste0(path.package("SizeExtractR"), "/data/Database.cal.RData"))
#'
#' # Run the function
#' Database.SizeOnly = SizeOnly_Database(Database.cal)
#'
#' # Histogram of Egg Sizes - Note the indexing is not needed for the latter dataset
#' par(mfrow = c(1,2))
#'
#' # Full Dataset
#' ind=which(Database.cal$ROI.Code == "e")
#' hist(Database.cal$Area[ind], xlab = bquote(Egg~Area~(cm^2)), main = "Full Dataset")
#'
#' # SizeOnly Dataset
#' hist(Database.SizeOnly$Area, xlab = bquote(Egg~Area~(cm^2)), main = "Size Only Dataset")
#'
SizeOnly_Database = function(datacal){
  datacal.SizeOnly = datacal[which(is.na(match(datacal$ROI.Code,c("Cali_Pts","M"))) == TRUE),]
  return(datacal.SizeOnly)
}
