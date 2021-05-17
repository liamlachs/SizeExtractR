#' Reduce the calibrated SzeExtractR database by removing all calibration ROIs
#'
#' @param datacal An object of class dataframe as output directly from SizeExtractR::Calibrate_Database()
#'
#' @return Returns a calibrated SizeExtractR database (i.e., dataframe object) without points and lengths used in calibrations.
#'
#' @export
#'
#' @examples
#' Database.SizeOnly = SizeOnly_Database(Database.cal)
#'
SizeOnly_Database = function(datacal){
  datacal.SizeOnly = datacal[which(is.na(match(datacal$ROI.Code,c("Cali_Pts","M"))) == TRUE),]
  return(datacal.SizeOnly)
}
