#' Calibrate the SizeExtractR database, based on known calibration length (cm)
#'
#' @param datalab An object of class dataframe as output directly from SizeExtractR::Add_ROILabelCodeVars()
#' @param known.length a numerical value of the length of calibration lengths from image analyses (in user-defined units, e.g. centimeters).
#'
#' @return For each image independently, this function returns a calibrated  SizeExtractR database (dataframe object).
#' @return It does these calibrations on a per image basis. The mean number of pixels is calculated among all the calibration lengths in that image (i.e., ROIs with ROI.Type of "M"). This is then compared to the 'known.length' parameter, to compute all aspects of size of each ROI.
#' @return Length, Position, Area, and Volume are all given in user-defined units, e.g. centimeters.
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
#' @examples
#' # load in the output of Add_ROILabelCodeVars
#' data(Database.ROILabelCode)
#'
#' #Run the function
#' Database.cal = Calibrate_Database(Database.ROILabelCode, known.length = 1)
#'
#' # Histogram of Egg Sizes:
#' par(mfrow = c(1,2))
#' # Uncalibrated
#' ind = which(Database.ROILabelCode$ROI.Type == "e")
#' hist(Database.ROILabelCode$Area[ind], xlab = "Area (pixels)", main = "Uncalibrated Egg Sizes")
#'
#' # Calibrated
#' ind2 = which(Database.cal$ROI.Type == "e")
#' hist(Database.cal$Area[ind2], xlab = bquote(Area~(cm^2)), main = "Calibrated Egg Sizes")
#'
Calibrate_Database = function(datalab, known.length){
  Cali = datalab[which(datalab$ROI.Type=="M"),] # Choose M because these are the calibration lengths
  Cali$ROI.Type = as.character(Cali$ROI.Rep)
  Cali2 <- Cali %>%
    dplyr::group_by(.data$Photo.Order) %>%
    dplyr::summarise(Cal.nums = max(as.character(.data$ROI.Rep)),
              Cal.Length.mean = mean(.data$Cal.Length),
              Cal.Length.sd = stats::sd(.data$Cal.Length))
  datacal = dplyr::left_join(datalab, Cali2, by = "Photo.Order")

  datacal[] <- lapply(datacal, function(x) if(is.factor(x)) factor(x) else x)
  # CALIBRATIONS
  # CHECK ALL CALIBRATION METHODS
  # All in user-defined units, e.g. centimeters
  datacal$Area = (datacal$Area*(known.length^2))/(datacal$Cal.Length.mean^2)
  datacal$Diameter.circular = sqrt(datacal$Area/pi)*2
  datacal$Volume.spherical = ((4*pi)/3)*((datacal$Diameter.circular/2)^3)
  if(is.element("X", colnames(datacal))){
    datacal$X = (datacal$X*known.length)/datacal$Cal.Length.mean
  }
  if(is.element("Y", colnames(datacal))){
      datacal$Y = (datacal$Y*known.length)/datacal$Cal.Length.mean
  }
  if(is.element("Perim.", colnames(datacal))){
    datacal$Perim. = (datacal$Perim.*known.length)/datacal$Cal.Length.mean
  }
  datacal$BX = (datacal$BX*known.length)/datacal$Cal.Length.mean
  datacal$BY = (datacal$BY*known.length)/datacal$Cal.Length.mean
  datacal$Width = (datacal$Width*known.length)/datacal$Cal.Length.mean
  datacal$Height = (datacal$Height*known.length)/datacal$Cal.Length.mean

  if(length(grep("Feret", colnames(datacal))) > 0){
    datacal$Feret = (datacal$Feret*known.length)/datacal$Cal.Length.mean
    datacal$FeretX = (datacal$FeretX*known.length)/datacal$Cal.Length.mean
    datacal$FeretY = (datacal$FeretY*known.length)/datacal$Cal.Length.mean
    datacal$MinFeret = (datacal$MinFeret*known.length)/datacal$Cal.Length.mean
    datacal$Volume.elliptical = (4/3) * pi * datacal$Feret * (datacal$MinFeret^2)
    datacal$Geom.Mean.D = sqrt(datacal$Feret * datacal$MinFeret)
  }

  datacal = datacal[,-which(is.na(match(colnames(datacal),c("Mean","Min","Max","X","Y","XM","YM","BX","BY","Width","Height","Angle","FeretX","FeretY","FeretAngle","Cal.Length.sd","Volume.elliptical")))==FALSE)]

  return(datacal)
}
