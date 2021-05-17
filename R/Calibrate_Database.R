#' Calibrate the SizeExtractR database, based on known calibration length (cm)
#'
#' @param data An object of class dataframe as output directly from SizeExtractR::Add_ROILabelVars()
#' @param known.length a numerical value of the length of calibration lengths from image analyses (in centimeters).
#'
#' @return For each image independently, this function returns a calibrated  SizeExtractR database (dataframe object).
#' @return It does these calibrations on a per image basis. The mean number of pixels is calculated among all the calibration lengths in that image (i.e., ROIs with ROI.Code of "M"). This is then compared to the 'known.length' parameter, to compute all aspects of size of each ROI.
#' @return Length, Position, Area, and Volume are all given in square centimeters.
#'
#' @export
#'
#' @examples
#' Database.cal = Calibrate_Database(Database.ROILab, known.length = 10)
#'
Calibrate_Database = function(datalab, known.length){
  Cali = datalab %>% subset(ROI.Code=="M") # Choose M because these are the calibration lengths
  Cali$ROI.Code = as.character(Cali$ROI.Rep)
  Cali2 <- Cali %>%
    group_by(Photo.Order) %>%
    summarise(Cal.nums = max(as.character(ROI.Rep)),
              Cal.Length.mean = mean(Cal.Length),
              Cal.Length.sd = sd(Cal.Length))
  datacal = left_join(datalab, Cali2, by = "Photo.Order")

  datacal[] <- lapply(datacal, function(x) if(is.factor(x)) factor(x) else x)
  # CALIBRATIONS
  # CHECK ALL CALIBRATION METHODS
  # All square centimeters
  datacal$Area = (datacal$Area*(known.length^2))/(datacal$Cal.Length.mean^2)
  datacal$Diameter.circular = sqrt(datacal$Area/pi)*2
  datacal$Volume.spherical = ((4*pi)/3)*((datacal$Diameter.circular/2)^3)
  datacal$XM = (datacal$XM*known.length)/datacal$Cal.Length.mean
  datacal$YM = (datacal$YM*known.length)/datacal$Cal.Length.mean
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
  }


  return(datacal)
}
