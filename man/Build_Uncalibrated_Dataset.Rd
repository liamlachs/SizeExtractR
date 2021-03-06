% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Build_Uncalibrated_Dataset.R
\name{Build_Uncalibrated_Dataset}
\alias{Build_Uncalibrated_Dataset}
\title{Build the Uncalibrated Dataset for further use in SizeExctractR}
\usage{
Build_Uncalibrated_Dataset(path, var.names)
}
\arguments{
\item{path}{A Directory path within which holds the text files outputted from SizeExtractR imageJ tools and protocol. The directory path should be given in double quotes in the same format as that returned by the function getwd()}

\item{var.names}{A vector object of variable names which relate to the directory structure of the directory given by the 'path' argument. This vector should be set using SizeExtractR::CheckSet_DirecVars VEctor length should be equal to the number of subdirectory levels within the directory given by the 'path' paramter.}
}
\value{
Returns a dataframe object of all regions of interest (ROIs) from all text files within the directory given by the 'path' argument. Directory strings are recoded into dataframe variables with title referring to the 'var.names' parameter.
}
\description{
Build the Uncalibrated Dataset for further use in SizeExctractR
}
\examples{
# load in the output of CheckSet_DirecVars
mypath = paste0(.libPaths()[1],"/SizeExtractR/TextFiles")
print(mypath)
data(varnames)

# Run the function
Database = Build_Uncalibrated_Dataset(mypath, varnames)
head(Database)

}
