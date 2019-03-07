#' Function to get XRF analysis data from Excel.
#' 
#' This function will return a tidy data frame from Panalytical E5 XRF
#' instrument results which are stored in an Excel worksheet.
#' @param file The path and file name of the Excel workbook containing the data.
#' @param XRF The name of the XRF instrument (e.g. Froya, Thor).
#' @param ... Any other parameters which will passed directly to read_excel().
#' @export
#' @examples
#' readXRF_excel()

readXRF_excel <- function(file, XRF, ...){
    library(dplyr)
    library(tidyr)
    library(readxl)
    
    application <- read_excel(file,range="A2",col_names=FALSE, ...) %>%
        pull() %>%
        gsub('(.*)( -.*)','\\1',.)  # remove " -" at end of app name
    
    suffix <- c("time","RawIntensity","Icorr","ArealDensity")
    
    df.names <- read_excel(file,skip=4,n_max=1,col_names=FALSE, ...)
    df.names <- paste(df.names,suffix,sep = ".")
    df.names <- c("SampleId","XRFDate","ResultType",df.names)
    
    data <- read_excel(file,skip=6,col_names=FALSE, ...)
    names(data) <- df.names
    
    data <- data %>%
        select(-ResultType) %>%
        gather(key=type,value=value,-SampleId,-XRFDate) %>%
        separate(type,c("Parameter","Unit")) %>%
        spread(key=Unit,value=value) %>%
        mutate(DeviceName=XRF,Application=application) %>%
        select(DeviceName,Application,SampleId,XRFDate,Parameter,time,
               RawIntensity,Icorr,ArealDensity)
    
    return(data)
}