#' Function to get XRF analysis data from Excel.
#' 
#' This function will return a tidy data frame from Panalytical E5 XRF
#' instrument results which are stored in an Excel worksheet.
#' @param file The path and file name of the Excel workbook containing the data.
#' @param XRF The name of the XRF instrument (e.g. Froya, Thor).
#' @param ... Any other parameters which will passed directly to read_excel().
#' @import dplyr
#' @import readxl
#' @import tidyr
#' @export
#' @examples
#' \dontrun{
#' readXRF_excel("pathTo/excel/file","Odin")
#' }
#' 

readXRF_excel <- function(file, XRF, ...){
    
    application <- read_excel(file,range="A2",col_names=FALSE, ...) %>%
        pull() %>%
        gsub('(.*)( -.*)','\\1',.)  # remove " -" at end of app name
    
    suffix <- c("time","RawIntensity","Icorr","ArealDensity")
    
    df.names <- readxl::read_excel(file,skip=4,n_max=1,col_names=FALSE, ...)
    df.names <- paste(df.names,suffix,sep = ".")
    df.names <- c("SampleId","XRFDate","ResultType",df.names)
    
    data <- readxl::read_excel(file,skip=6,col_names=FALSE, ...)
    names(data) <- df.names
    
    data <- data %>%
        select(-ResultType) %>%
        tidyr::gather(key=type,value=value,-SampleId,-XRFDate) %>%
        tidyr::separate(type,c("Parameter","Unit")) %>%
        tidyr::spread(key=Unit,value=value) %>%
        mutate(DeviceName=XRF,Application=application) %>%
        select(DeviceName,Application,SampleId,XRFDate,Parameter,time,
               RawIntensity,Icorr,ArealDensity)
    
    return(data)
}