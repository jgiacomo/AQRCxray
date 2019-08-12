#' Function to connect to the database
#' 
#' This function will connect to the AQRC databases (IMPROVE or CSN) and pull 
#' data from multiple tables that are useful for XRF data analysis. The database
#' connection is done with the help of a config file which is stored in the 
#' ../config/config.yml file. See the documentation for the \code{config} 
#' package for details.
#' 
#' @param network Character string of the network to connect to (IMPROVE or 
#'   CSN).
#'   
#' @param server Character string of the server to connect to. Typically 
#'   something like "production", "development", and "test" (default is 
#'   "production").
#'   
#' @param config_file Character string of the relative path to the config file
#'   (default = \code{../config/config.yml})
#'   
#' @return A pool connection to the database defined by the parameters.
#'   
#' @details This function will open a database connection via the \code{pool} 
#'   package. It uses the \code{config} package to place the connection 
#'   configuration into a separate file that can be read in to make it easier to
#'   distribute between desktop sessions and deployed server sessions (which may
#'   have different connection parameters). Also, because this is opening pool 
#'   connections to the database, it is best practice to ensure these 
#'   connections are closed after database operations are finished. Also, while 
#'   the database servers are being changed over (a new database server was 
#'   installed) the IMPROVE and CSN are on different servers. To connect to CSN 
#'   database you need to use the \code{config_test.yml} file and to connecto to
#'   IMPROVE you need to use the \code{config.yml} file.
#'   
#' @examples
#' poolIMP <- xrayDBconnect("IMPROVE", "../config/config.yml")
#' poolCSN <- xrayDBconnect("CSN", "../config/config_test.yml")
#' # Perform some database operations with the connection.
#' # Once complete, close the pool connection.
#' pool::poolClose(poolIMP)
#' pool::poolClose(poolCSN)
#' 
#' @export
#' 

aqrcDBconnect <- function(network=c("IMPROVE","CSN"),
                          server="production",
                          config_file="../config/config.yml"){
    
    config_set <- paste(server, .Platform$OS.type, sep="_")
    
    db <- config::get(file=config_file,value=config_set)
    
    if(tolower(network)=="csn"){
        # Check for OS type and create connection accordingly.
        if(.Platform$OS.type == "windows"){
            pool <- pool::dbPool(
                drv=odbc::odbc(),
                Driver=db$driver,
                Server=db$server,
                Port=db$port,
                Database="CSN_1.0"
            )
        } else {
            pool <- pool::dbPool(
                drv=odbc::odbc(),
                Driver=db$driver,
                Server=db$server,
                Port=db$port,
                uid=db$uid,
                Database="CSN_1.0",
                pwd=db$pwd,
                TDS_Version=db$TDS_Version,
                UseNTLMv2=db$UseNTLMv2
            )
        }
    }
    
    if(tolower(network)=="improve"){
        # Check for OS type and create connection accordingly.
        if(.Platform$OS.type == "windows"){
            pool <- pool::dbPool(
                drv=odbc::odbc(),
                Driver=db$driver,
                Server=db$server,
                Port=db$port,
                Database="Improve_2.1"
            )
        } else {
            pool <- pool::dbPool(
                drv=odbc::odbc(),
                Driver=db$driver,
                Server=db$server,
                Port=db$port,
                Database="Improve_2.1",
                uid=db$uid,
                pwd=db$pwd,
                TDS_Version=db$TDS_Version,
                UseNTLMv2=db$UseNTLMv2
            )
        }
    }
    
    if(!(tolower(network) %in% c("improve","csn"))) {
        stop("Network unknown.")
    }
    
    return(pool)
    
}