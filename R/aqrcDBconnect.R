#' Function to connect to the AQRC databases
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
#'   something like "aqrc-sql", "aqrc-sql-test", and "aqrc-sql-appdev"
#'   (default is the production database server "aqrc-sql").
#'
#' @return A pool connection to the database defined by the parameters.
#'
#' @details This function will open a database connection via the \code{pool}
#'   package. Because this is opening pool connections to the database, it is
#'   best practice to ensure these connections are closed after database
#'   operations are finished. For details of the \code{pool} package see its
#'   website at
#'   \href{https://github.com/rstudio/pool}{https://github.com/rstudio/pool}.
#'
#' @import odbc
#'
#' @import pool
#'
#' @examples
#' \dontrun{
#' poolIMP <- aqrcDBconnect("IMPROVE", "aqrc-sql-test")
#' poolCSN <- aqrcDBconnect("CSN", "aqrc-test")
#' # Perform some database operations with the connection.
#' # Once complete, close the pool connection.
#' pool::poolClose(poolIMP); rm(poolIMP)
#' pool::poolClose(poolCSN); rm(poolCSN)
#' }
#'
#' @export
#' 

aqrcDBconnect <- function(network=c("IMPROVE","CSN"),
                          server){
    
    if(tolower(network)=="csn"){
        # Create connection to CSN_1.0 database.
        pool <- pool::dbPool(
                drv=odbc::odbc(),
                Driver='SQL Server',
                Server=server,
                Port=1433L,
                Database="CSN_1.0"
            )
    }
    
    if(tolower(network)=="improve"){
        # Create connection to Improve_2.1 database.
        pool <- pool::dbPool(
            drv=odbc::odbc(),
            Driver='SQL Server',
            Server=server,
            Port=1433L,
            Database="Improve_2.1"
        )
    }
    
    if(!(tolower(network) %in% c("improve","csn"))) {
        stop("Network unknown.")
    }
    
    return(pool)
    
}

#' Function to disconnect from any pool connection and cleanup the connection
#'
#' This function will disconnect from any pool connection and remove the
#' connection object from the R environment.
#'
#' @param poolConn pool object which represents an active connection.
#'
#' @param pos inherited from rm(). The numeric position of the environment from
#'   which to remove the object, or a character string naming the environment
#'   (parsed with as.environment()).
#'
#' @return Nothing.
#'
#' @details This function will disconnect a database connection via the
#'   \code{pool} package. After disconnecting it will remove the connection
#'   object from the R environment.
#'   
#' @seealso \code{\link{poolClose}}
#' 
#' @seealso \code{\link{rm}}
#'
#' @import pool
#'
#' @examples
#' \dontrun{
#' # poolIMP is a database connection
#' aqrcDBdisconnect(poolIMP)
#' # the connection is closed and the object, poolIMP, is removed.
#' }
#'
#' @export
#' 

aqrcDBdisconnect <- function(poolConn, pos=NULL) {
    pool::poolClose(poolConn)
    if(is.null(pos)){
        rm(list=deparse(substitute(poolConn)), pos=".GlobalEnv")
    } else {
        rm(list=deparse(substitute(poolConn)), pos=pos)
    }
    
}