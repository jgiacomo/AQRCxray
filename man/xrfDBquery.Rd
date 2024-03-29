% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xrfDBquery.R
\name{xrfDBquery}
\alias{xrfDBquery}
\title{Function to get XRF specific data from the database.}
\usage{
xrfDBquery(
  conn,
  network = c("IMPROVE", "CSN"),
  minDate = "2019-01-01",
  maxDate = as.character(Sys.Date()),
  site = NULL,
  sampleDate = NULL,
  minSampleDate = NULL,
  maxSampleDate = NULL,
  filterId = NULL,
  barcodeId = NULL,
  trayFileSampleId = NULL,
  includeQC = c("yes", "no", "only"),
  valid = TRUE,
  elements = "all",
  showQuery = FALSE
)
}
\arguments{
\item{conn}{connection object linked to the IMPROVE or CSN database}

\item{network}{character string for the network, \code{IMPROVE} or \code{CSN}}

\item{minDate}{character string of the minimum XRF analysis date (inclusive)}

\item{maxDate}{character string of the maximum XRF analysis date (inclusive)}

\item{site}{character string of the site name to query. For IMPROVE a 
character vector can be used to query multiple sites.}

\item{sampleDate}{character string in the format \code{YYYY-mm-dd}. This will
query the database for a filter with the specified sample date. For CSN the
sample date will be the 'IntendedUseDate'. Supercedes \code{minSampleDate} 
and \code{maxSampleDate}.}

\item{minSampleDate}{character string of the minimum sample date to query. 
For CSN the sample date is the 'IntendedUseDate'. If a sampleDate is 
entered this argument will be ignored.}

\item{maxSampleDate}{character string of the maximum sample date to query. 
For CSN the sample date is the 'IntendedUseDate'. If a sampleDate is 
entered this argument will be ignored.}

\item{filterId}{numeric vector of Filter IDs to query.}

\item{barcodeId}{character vector of barcodes (ContractorFilterAnalysisId in 
the database). This field doesn't exist in the IMPROVE database, so this 
argument will be ignored if the network = IMPROVE.}

\item{trayFileSampleId}{character vector of TrayFileSampleIdent's to query.}

\item{includeQC}{character string. \code{yes} indicates QC samples will be 
included with network samples in the query. \code{no} will exclude QC 
samples from the query results. \code{only} will only include QC samples in
the query results.}

\item{valid}{boolean. \code{TRUE} will only include valid results in the 
query results. \code{FALSE} will return both valid and invalid results.}

\item{elements}{character vector of elements to include in the results (e.g. 
c("Al", "Si", "Ca", "Fe")). \code{all} will include all elements in the 
results.}

\item{showQuery}{boolean. If \code{FALSE} (the default), the data frame
created from the database query is returned. If \code{TRUE}, then the SQL
query text generated by dbplyr is returned. This is mostly for debugging
information.}
}
\value{
data frame of the query results from the database.
}
\description{
This function will perform a query on the database to get XRF analysis 
results. The query can be filtered using the arguments of the function to 
reduce the size of the data obtained from the database. Further filtering of 
the returned data frame can be done in R. The function uses an open database 
connection to perform a query and stores the result of the query in a data 
frame.
}
\details{
Use of this function requires some knowledge of the database 
  structure for the AQRC databases. The queries produced by this function are
  specfically for obtaining results from the XRF instruments (i.e. no HIPS, 
  qCarbon, or ions data can be queried using this function). The most common 
  search criteria were included in the list of function arguments, however, 
  if further filtering of the database results is required, this can easily 
  be done in the R environment with the returned data frame. The elements 
  over which you can filter is created by the available elements using the 
  other filtering criteria in your search, therefore, it could change based 
  on the dates you are searching over.
}
\examples{
\dontrun{
# A query of the crustal element results from samples analyzed in the month
# of May, 2019 from the IMPROVE database.
df <- xrfDBquery(poolConn, network="IMPROVE", minDate="2019-05-01",
                 maxDate="2019-05-31", elements=c("Al","Si","Ca","Fe"))
                 
# Another query of all valid sample results (no QC samples) sampled in
# September, 2017 in the CSN network.
df <- xrfDBquery(poolConn, network="CSN", minSampleDate="2017-09-01",
                 maxSampleDate="2017-09-30", includeQC="no")
                 
# To include both valid and invalid results add \code{valid=FALSE}
df <- xrfDBquery(poolConn, network="CSN", minSampleDate="2017-09-01",
                 maxSampleDate="2017-09-30", includeQC="no", valid=FALSE)
                 
# For the CSN network specific filter barcodes can be searched.
df <- xrfDBquery(poolConn, network="CSN",
                 barcodeId=c("F164145","F162513","F163396"))
                 
# Both networks can be searched by the TrayFileSampleIdent.
df <- xrfDBquery(poolConn, network="IMPROVE",
                 trayFileSampleId=c("BIBE1", "SIME1"),
                 sampleDate="2018-11-19")
                 
# Additional filtering can be done via normal R procedures. For instance, to
# get the IMPROVE results for May, 2018 from just Froya add filtering after
# the function call.
df <- xrfDBquery(poolConn, network="IMPROVE", minSampleDate="2018-05-01",
                 maxSampleDate="2018-05-31", includeQC="no")
froya_results <- df[df$DeviceName=="Froya",]
}

}
