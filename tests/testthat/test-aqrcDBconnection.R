context("Connects to database")

suppressWarnings({
    library(DBI)
    library(dbplyr)
    library(pool)
})

# Set relative path to config files (working directory for tests is 'testthat')
imp_config <- file.path("../../../config/config.yml")
csn_config <- "../../../config/config_test.yml"

test_that("DB connection fails if config file is not provided",{
    expect_error(aqrcDBconnect("IMPROVE",config_file=NULL))
})

test_that("DB connection fails if path to config file is bad",{
    expect_error(aqrcDBconnect("CSN",config_file="bad/path/config.yml"))
})

test_that("DB connection creates a pool object", {
    impDBconn <- aqrcDBconnect("IMPROVE",config_file=imp_config)
    csnDBconn <- aqrcDBconnect("CSN",config_file=csn_config)
    
    expect_equal(class(impDBconn)[1],"Pool")
    
    expect_equal(class(csnDBconn)[1],"Pool")
    
    poolClose(impDBconn)
    poolClose(csnDBconn)
})

test_that("A connecton to the Microsoft SQL Server is made", {
    impDBconn <- aqrcDBconnect("IMPROVE",config_file=imp_config)
    csnDBconn <- aqrcDBconnect("CSN",config_file=csn_config)
    
    expect_output(show(impDBconn),'Microsoft SQL Server')
    
    expect_output(show(csnDBconn),'Microsoft SQL Server')
    
    poolClose(impDBconn)
    poolClose(csnDBconn)
})

test_that("A connection to the database is made", {
    testSQL <- sql("SELECT DB_Name() AS DB_Name")
    impDBconn <- aqrcDBconnect("IMPROVE",config_file=imp_config)
    csnDBconn <- aqrcDBconnect("CSN",config_file=csn_config)
    
    imp <- DBI::dbGetQuery(impDBconn,testSQL)
    csn <- DBI::dbGetQuery(csnDBconn,testSQL)
    
    expect_true(grepl('Improve',imp$DB_Name))
    expect_true(grepl('CSN',csn$DB_Name))
    
    poolClose(impDBconn)
    poolClose(csnDBconn)
})