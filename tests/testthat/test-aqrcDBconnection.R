context("Connects to database")

suppressWarnings({
    library(DBI)
    library(dbplyr)
    library(pool)
})

test_that("DB connection fails if incorrect network is provided",{
    expect_error(aqrcDBconnect("IMP","aqrc-sql"))
})

test_that("DB connection fails if incorrect server is provided",{
    expect_error(aqrcDBconnect("CSN","aqrc-wrong"))
})

test_that("DB connection to production creates a pool object", {
    impDBconn <- aqrcDBconnect("IMPROVE","aqrc-sql")
    csnDBconn <- aqrcDBconnect("CSN","aqrc-sql")
    
    expect_equal(class(impDBconn)[1],"Pool")
    
    expect_equal(class(csnDBconn)[1],"Pool")
    
    poolClose(impDBconn)
    poolClose(csnDBconn)
})

test_that("DB connection to test creates a pool object", {
    impDBconn <- aqrcDBconnect("IMPROVE","aqrc-sql-test")
    csnDBconn <- aqrcDBconnect("CSN","aqrc-sql-test")
    
    expect_equal(class(impDBconn)[1],"Pool")
    
    expect_equal(class(csnDBconn)[1],"Pool")
    
    poolClose(impDBconn)
    poolClose(csnDBconn)
})

test_that("DB connection to development creates a pool object", {
    impDBconn <- aqrcDBconnect("IMPROVE","aqrc-sql-appdev")
    csnDBconn <- aqrcDBconnect("CSN","aqrc-sql-appdev")
    
    expect_equal(class(impDBconn)[1],"Pool")
    
    expect_equal(class(csnDBconn)[1],"Pool")
    
    poolClose(impDBconn)
    poolClose(csnDBconn)
})

test_that("A connecton to the Microsoft SQL Server is made", {
    impDBconn <- aqrcDBconnect("IMPROVE","aqrc-sql")
    csnDBconn <- aqrcDBconnect("CSN","aqrc-sql")
    
    expect_output(show(impDBconn),'Microsoft SQL Server')
    
    expect_output(show(csnDBconn),'Microsoft SQL Server')
    
    poolClose(impDBconn)
    poolClose(csnDBconn)
})

test_that("A connection to the database is made", {
    testSQL <- sql("SELECT DB_Name() AS DB_Name")
    impDBconn <- aqrcDBconnect("IMPROVE","aqrc-sql")
    csnDBconn <- aqrcDBconnect("CSN","aqrc-sql")
    
    imp <- DBI::dbGetQuery(impDBconn,testSQL)
    csn <- DBI::dbGetQuery(csnDBconn,testSQL)
    
    expect_true(grepl('Improve',imp$DB_Name))
    expect_true(grepl('CSN',csn$DB_Name))
    
    poolClose(impDBconn)
    poolClose(csnDBconn)
})
