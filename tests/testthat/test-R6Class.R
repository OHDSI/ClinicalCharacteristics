library(testthat)
testthat::local_edition(3)

# test creating a TableShell object
test_that("TableShell object creates", {
  # create a TableShell object
  tbl <- TableShell$new()
  expect_true(inherits(tbl, "TableShell"), info = "tbl should be a TableShell object")
})

test_that("getTitle works", {
  # create a TableShell object
  tbl <- TableShell$new()
  # set the title
  tbl$setTitle("My Title")
  # get the title
  title <- tbl$getTitle()
  expect_equal(title, "My Title", info = "title should be 'My Title'")
})

test_that("addTargetCohorts and getTargetCohorts work", {
  # create a TableShell object
  tbl <- TableShell$new()
  # create a TargetCohort object
  tc <- TargetCohort$new()
  # add the TargetCohort object to the TableShell object
  tbl$setTargetCohorts(c(tc))
  # get the target cohorts
  tcs <- tbl$getTargetCohorts()
  expect_true(inherits(tcs[[1]], "TargetCohort"), info = "tcs should be a list of TargetCohort objects")
})

test_that("addSections works", {
  # create a TableShell object
  tbl <- TableShell$new()
  # create a Section object
  sec <- Section$new()
  # add the Section object to the TableShell object
  tbl$setSections(c(sec))
  # get the sections
  secs <- tbl$getSections()
  expect_true(inherits(secs[[1]], "Section"), info = "secs should be a list of Section objects")
})

test_that("setExecutionSettings works", {
  # create a TableShell object
  tbl <- TableShell$new()
  # create an ExecutionSettings object
  es <- ExecutionSettings$new(connectionDetails = "fake_connection_details",
                connection = NULL,
                cdmDatabaseSchema = "fake_cdm_database_schema",
                workDatabaseSchema = "fake_work_database_schema",
                tempEmulationSchema = "fake_temp_emulation_schema",
                targetCohortTable = "fake_target_cohort_table",
                cdmSourceName = "fake_cdm_source_name",
                numThreads = 4)
  # set the ExecutionSettings object to the TableShell object
  tbl$setExecutionSettings(es)
  # get the execution settings
  es <- tbl$getExecutionSettings()
  expect_true(inherits(es, "ExecutionSettings"), info = "es should be an ExecutionSettings object")
})

test_that("TargetCohort object creates", {
  # create a TargetCohort object
  tc <- TargetCohort$new()
  expect_true(inherits(tc, "TargetCohort"), info = "tc should be a TargetCohort object")
})

test_that("TargetCohort class tests - setId and getId", {
  tc <- TargetCohort$new()

  # Test setId and getId methods
  tc$setId(1)
  expect_equal(tc$getId(), 1)
})

test_that("TargetCohort class tests - setName and getName", {
  tc <- TargetCohort$new()

  # Test setName and getName methods
  tc$setName("Test Name")
  expect_equal(tc$getName(), "Test Name")
})

test_that("Section object creates", {
  # create a Section object
  sec <- Section$new()
  expect_true(inherits(sec, "Section"), info = "sec should be a Section object")
})

test_that("Section class tests - setTitle and getTitle", {
  section <- Section$new()

  # Test setTitle and getTitle methods
  section$setTitle("Test Title")
  expect_equal(section$getTitle(), "Test Title")
})

test_that("Section class tests - setOrdinal and getOrdinal", {
  section <- Section$new()

  # Test setOrdinal and getOrdinal methods
  section$setOrdinal(1)
  expect_equal(section$getOrdinal(), 1)
})

test_that("Section class tests - setLineItems and getLineItems", {
  section <- Section$new()

  # Test setLineItems and getLineItems methods
  lineItems <- list(LineItem$new(), LineItem$new())
  section$setLineItems(lineItems)
  expect_equal(section$getLineItems(), lineItems)
})

test_that("ExecutionSettings object creates", {
  # create an ExecutionSettings object
  es <- ExecutionSettings$new(connectionDetails = "fake_connection_details",
                              connection = NULL,
                              cdmDatabaseSchema = "fake_cdm_database_schema",
                              workDatabaseSchema = "fake_work_database_schema",
                              tempEmulationSchema = "fake_temp_emulation_schema",
                              targetCohortTable = "fake_target_cohort_table",
                              cdmSourceName = "fake_cdm_source_name",
                              numThreads = 4)
  expect_true(inherits(es, "ExecutionSettings"), info = "es should be an ExecutionSettings object")
})

test_that("LineItem class tests - setOrdinal and getOrdinal", {
  lineItem <- LineItem$new()

  # Test setOrdinal and getOrdinal methods
  lineItem$setOrdinal(1)
  expect_equal(lineItem$getOrdinal(), 1)
})

test_that("LineItem class tests - setLabel", {
  lineItem <- LineItem$new()

  # Test setLabel method
  lineItem$setLabel("Test Label")
  expect_equal(lineItem$getLabel(), "Test Label")
})

test_that("LineItem class tests - setShowMissing and getShowMissing", {
  lineItem <- LineItem$new()

  # Test setShowMissing and getShowMissing methods
  lineItem$setShowMissing(TRUE)
  expect_equal(lineItem$getShowMissing(), TRUE)
})

# test_that("LineItem class tests - setStatisticType and getStatisticType", {
#   lineItem <- LineItem$new()
#
#   # Test setStatisticType and getStatisticType methods
#   lineItem$setStatisticType("Test Statistic Type")
#   expect_equal(lineItem$getStatisticType(), "Test Statistic Type")
# })
#
# test_that("LineItem class tests - setLimit and getLimit", {
#   lineItem <- LineItem$new()
#
#   # Test setLimit and getLimit methods
#   lineItem$setLimit("Test Limit")
#   expect_equal(lineItem$getLimit(), "Test Limit")
# })

# test_that("LineItem class tests - setTimeWindows and getTimeWindows", {
#   lineItem <- LineItem$new()
#
#   # Test setTimeWindows and getTimeWindows methods
#   timeWindows <- TimeWindows$new()
#   lineItem$setTimeWindows(timeWindows)
#   expect_equal(lineItem$getTimeWindows(), timeWindows)
# })
