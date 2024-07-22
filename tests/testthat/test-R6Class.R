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
  tbl$addTargetCohorts(c(tc))
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
  tbl$addSections(c(sec))
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
  tbl$setExectionSettings(es)
  # get the execution settings
  es <- tbl$getExecutionSettings()
  expect_true(inherits(es, "ExecutionSettings"), info = "es should be an ExecutionSettings object")
})

test_that("TargetCohort object creates", {
  # create a TargetCohort object
  tc <- TargetCohort$new()
  expect_true(inherits(tc, "TargetCohort"), info = "tc should be a TargetCohort object")
})

test_that("Section object creates", {
  # create a Section object
  sec <- Section$new()
  expect_true(inherits(sec, "Section"), info = "sec should be a Section object")
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
