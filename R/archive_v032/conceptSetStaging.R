# -- Step 1:
#   -- create a staging concept set table
# -- for each concept set, in parallel, insert resolved concept sets to it
# -- final - join back to concept to get domain_ids
# -- select a data frame of concept set ids and domains
#
# -- Step 2:
#   -- using this df, we can then arrange the template to give us all necessary domains unioned for that concept set
# --




.createConceptSetStagingTable <- function(connection,
                                          workDatabaseSchema) {
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "createConceptSetStagingTable.sql",
                                           packageName = pkgload::pkg_name(),
                                           workDatabaseSchema = workDatabaseSchema)
  DatabaseConnector::executeSql(connection = connection, sql = sql)
}

.getConceptSetStagingInsert <- function(connection,
                                        executionSettings,
                                        jsonPath) {

  rawJson <- readChar(con = jsonPath, nchars = file.info(jsonPath)$size)
  sql <- CirceR::buildConceptSetQuery(conceptSetJSON = rawJson)
  sql <- SqlRender::render(sql = sql,
                           vocabulary_database_schema = cdmDatabaseSchema)

  sql <- SqlRender::translate(sql = sql, targetDialect = connection@dbms)
  result <- DatabaseConnector::querySql(connection = connection, sql = sql, snakeCaseToCamelCase = TRUE)

  return (result$conceptId)
}
