#' Function to find the type concept ids within each CDM domain
#' @description
#' In some databases the type concept id is very important in distinguishing what
#' type of information is available. A domain can have different provenances such
#' as information from claims, NLP or EHR. This function helps list all possible
#' concept types corresponding to an OMOP CDM. You should use this function
#' to identify the correct concept types for different chars.
#' @param connection the connection to the OMOP data
#' @param cdmDatabaseSchema the database schema hosting the OMOP cdm
#' @param domain the domain to assess. Options include: condition_occurrence,
#' drug_exposure, measurement, observation and procedure_occurrence
#' @return this function returns a tibble describing the concept type ids
#' utilized in a specific OMOP domain
#' @export
findConceptType <- function(connection,
                            cdmDatabaseSchema,
                            domain) {

  checkmate::check_choice(domain, choices = c("condition_occurrence", "drug_exposure",
                                              "measurement", "observation",
                                              "procedure_occurrence"))

  dm <- domain_translate(domain)
  concept_type <- dm$concept_type_id
  sql <- glue::glue(
    "WITH T1 AS (
      SELECT {concept_type},
        COUNT({concept_type}) as n
      FROM {cdmDatabaseSchema}.{domain}
      GROUP BY {concept_type}
    )
    SELECT b.concept_id, b.concept_name, a.n
    FROM T1 a
    JOIN {cdmDatabaseSchema}.concept b
    ON a.{concept_type} = b.concept_id
    ;"
  )
  cli::cat_bullet(
    glue::glue("Looking up concept types for domain: {crayon::magenta(domain)}"),
    bullet = "info",
    bullet_col = "blue"
  )

  type_tb <- DatabaseConnector::querySql(connection, sql = sql) |>
    tibble::as_tibble() |>
    dplyr::rename_with(tolower) |>
    dplyr::mutate(
      domain = domain,
      .before = 1
    )

  return(type_tb)

}


concept_type_sql <- function(domain, conceptType) {

  domain_trans <- domain_translate(domain)
  if (!all(is.na(conceptType))) {
    conceptType <- paste(conceptType, collapse = ", ")
    conceptTypeSql <- glue::glue(
      "AND {domain_trans$concept_type_id} IN ({conceptType})"
    )
  } else{
    conceptTypeSql <- ""
  }

  return(conceptTypeSql)

}
