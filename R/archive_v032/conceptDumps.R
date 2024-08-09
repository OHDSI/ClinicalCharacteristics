#' Charlson concept set
#' @export
charlsonConcepts <- function() {
  conceptSet <- list(
    Capr::cs(Capr::descendants(4329847), name = "Acute Myocardial Infarction"),
    Capr::cs(Capr::descendants(316139), name = "Congestive Heart Failure"),
    Capr::cs(Capr::descendants(321052), name = "Peripheral Vascular Disease"),
    Capr::cs(Capr::descendants(381591, 434056), name = "Cerebrovascular Disease"),
    Capr::cs(Capr::descendants(4182210), name = "Dementia"),
    Capr::cs(Capr::descendants(4063381), name = "Chronic Pulmonary Disease"),
    Capr::cs(Capr::descendants(257628, 134442, 80800, 80809, 256197, 255348), name = "Rheumatologic Disease"),
    Capr::cs(Capr::descendants(4247120), name = "Peptic ulcer disease"),
    Capr::cs(Capr::descendants(4064161, 4212540), name = "Mild liver disease"),
    Capr::cs(Capr::descendants(201820), name = "Diabetes (mild to moderate)"),
    Capr::cs(Capr::descendants(443767, 442793), name = "Diabetes with chronic complications"),
    Capr::cs(Capr::descendants(192606, 374022), name = "Hemoplegia or paralegia"),
    Capr::cs(Capr::descendants(4030518), name = "Renal disease"),
    Capr::cs(Capr::descendants(443392), name = "Any malignancy"),
    Capr::cs(Capr::descendants(4245975, 4029488, 192680, 24966), name = "Moderate to severe liver disease"),
    Capr::cs(Capr::descendants(432851), name = "Metastatic solid tumor"),
    Capr::cs(Capr::descendants(439727), name = "AIDS")
  )
  return(conceptSet)
}

#' Atc concept set
#' @export
atcConcepts <- function() {

  conceptSet <- list(
    Capr::cs(Capr::descendants(21600046), name = "Drugs for acid related disorders"),
    Capr::cs(Capr::descendants(21600712), name = "Drugs used in diabetes"),
    Capr::cs(Capr::descendants(21600960), name = "Antithrombotic agents"),
    Capr::cs(Capr::descendants(21601461), name = "Diuretics"),
    Capr::cs(Capr::descendants(21601664), name = "Beta blocking agents"),
    Capr::cs(Capr::descendants(21601744), name = "Calcium channel blockers"),
    Capr::cs(Capr::descendants(21601782), name = "Agents acting on the renin-angiotensin system"),
    Capr::cs(Capr::descendants(21601853), name = "Lipid modifying agents"),
    Capr::cs(Capr::descendants(21602028), name = "Antipsoriatics"),
    Capr::cs(Capr::descendants(21602796), name = "Antibacterials for systemic use"),
    Capr::cs(Capr::descendants(21601387), name = "Antineoplastic agents"),
    Capr::cs(Capr::descendants(21603890), name = "Immunosuppressants"),
    Capr::cs(Capr::descendants(21603932), name = "Antiinflammatory and antirheumatic products"),
    Capr::cs(Capr::descendants(21604254), name = "Opioids"),
    Capr::cs(Capr::descendants(21604389), name = "Antiepilieptics"),
    Capr::cs(Capr::descendants(21604489), name = "Psycholeptics"),
    Capr::cs(Capr::descendants(21604686), name = "Antidepressants"),
    Capr::cs(Capr::descendants(21604752), name = "Psychostimulants, agents used for ADHD and nootropics"),
    Capr::cs(Capr::descendants(21603248), name = "Drugs for obstructive airway diseases")
  )
  return(conceptSet)

}

#' Standard visit concept set
#' @export
standardVisitConcepts <- function() {

  conceptSet <- list(
    Capr::cs(Capr::descendants(232,9201), name = "Inpatient"),
    Capr::cs(Capr::descendants(9202), name = "Outpatient"),
    Capr::cs(Capr::descendants(9203), name = "ER")
  )

  return(conceptSet)

}
