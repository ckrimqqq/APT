param_business_group$groups <- c(
  "GOO001.0_GoogleMainPage"			= "Main",

  "Total" 						= "Total"
  )

.p90_sla <- list(
  "Main"				= 1,

  "Total"			= 3

  )

param_sla$report.summary <- list(
    define_sla("txn.p90", .p90_sla, critical = TRUE),
    define_sla(
        "txn.errate",
        list(
            "Main" = 0.01,
            "Total" = 0.01
        ),
        critical = TRUE
    )
)

param_sla$report.percentiles <- list(
    define_sla("perc.p0.9", .p90_sla, critical = TRUE)
)

