### This script provides the syntax for different check() options

# _R_CHECK_CRAN_INCOMING_: Check whether package is suitable for publication on CRAN. Default: false, except for CRAN submission checks.

# _R_CHECK_LIMIT_CORES_: If set, check the usage of too many cores in package parallel. If set to ‘warn’ gives a warning, to ‘false’ or ‘FALSE’ the check is skipped, and any other non-empty value gives an error when more than 2 children are spawned. Default: unset (but ‘TRUE’ for CRAN submission checks).
Sys.setenv("--_R_CHECK_LIMIT_CORES_" = FALSE)


# To resume check where it left after encountering an error
check(restore_output = TRUE)


