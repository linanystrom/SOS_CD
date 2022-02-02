################################################################################

# SoS-CD -- Sequential analysis planning

################################################################################

# Load necessary packages ------------------------------------------------------

packages <- c("gsDesign", "pwr")

lapply(packages, library, character.only = TRUE)

# Analysis ---------------------------------------------------------------------

sos_gs <- gsDesign(
  k = 2,         # Two test-points
  test.type = 2, 
  alpha = .05,   # One-tailed alpha level
  beta = .20,    # 80% power
  sfupar = .25,  # As per recommendations of Weigl & Ponocny (2020)
  sfu = "WT"     # Wang-Tsiatis approach
)

sos_power <- pwr.t.test(
  d           = .35,
  power       = .80,
  alternative = "greater"
)

gs_samples <- sos_power$n * sos_gs$n.I
