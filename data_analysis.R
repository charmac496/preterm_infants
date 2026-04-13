# =============================================================================
# data_analysis.R
# Description: Main analysis script for the study on the effects of DXMS
#              (prenatal corticosteroids) on neurodevelopment in preterm infants.
#              Performs univariate and multivariate analyses for three
#              gestational age groups (all, <=34 weeks, >34 weeks) and then
#              runs mediation analyses.
# Dependencies: functions.R (provides uni_test, mul_test, run_mediation_analysis)
# =============================================================================

# -----------------------------------------------------------------------------
# Analysis for full sample (all gestational ages)
# -----------------------------------------------------------------------------

# Univariate analysis for all outcome variables (dep_g) using all candidate
# predictors (var_gl). Significance threshold = 0.1.
gri_DXMS_all_uni <- uni_test(data_gl, var_gl, dep_g, 0.1)

# Multivariate analysis: use predictors that were significant in univariate
# tests, keep "DXMS" forced in the model, significance threshold = 0.3 for
# LASSO/p-value filtering.
gri_DXMS_all_mul <- mul_test(data_gl, gri_DXMS_all_uni$significant, 0.3, c("DXMS"))

# Univariate analysis for population/demographic variables (pop_var) only.
gri_DXMS_pop_uni <- uni_test(data_gl, pop_var, dep_g, 0.1)

# Multivariate analysis restricted to significant demographic predictors.
gri_DXMS_pop_mul <- mul_test(data_gl, gri_DXMS_pop_uni$significant, 0.3, c("DXMS"))

# -----------------------------------------------------------------------------
# Analysis for early preterm subgroup (gestational age <= 34 weeks)
# -----------------------------------------------------------------------------

# Univariate analysis for all predictors in the early preterm subgroup.
gri_DXMS_all_uni_down <- uni_test(data_gl_down, var_gl_down, down_g, 0.1)

# Multivariate analysis for the early preterm subgroup.
gri_DXMS_all_mul_down <- mul_test(data_gl_down, gri_DXMS_all_uni_down$significant, 0.3, c("DXMS"))

# Univariate analysis for demographic variables in the early preterm subgroup.
gri_DXMS_pop_uni_down <- uni_test(data_gl_down, pop_var_down, down_g, 0.1)

# Multivariate analysis for demographics in the early preterm subgroup.
gri_DXMS_pop_mul_down <- mul_test(data_gl_down, gri_DXMS_pop_uni_down$significant, 0.3, c("DXMS"))

# -----------------------------------------------------------------------------
# Analysis for late preterm / term subgroup (gestational age > 34 weeks)
# -----------------------------------------------------------------------------

# Univariate analysis for all predictors in the late preterm/term subgroup.
gri_DXMS_all_uni_up <- uni_test(data_gl_up, var_gl_up, up_g, 0.1)

# Multivariate analysis for the late preterm/term subgroup.
gri_DXMS_all_mul_up <- mul_test(data_gl_up, gri_DXMS_all_uni_up$significant, 0.3, c("DXMS"))

# Univariate analysis for demographic variables in the late preterm/term subgroup.
gri_DXMS_pop_uni_up <- uni_test(data_gl_up, pop_var_up, up_g, 0.1)

# Multivariate analysis for demographics in the late preterm/term subgroup.
gri_DXMS_pop_mul_up <- mul_test(data_gl_up, gri_DXMS_pop_uni_up$significant, 0.3, c("DXMS"))

# -----------------------------------------------------------------------------
# Mediation analyses for the full sample and subgroups
# -----------------------------------------------------------------------------

# Mediation analysis for the full sample using the multivariate demographic model
# (gri_DXMS_pop_mul) as the outcome model. Mediators are specified in var_mid.
# The treatment variable is "DXMS". Bootstrap is used for inference (TRUE).
med_ans <- run_mediation_analysis(gri_DXMS_pop_mul, data_gl, pop_var, var_mid, "DXMS", TRUE)

# Mediation analysis for the early preterm subgroup.
med_ans_down <- run_mediation_analysis(gri_DXMS_pop_mul_down, data_gl_down, pop_var_down, var_mid_down, "DXMS", TRUE)

# Mediation analysis for the late preterm/term subgroup.
med_ans_up <- run_mediation_analysis(gri_DXMS_pop_mul_up, data_gl_up, pop_var_up, var_mid_up, "DXMS", TRUE)