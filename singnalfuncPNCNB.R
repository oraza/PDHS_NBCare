library(dplyr)
library(foreign)
library(labelled)
library(pollster)
library(survey)
library(openxlsx)
pkdhs <- read.dta("//Users//sulailfatima//Desktop//Megasync//DUHS-SPH//MGWR_DHS//PKKR71FL.DTA")
# creating the sampling weight variable. 
pkdhs$wt <- pkdhs$v005/1000000 

## Descriptive
# Recreating PDHS Table 9.14 Content of postnatal care for the newborn
# cord examination
pkdhs <- pkdhs %>%
  filter(v101 != "gb" & v101 != "ajk" & midx == 1 & b19 < 24) %>%
  mutate(
    cord = case_when(
      m78a %in% c("no","don't know") ~ 0,
      m78a == "yes" ~ 1)) %>% 
  set_value_labels(cord = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(cord = "Cord examined")
topline(pkdhs, variable = cord, weight = wt)

# measure temp
pkdhs <- pkdhs %>%
  filter(v101 != "gb" & v101 != "ajk" & midx == 1 & b19 < 24) %>%
  mutate(
    temp = case_when(
      m78b %in% c("no","don't know") ~ 0,
      m78b == "yes" ~ 1)) %>% 
  set_value_labels(temp = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(temp = "Temperature measured")
topline(pkdhs, variable = temp, weight = wt)

# Counseling on danger signs
pkdhs <- pkdhs %>%
  filter(v101 != "gb" & v101 != "ajk" & midx == 1 & b19 < 24) %>%
  mutate(
    dang = case_when(
      m78c %in% c("no","don't know") ~ 0,
      m78c == "yes" ~ 1)) %>% 
  set_value_labels(dang = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dang = "Counselling on danger signs")
topline(pkdhs, variable = dang, weight = wt)

# Counseling on breastfeeding
pkdhs <- pkdhs %>%
  filter(v101 != "gb" & v101 != "ajk" & midx == 1 & b19 < 24) %>%
  mutate(
    c_bf = case_when(
      m78d %in% c("no","don't know") ~ 0,
      m78d == "yes" ~ 1)) %>% 
  set_value_labels(c_bf = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(c_bf = "Counselling on breastfeeding")
topline(pkdhs, variable = c_bf, weight = wt)

# Observing breastfeeding
pkdhs <- pkdhs %>%
  filter(v101 != "gb" & v101 != "ajk" & midx == 1 & b19 < 24) %>%
  mutate(
    obs_bf = case_when(
      m78e %in% c("no","don't know") ~ 0,
      m78e == "yes" ~ 1)) %>% 
  set_value_labels(obs_bf = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(obs_bf = "Observation of breastfeeding")
topline(pkdhs, variable = obs_bf, weight = wt)

# Weighed
pkdhs <- pkdhs %>%
  filter(v101 != "gb" & v101 != "ajk" & midx == 1 & b19 < 24) %>%
  mutate(
    weighed = case_when(
      m19a %in% c("not weighed","don't know") ~ 0,
      m19a %in% c("from written card", "from mother's recall") ~ 1)) %>% 
  set_value_labels(weighed = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(weighed = "Weighed")
topline(pkdhs, variable = weighed, weight = wt)

# Percentage with at least two signal functions performed during the
# first 2 days after birth

pkdhs <- pkdhs %>%
  rowwise() %>%
  mutate(yes_count = sum(cord, temp, dang, c_bf, obs_bf, weighed)) %>%
  mutate(
    signalfunc = case_when(
      yes_count >= 2 ~ 1,
      TRUE ~ 0)) %>%
  set_value_labels(signalfunc = c("At least 2 SFs" = 1, "Less than 2 SFs" = 0)) %>%
  set_variable_labels(signalfunc = "Signal functions for NB-PNC")
topline(pkdhs, variable = signalfunc, weight = wt)

# Not included in the current analysis
# Media & Internet use
# Newspaper
pkdhs <- pkdhs %>% 
  filter(v101 != "gb" & v101 != "ajk" & midx == 1 & b19 < 24) %>%
  mutate(medx_newsp = case_when(
    v157 %in% c("at least once a week", "almost every day") ~ 1,
    v157 %in% c("not at all", "less than once a week") ~ 0,
    is.na(v157) ~ 0)) %>%
  set_value_labels(medx_newsp = c("Yes" = 1, "No" = 0)) %>%
  set_variable_labels(medx_newsp = "Reads a newspaper at least once a week")
topline(pkdhs, variable = medx_newsp, weight = wt)
# TV
pkdhs <- pkdhs %>% 
  filter(v101 != "gb" & v101 != "ajk" & midx == 1 & b19 < 24) %>%
  mutate(medx_tv = case_when(
    v159 %in% c("at least once a week", "almost every day") ~ 1,
    v159 %in% c("not at all", "less than once a week") ~ 0,
    is.na(v159) ~ 0)) %>%
  set_value_labels(medx_tv = c("Yes" = 1, "No" = 0)) %>%
  set_variable_labels(medx_tv = "Watches television at least once a week")
topline(pkdhs, variable = medx_tv, weight = wt)
# Radio
pkdhs <- pkdhs %>% 
  filter(v101 != "gb" & v101 != "ajk" & midx == 1 & b19 < 24) %>%
  mutate(medx_rad = case_when(
    v158 %in% c("at least once a week", "almost every day") ~ 1,
    v158 %in% c("not at all", "less than once a week") ~ 0,
    is.na(v158) ~ 0)) %>%
  set_value_labels(medx_rad = c("Yes" = 1, "No" = 0)) %>%
  set_variable_labels(medx_rad = "Listens to the radio at least once a week")
topline(pkdhs, variable = medx_rad, weight = wt)
# Internet
pkdhs <- pkdhs %>% 
  filter(v101 != "gb" & v101 != "ajk" & midx == 1 & b19 < 24) %>%
  mutate(medx_int = case_when(
    v171a == "yes, last 12 months" | v171a == "yes, before last 12 months" | v171a == "yes, can't establish when" ~ 1,
    v171a == "never" ~ 0)) %>%
  set_value_labels(medx_int = c("Yes" = 1, "No" = 0)) %>%
  set_variable_labels(medx_int = "Ever used the internetk")
topline(pkdhs, variable = medx_int, weight = wt)
# Overall media exposure
pkdhs <- pkdhs %>%
  filter(v101 != "gb" & v101 != "ajk" & midx == 1 & b19 < 24) %>%
  mutate(media_exp = case_when(
    medx_newsp == 1 | medx_tv == 1 | medx_rad == 1 | medx_int == 1 ~ 1,
    TRUE ~ 0)) %>%
  set_value_labels(media_exp = c("Yes" = 1, "No" = 0)) %>%
  set_variable_labels(media_exp = "Media & Internet Exposure")
topline(pkdhs, media_exp, weight = wt)
xtab.mediaexp <- crosstab(pkdhs, media_exp, signalfunc, weight = wt,
                       pct_type = "row", unwt_n = T, format = "wide", n = T)



# Problems in accessing healthcare facility
pkdhs <- pkdhs %>%
  filter(v101 != "gb" & v101 != "ajk" & midx == 1 & b19 < 24) %>%
  mutate(
    v467b_bi = case_when(
      v467b == "big problem" ~ 1,
      TRUE ~ 0)) %>%
  set_value_labels(v467b_bi = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(v467b_bi = "Getting permission to go for treatment")

pkdhs <- pkdhs %>%
  filter(v101 != "gb" & v101 != "ajk" & midx == 1 & b19 < 24) %>%
  mutate(
    v467c_bi = case_when(
      v467c == "big problem" ~ 1,
      TRUE ~ 0)) %>%
  set_value_labels(v467c_bi = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(v467c_bi = "Getting money for treatment")
topline(pkdhs, v467c_bi, weight = wt)

pkdhs <- pkdhs %>%
  filter(v101 != "gb" & v101 != "ajk" & midx == 1 & b19 < 24) %>%
  mutate(
    v467d_bi = case_when(
      v467d == "big problem" ~ 1,
      TRUE ~ 0)) %>%
  set_value_labels(v467d_bi = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(v467d_bi = "Distance to health facility")

pkdhs <- pkdhs %>%
  filter(v101 != "gb" & v101 != "ajk" & midx == 1 & b19 < 24) %>%
  mutate(
    v467f_bi = case_when(
      v467f == "big problem" ~ 1,
      TRUE ~ 0)) %>%
  set_value_labels(v467f_bi = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(v467f_bi = "Not wanting to go alone")

## Percentage with at least one problem in accessing HF
pkdhs <- pkdhs %>%
  rowwise() %>%
  mutate(
    yes_count = sum(v467b_bi, v467c_bi, v467d_bi, v467f_bi)) %>%
  mutate(
    accHF = case_when(yes_count >= 1 ~ 1,
    TRUE ~ 0)) %>%
  set_value_labels(accHF = c("Yes" = 1, "No" = 0)) %>%
  set_variable_labels(accHF = "At least one problem in accessing healthcare
                      facility")
topline(pkdhs, accHF, weight = wt)

# Cross Tabulations with maternal age (m_age_cat), birth order (bord_cat), place
# of delivery (b_HF), education (v106), region (v101), place of residence (v102),
# wealth index (v190), sex of child (b4), anc visits (anc), c-section performed 
# (cs), healthcare access (accHF) & owns a mobile phone (v169a) 
# 
# Mother's age @ birth
pkdhs <- pkdhs %>% 
  mutate(m_age = (b3 - v011)/12) %>% 
  mutate(
    m_age_cat = case_when(
      m_age < 20 ~ 0,
      m_age >= 20 & m_age < 35 ~ 1,
      m_age >= 35 ~ 2)) %>%
  set_value_labels(m_age_cat = c("< 20" = 0, "20 - 34" = 1, "35 - 49" = 2)) %>%
  set_variable_labels(m_age_cat = "Maternal age at birth")
#topline(pkdhs, m_age_cat, wt)
pkdhs$m_age_cat <- as.factor(pkdhs$m_age_cat)
xtab.m_age <- crosstab(pkdhs, m_age_cat, signalfunc, weight = wt,
                       pct_type = "row", unwt_n = T, format = "wide", n = T)
# birth order 
pkdhs <- pkdhs %>% mutate(
  bord_cat = case_when(
    bord == 1 ~ 1,
    bord >= 2 & bord <= 3 ~ 2,
    bord >= 4 & bord <= 5 ~ 3,
    bord >= 6 ~ 4)) %>%
  set_value_labels(bord_cat = c("1" = 1, "2-3" = 2, "4-5" = 3, ">6" = 4)) %>%
  set_variable_labels(bord_cat = "Birth order")
#topline(pkdhs, bord_cat, wt)
xtab.bord_cat <- crosstab(pkdhs, bord_cat, signalfunc, weight = wt,
                          pct_type = "row", unwt_n = T, format = "wide", n = T)
pkdhs$bord_cat <- as.factor(pkdhs$bord_cat)


# place of delivery
pkdhs <- pkdhs %>% 
  mutate(
  b_HF = case_when(
    m15 %in% c("government hospital", "rural health centre/mother child health centre",
               "bhu(basic health unit)","community midwife", "other public sector",
               "private hospital/clinic", "other private medical sector") ~ 1,
    TRUE ~ 0)) %>% 
  set_value_labels(b_HF = c("Institional delivery" = 1, "Elsewhere" = 0)) %>%
  set_variable_labels(b_HF = "Place of delivery")
#topline(pkdhs, b_HF, wt)
xtab.b_HF <- crosstab(pkdhs, b_HF, signalfunc, weight = wt,
                      pct_type = "row", unwt_n = T, format = "wide", n = T)
# education v106
xtab.v106 <- crosstab(pkdhs, v106, signalfunc, weight = wt,
                      pct_type = "row", unwt_n = T, format = "wide", n = T)
# region v101
xtab.v101 <- crosstab(pkdhs, v101, signalfunc, weight = wt,
                      pct_type = "row", unwt_n = T, format = "wide", n = T)
# place of residence v102
xtab.v102 <- crosstab(pkdhs, v102, signalfunc, weight = wt,
                      pct_type = "row", unwt_n = T, format = "wide", n = T)
# wealth index v190
xtab.v190 <- crosstab(pkdhs, v190, signalfunc, weight = wt,
                      pct_type = "row", unwt_n = T, format = "wide", n = T)
# sex of child b4
xtab.b4 <- crosstab(pkdhs, b4, signalfunc, weight = wt,
                    pct_type = "row", unwt_n = T, format = "wide", n = T)
# anc visits
pkdhs <- pkdhs %>%
  mutate(
    anc = case_when(
      m14 == 0 ~ 0,
      m14 >= 1 & m14 < 4 ~ 1,
      TRUE ~ 2)) %>%
  set_value_labels(anc = c("None" = 0, "1-3" = 1, "4 and more" = 2)) %>%
  set_variable_labels(anc = "Antenatal care visits") 
xtab.anc <- crosstab(pkdhs, anc, signalfunc, weight = wt,
                     pct_type = "row", unwt_n = T, format = "wide", n = T)
pkdhs$anc <- as.factor(pkdhs$anc)
# quality of anc
pkdhs <- pkdhs %>%
  mutate(
    m42cbi = case_when(
      m42c == "yes" ~ 1,
      TRUE ~ 0)) %>%
  mutate(
    m42dbi = case_when(
      m42d == "yes" ~ 1,
      TRUE ~ 0)) %>%
  mutate(
    m42ebi = case_when(
      m42e == "yes" ~ 1,
      TRUE ~ 0)) %>%
  mutate(
      m45bi = case_when(
        m45 == "yes" ~ 1,
        TRUE ~ 0)) %>%
  mutate(
    m60bi = case_when(
      m60 == "yes" ~ 1,
      TRUE ~ 0)) %>%
  rowwise() %>%
  mutate(
    yes_count = sum(m42cbi, m42dbi, m42ebi, m45bi, m60bi)) %>%
  mutate(
    ancqua = case_when(
      yes_count >= 4 ~1,
      TRUE ~ 0))%>%
  set_value_labels(ancqua = c("Poor" = 0, "Good" = 1)) %>%
  set_variable_labels(ancqua = "Quality of ANC") 
xtab.ancqua <- crosstab(pkdhs, ancqua, signalfunc, weight = wt,
                     pct_type = "row", unwt_n = T, format = "wide", n = T)

# v743a (women's participation in decision making)
pkdhs <- pkdhs %>%
  mutate(
    healthdecision = case_when(
      v743a == "respondent alone" ~ 1,
      v743a == "respondent and husband/partner" ~ 1,
      v743a == "respondent and other person" ~ 0,
      v743a == "husband/partner alone" ~ 0,
      v743a == "someone else" ~ 0,
      v743a == "other" ~ 0,
    )) %>%
  set_value_labels(healthdecision = c("Respondent Decides" = 1, "Respondent Does not Decide" = 0)) %>%
  set_variable_labels(healthdecision = "Participation in Decision-making for Health")
xtab.healthdecision <- crosstab(pkdhs, healthdecision, signalfunc, weight = wt,
                     pct_type = "row", unwt_n = T, format = "wide", n = T)

# delivery by csection (m17)
xtab.cs <- crosstab(pkdhs, m17, signalfunc, weight = wt,
                    pct_type = "row", unwt_n = T, format = "wide", n = T)

# Skilled Birth Attendence
pkdhs <- pkdhs %>%
  mutate(
    sba = case_when(
      m3a == "yes" ~ 1, # doctor
      m3b == "yes" ~ 1, # Nurse
      m3c == "yes" ~ 1, # Midwife
      m3d == "yes" ~ 1, # Lady health visitor
      m3e == "yes" ~ 1, # Community midwife 
      m3g == "yes" ~ 0, # Dai/traditional birth attendant
      m3h == "yes" ~ 0, # Family welfare wk
      m3i == "yes" ~ 0, # LHW
      m3j == "yes" ~ 0, # Homeopath 
      m3k == "yes" ~ 0, # other
      m3l == "yes" ~ 0, # Hakim
      m3m == "yes" ~ 0, # Relative/friend
      )) %>%
  set_value_labels(sba = c("Skilled" = 1, "Unskilled" = 0)) %>%
  set_variable_labels(sba = "Birth assisted by a skilled provider")
xtab.sba <- crosstab(pkdhs, sba, signalfunc, weight = wt,
                    pct_type = "row", unwt_n = T, format = "wide", n = T)
xtab.accHF <- crosstab(pkdhs, accHF, signalfunc, weight = wt,
                       pct_type = "row", unwt_n = T, format = "wide", n = T)
# Owns a mobile phone (v169a)
xtab.mobile <- crosstab(pkdhs, v169a, signalfunc, weight = wt,
                    pct_type = "row", unwt_n = T, format = "wide", n = T)


# exporting bivar analysis to MS Excel
library(openxlsx)
# storing all bivar analysis in one list
crosstab_results <- list(xtab.m_age, xtab.bord_cat, xtab.b_HF, 
                         xtab.v106, xtab.v101, xtab.v102, 
                         xtab.v190, xtab.b4, xtab.anc, xtab.cs, xtab.sba,
                         xtab.accHF, xtab.mediaexp, xtab.mobile, xtab.healthdecision)
# Specify sheet names (modify as needed)
sheet_names <- c("Sheet1", "Sheet2", "Sheet3", "Sheet4", "Sheet5",
                 "Sheet6", "Sheet7", "Sheet8", "Sheet9", "Sheet10", "Sheet11",
                 "Sheet12", "Sheet13", "Sheet14", "Sheet15")
# Create a new Excel workbook
wb <- createWorkbook()

# Loop through the list of results and write them to separate sheets
for (i in 1:length(crosstab_results)) {
  addWorksheet(wb, sheetName = sheet_names[i])
  writeData(wb, sheet = i, x = crosstab_results[[i]])
}
excel_file_path <- "output_crosstabs1.xlsx"

saveWorkbook(wb, file = excel_file_path, overwrite = TRUE)

# Running bivariate logistic regression
# Create a new Excel workbook
wb <- createWorkbook()
# Listing all Xs in one object
x_vars <- c("m_age_cat", "bord_cat", "b_HF", "v106", "v101", 
            "v102","v190", "b4", "anc", "m17", "sba", "accHF",
            "media_exp", "v169a", "healthdecision")

model_list <- list()

for (var in x_vars) {
  formula <- formula(paste("signalfunc ~", var))
  model <- glm(formula, data = pkdhs, family = binomial)
  odds_ratios <- exp(cbind(OR = coef(model), confint(model)))
  #model_list[[var]] <- list(model = model, odds_ratios = odds_ratios)
  # Create a new sheet for each model
  addWorksheet(wb, sheetName = var)
  
  # Write the model summary to the sheet
  writeData(wb, sheet = var, x = model)
  # Write the odds ratios to the sheet
  writeData(wb, sheet = var, x = odds_ratios, startCol = "B", startRow = 10)
}

saveWorkbook(wb, file = "BivarLogRegA.xlsx", overwrite = TRUE)

adj.model <- glm(signalfunc ~ m_age_cat + bord_cat + b_HF + v106 + v101 +
                   v102 + v190 + b4 + anc + m17 + sba, data = pkdhs, 
                 family = binomial)
summary(adj.model)
adj.ORs <- exp(cbind(OR = coef(adj.model), confint(adj.model)))
wb <- createWorkbook()
addWorksheet(wb, sheetName = "adjModel")
writeData(wb, sheet = "adjModel", x = adj.model)
writeData(wb, sheet = "adjModel", x = adj.ORs, startCol = "H", rowNames = TRUE)
saveWorkbook(wb, file = "AdjLogReg_results3ANYmedia.xlsx", overwrite = TRUE)

## Plotting Adj ORs as a forest plot
library(MASS)
or_CI <- round(exp(cbind(coef(adj.ORs),
                         confint(adj.ORs))), digits = 3) %>%
  as.data.frame()
or_CI <- adj.ORs %>%
  as.data.frame() %>%
  mutate(variable = rownames(adj.ORs))

or_CI <- rename(or_CI, c("AOR" = "OR",
                         "Lower_bound" = "2.5 %",
                         "Upper_bound" = "97.5 %"))

col_order <- c("variable", "AOR", "Lower_bound", "Upper_bound")
or_CI <- or_CI[, col_order]

## Plot (do not run)
plot_logit_model <- or_CI[-1,] %>%  #remove row number 1 (The intercept) 
  ggplot(aes(x = reorder(variable, AOR), y = AOR)) +
  geom_point(shape = 1,
             size  = 4,
             position = "dodge", color="black") + 
  geom_errorbar(aes(ymin  = Lower_bound,
                    ymax  = Upper_bound),
                size  = 0.7,
                position = "dodge", color="turquoise4") +
  theme(axis.title = element_text(face = "bold")) +
  xlab("Variables") + ylab("Adjusted odds ratios with 95% CI") +
  coord_flip(ylim = c(0, 3.5)) + 
  geom_hline(yintercept = 1, color = "red", size = 1) +
  theme(axis.title = element_text(size = 17)) + 
  theme(axis.text = element_text(size = 14)) 
plot_logit_model


### For assessing interaction
##### Visualizing Interaction without running Regression
custom_colors1 <- rev(c(
  "#16722c", "#66d280", "#8199ba",
  "#88292f", "#c96480"))
df_summary <- pkdhs %>%
  filter(!is.na(v102)) %>%
  group_by(v190, v102) %>%
  summarise(proportion = mean(signalfunc))
df_summary$v190 <- factor(df_summary$v190,
                          levels = c("poorest", "poorer", "middle", "richer", "richest"),
                          labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))
EMPlot <- ggplot(df_summary, aes(x = v102 , y = proportion,
                                 group = v190, color = v190)) +
  geom_line(size = 2, alpha = 0.5) +  # Line plot to show interaction
  geom_point(size = 8) +  # Adding points for each value
  labs(title = "Visualizing Interaction between Place of\n        Residence and Wealth Index",
       x = NULL,
       y = "Proportion of At least 2 Signal Functions",
       color = "Wealth Index") +
  scale_color_manual(values = custom_colors1,   # Custom color palette for legend
                     labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")) +  # Custom legend labels
  scale_x_discrete(labels = c("Urban", "Rural")) +
  theme_minimal() +
  theme(plot.title = element_text(size = rel(1.6)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(hjust = 0.5, size = rel(1.5)),
        axis.text.y = element_text(hjust = 0.5, size = rel(1.5)))  # Rotate x-axis labels for readability
ggsave("InteractionPlot1.png", EMPlot, dpi = 600 )


## Not included in the current analysis
### stepwise regression
fullMod <- glm(signalfunc ~ m_age_cat + bord_cat + b_HF + v106 + v101 + 
               v102 + v190 + b4 + anc + m17 + sba + accHF + media_exp +
               v169a, data = pkdhs, family = binomial)

nullMod <- glm(signalfunc ~ 1, data = pkdhs, family = binomial)
summary(nullMod)

fullMod$deviance; nullMod$deviance
backMod <- step(fullMod, direction = "backward")


library(olsrr)
pncNBmod <- lm(signalfunc ~ v169a + accHF + media_exp + v102 + b_HF + 
                 sba + v190 + anc + anc + m17 + v101, data = pkdhs)
ols_step_forward_p(pncNBmod, 0.2, hierarchical = T)
