### ------------------------------- ###
###        Sanctuary Lost           ###
###      Replication data           ###
### ------------------------------- ###

### Load key packages ----------------------------------------------------------
library(ggplot2)
library(zoo)
library(MASS) 
library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggpubr)
library(glm.predict)
library(jtools)
library(broom)
library(ggstance)
library(data.table)
library(ggplot2)
library(ggprism)
library(plm)
library(clubSandwich)
library(pglm)
library(lmtest)
library(sjmisc)
library(grid)
library(pscl)
library(magrittr)
library(peacesciencer)
library(readstata13)
library(dplyr)
library(broom)
library(kableExtra)


### ----------------------------------------------------------------------------

### ------------------------------- ###
###          Figure 1               ###
### ------------------------------- ###
## Read in file of 1990-2018 monthly summary data: 
locscon <- readRDS(
  "[Insert your folder path here]/locs_monthlysummary.Rdata"
)


### Settings -------------------------------------------------------------------
windowsFonts(Palatino=windowsFont("Palatino Linotype"))
My_Theme = theme(
  title = element_text(size = 9, family="Palatino"),
  plot.title.position = "plot",
  plot.caption = element_text(size = 6, hjust = 0,family="Palatino", face = "italic"),
  plot.caption.position = "plot",
  axis.title.x = element_text(size = 9, family="Palatino"),
  axis.text.x = element_text(size = 9, family="Palatino"),
  axis.text.y = element_text(size = 9, family="Palatino"),
  axis.title.y = element_text(size = 9, family="Palatino"),
  legend.title = element_text(size = 9, family="Palatino", face = "italic"),
  legend.text = element_text(size = 9, family="Palatino"),
  strip.text.x = element_text(size = 9, family = "Palatino"))
# Reduce the opacity of the grid lines ("gray92"):
col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255)

### Scales ---------------------------------------------------------------------
# Generate years from 1990 to 2018, stepping by every two years
years_to_label <- seq(1990, 2018, by = 4)

# Convert these years to as.yearmon, assuming January for simplicity
yearmon_for_breaks <- as.yearmon(paste0(years_to_label, "-01"))

# Convert yearmon objects to the internal numeric representation
breaks_numeric <- as.numeric(yearmon_for_breaks)

### Plot settings --------------------------------------------------------------
library(ggplot2)
legend_title <- "Observed Host Policy"
colors_1 <- c("Pledge to attack" = "gray88", "Attack rebels" = "wheat3", "Do nothing" = "steelblue4") # Darker
colors_2 <- c("Pledge to attack" = "gray93", "Attack rebels" = "wheat2", "Do nothing" = "dodgerblue4") # Lighter
#border_col <- "gray45" 

### Plot -----------------------------------------------------------------------
ggplot(locscon, aes(x=IDMonthly, fill=monthly_status)) +
  geom_bar(position="fill", aes(y=after_stat(count), color=monthly_status), 
           size=0.2,
           alpha = 0.8) + # Removed width for automatic adjustment
  scale_fill_manual(name=legend_title, values=colors_2) +
  scale_color_manual(name=legend_title, values=colors_1) +
  labs(y="Proportion of Dyads \\small(\\emph{n}=88)", x="Time") +
  theme_classic() +
  scale_x_continuous(breaks = breaks_numeric, labels = years_to_label) +
  scale_y_continuous(limits = c(0, 1)) +
  My_Theme


### ----------------------------------------------------------------------------

### ------------------------------- ###
###             H1                  ###
### ------------------------------- ###

###  Task 1: Finding baseline probability  -------------------------------------

### Upload data ----------------------------------------------------------------

## Read in file of 1990-2018 duration spells: 
locscon <- readRDS(
  "[Insert your folder path here]/locs_durationspells_v1.Rdata"
)

## Define as time series:
locscon_p <- pdata.frame(locscon, index = c("hostguest_id", "IDMonthly"))


# Make the key model without IV:
PA_t1.twfe_pooled <- plm(PA_transition ~ rebhost_troopratio_lag +
                           gsv_mo_osv_lag + 
                           conf_poly1_lag +
                           conf_poly1_trend_lag +
                           mo_battles_n_lag +
                           gtd_attacks_n_lag +
                           host_vdem_polyarchy_lag +
                           host_rpe_gdp_lag +
                           host_vdem_svstterr_lag +
                           relpow_lag,
                         model = "pooling", index = c("hostguest_id"),
                         data = locscon_p)


# Make scenario data:
pframe_pooled <- data.frame(relpow_lag=mean(locscon_p$relpow_lag, na.rm=TRUE),
                            rebhost_troopratio_lag=mean(locscon_p$rebhost_troopratio_lag, na.rm=TRUE),
                            gsv_mo_osv_lag=mean(locscon_p$gsv_mo_osv_lag, na.rm=TRUE),
                            conf_poly1_lag=mean(locscon_p$conf_poly1_lag, na.rm=TRUE),
                            conf_poly1_trend_lag=mean(locscon_p$conf_poly1_trend_lag, na.rm=TRUE),
                            mo_battles_n_lag=mean(locscon_p$mo_battles_n_lag, na.rm=TRUE),
                            gtd_attacks_n_lag=mean(locscon_p$gtd_attacks_n_lag, na.rm=TRUE),
                            host_vdem_polyarchy_lag=mean(locscon_p$host_vdem_polyarchy_lag, na.rm=TRUE),
                            host_rpe_gdp_lag=mean(locscon_p$host_rpe_gdp_lag, na.rm=TRUE),
                            host_vdem_svstterr_lag=mean(locscon_p$host_vdem_svstterr_lag, na.rm=TRUE)
)


## Task A: Predict the probability at the mean level of covariates
baseline_probability <- predict(PA_t1.twfe_pooled, newdata = pframe_pooled, type = "response")
print(baseline_probability)   # Baseline prob of official policy shift when covariates are at mean
mean(locscon_p$PA_transition) # Baseline prob ignoring covariates
## End of task A ---------------------------------------------------------------

## Task B: Predict the probability of pledge given lowest quartile and highest quartile of relative power:
relpow_qtls <- quantile(locscon_p$relpow_lag, na.rm = T)

# Make the key model of interest:
PA_t1.twfe <- plm(PA_transition ~ idea_criticize_1mo_t1 +
                    rebhost_troopratio_lag +
                    gsv_mo_osv_lag + 
                    conf_poly1_lag +
                    mo_battles_n_lag +
                    gtd_attacks_n_lag +
                    host_vdem_polyarchy_lag +
                    host_rpe_gdp_lag +
                    host_vdem_svstterr_lag +
                    relpow_lag,
                  model = "pooling", index = c("hostguest_id"),
                  data = locscon_p)

### Find quartiles: Relpow
relpow_25th <- relpow_qtls[[2:2]]
relpow_mean <- mean(locscon_p$relpow_lag, na.rm=T)
relpow_75th <- relpow_qtls[[4:4]]

# Make scenario data 
newframe_H1_relpow <- data.frame(idea_criticize_1mo_t1 = c(0, 0, 1, 1),
                                 relpow_lag=c(relpow_25th, relpow_75th, relpow_25th, relpow_75th),
                                 rebhost_troopratio_lag=mean(locscon_p$rebhost_troopratio_lag, na.rm=TRUE),
                                 gsv_mo_osv_lag=mean(locscon_p$gsv_mo_osv_lag, na.rm=TRUE),
                                 conf_poly1_lag=mean(locscon_p$conf_poly1_lag, na.rm=TRUE),
                                 mo_battles_n_lag=mean(locscon_p$mo_battles_n_lag, na.rm=TRUE),
                                 gtd_attacks_n_lag=mean(locscon_p$gtd_attacks_n_lag, na.rm=TRUE),
                                 host_vdem_polyarchy_lag=mean(locscon_p$host_vdem_polyarchy_lag, na.rm=TRUE),
                                 host_rpe_gdp_lag=mean(locscon_p$host_rpe_gdp_lag, na.rm=TRUE),
                                 host_vdem_svstterr_lag=mean(locscon_p$host_vdem_svstterr_lag, na.rm=TRUE)
)

# Predict the probability at the mean level of covariates
relpow_probability <- predict(PA_t1.twfe, newdata = newframe_H1_relpow, type = "response")
print(relpow_probability)
## End of Task B ---------------------------------------------------------------


### Task 2: Testing H1 ---------------------------------------------------------

### Definition:
### Two-way fixed-effects linear probability model (TWFE-LPM).

# Define all time lags including both 't' and 'le'
time_lags <- c("le5", "le4", "le3", "le2", "le1", "t", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12")

# Initialize an empty list to store data frames
estframe_PA_list <- list()

# Loop to process each time lag
for (lag in time_lags) {
  iv_part <- ifelse(grepl("le", lag), paste0("factor(idea_criticize_1mo_", lag, ")"), paste0("factor(idea_criticize_1mo_", lag, ")"))
  
  # Construct the full model formula dynamically
  formula_lag <- as.formula(paste0("PA_transition ~ ", iv_part, " + 
                                      rebhost_troopratio_lag +
                                      gsv_mo_osv_lag + 
                                      conf_poly1_lag +
                                      host_anyTEK_lag +
                                      mo_battles_n_lag +
                                      gtd_attacks_n_lag +
                                      host_vdem_polyarchy_lag +
                                      host_rpe_gdp_lag +
                                      host_vdem_svstterr_lag +
                                      relpow_lag"))
  
  # Model estimation
  model_name <- paste0("PA_", lag, ".twfe")
  eval(parse(text = paste0(model_name, " <- plm(formula_lag, model = 'within', effect = 'twoways', index = c('hostguest_id'), data = locscon_p)")))
  
  # Extract model summary
  eval(parse(text = paste0(model_name, ".summary <- summary(", model_name, ")")))
  
  # Convert the summary coefficients to a data frame
  eval(parse(text = paste0(model_name, ".df <- as.data.frame(", model_name, ".summary$coefficients)")))
  
  # Calculate confidence intervals
  eval(parse(text = paste0(model_name, ".df$conf.low95 <- ", model_name, ".df$Estimate - ", model_name, ".df$'Std. Error' * qt(0.975, df.residual(", model_name, "))")))
  eval(parse(text = paste0(model_name, ".df$conf.high95 <- ", model_name, ".df$Estimate + ", model_name, ".df$'Std. Error' * qt(0.975, df.residual(", model_name, "))")))
  eval(parse(text = paste0(model_name, ".df$conf.low90 <- ", model_name, ".df$Estimate - ", model_name, ".df$'Std. Error' * qt(0.95, df.residual(", model_name, "))")))
  eval(parse(text = paste0(model_name, ".df$conf.high90 <- ", model_name, ".df$Estimate + ", model_name, ".df$'Std. Error' * qt(0.95, df.residual(", model_name, "))")))
  eval(parse(text = paste0(model_name, ".df$conf.low99 <- ", model_name, ".df$Estimate - ", model_name, ".df$'Std. Error' * qt(0.995, df.residual(", model_name, "))")))
  eval(parse(text = paste0(model_name, ".df$conf.high99 <- ", model_name, ".df$Estimate + ", model_name, ".df$'Std. Error' * qt(0.995, df.residual(", model_name, "))")))
  
  # Store results in the list
  estframe_PA_list[[lag]] <- get(paste0(model_name, ".df"))
}

### ---------------------------------- ###
### Combine all data frames into one   ###
### ---------------------------------- ###

# Define the suffixes corresponding to the different data frames
PA_suffixes <- c("le5", "le4", "le3", "le2", "le1", "t", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12")

# Initialize vectors to store the extracted values
est <- lwr95 <- upr95 <- lwr90 <- upr90 <- lwr99 <- upr99 <- numeric(length(PA_suffixes))

# Loop over the suffixes to extract values from the corresponding data frames
for (i in 1:length(PA_suffixes)) {
  df_name <- paste0("PA_", PA_suffixes[i], ".twfe.df")
  est[i] <- get(df_name)$Estimate[1]
  lwr95[i] <- get(df_name)$conf.low95[1]
  upr95[i] <- get(df_name)$conf.high95[1]
  lwr90[i] <- get(df_name)$conf.low90[1]
  upr90[i] <- get(df_name)$conf.high90[1]
  lwr99[i] <- get(df_name)$conf.low99[1]
  upr99[i] <- get(df_name)$conf.high99[1]
}

# Create the data frame
estframe_PA <- data.frame(
  month = c(-5:12),
  est = est,
  lwr95 = lwr95,
  upr95 = upr95,
  lwr90 = lwr90,
  upr90 = upr90,
  lwr99 = lwr99,
  upr99 = upr99
)

### ------------- ###
###      Plot     ###
### ------------- ###


## Æsthethics:
windowsFonts(Palatino=windowsFont("Palatino Linotype"))
My_Theme = theme(
  title = element_text(size = 10, family="Palatino"),
  plot.title.position = "plot",
  plot.caption = element_text(size = 7, hjust = 0,family="Palatino", face = "italic"),
  plot.caption.position = "plot",
  axis.title.x = element_text(size = 10, family="Palatino", face = "italic"),
  axis.text.x = element_text(size = 8, family="Palatino"),
  axis.text.y = element_text(size = 8, family="Palatino"),
  axis.title.y = element_text(size = 8, family="Palatino"),
  legend.title = element_text(size = 11, family="Palatino", face = "italic"),
  legend.text = element_text(size = 10, family="Palatino"),
  strip.text.x = element_text(size = 13, family = "Palatino"))
# Reduce the opacity of the grid lines ("gray92"):
col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255)
colors <- c("CI 99" = "gray8", "CI 95" = "gray50")

## Define plot:
Protest_PA_plot <- ggplot(data=estframe_PA, 
                          aes(x = month, y = est)) +
  geom_hline(aes(yintercept = 0), # Origo
             colour = "black", size = 0.5, lty = 2, alpha = 0.7) +
  geom_vline(aes(xintercept = 0), # Origo
             colour = "black", size = 0.5, lty = 2, alpha = 0.7) +
  geom_linerange(aes(y=est, 
                     ymin=lwr90, ymax=upr90),
                 color =  ifelse((estframe_PA$lwr99 > 0 & estframe_PA$upr99 > 0) | 
                                   (estframe_PA$lwr99 < 0 & estframe_PA$upr99 < 0), "gray8",
                                 ifelse((estframe_PA$lwr95 > 0 & estframe_PA$upr95 > 0) | 
                                          (estframe_PA$lwr95 < 0 & estframe_PA$upr95 < 0), "gray56",
                                        "gray56")),
                 alpha=0.8, lwd=2, position=position_dodge(0.1)) +
  geom_linerange(aes(y=est, 
                     ymin=lwr95, ymax=upr95),
                 color =  ifelse((estframe_PA$lwr99 > 0 & estframe_PA$upr99 > 0) | 
                                   (estframe_PA$lwr99 < 0 & estframe_PA$upr99 < 0), "gray8",
                                 ifelse((estframe_PA$lwr95 > 0 & estframe_PA$upr95 > 0) | 
                                          (estframe_PA$lwr95 < 0 & estframe_PA$upr95 < 0), "gray56",
                                        "gray56")),
                 alpha=0.8, lwd=1, position=position_dodge(0.1)) +
  geom_linerange(aes(y=est, 
                     ymin=lwr99, ymax=upr99),
                 color =  ifelse((estframe_PA$lwr99 > 0 & estframe_PA$upr99 > 0) | 
                                   (estframe_PA$lwr99 < 0 & estframe_PA$upr99 < 0), "gray8",
                                 ifelse((estframe_PA$lwr95 > 0 & estframe_PA$upr95 > 0) | 
                                          (estframe_PA$lwr95 < 0 & estframe_PA$upr95 < 0), "gray56",
                                        "gray56")),
                 alpha=0.8, lwd=0.5, position=position_dodge(0.1)) +
  geom_point(aes(x=month, y=est), 
             colour = "black", fill = "white", size = 2, pch=21) 
#  + geom_line() 

# Implement:
Protest_PA_plot  +  theme_prism() + My_Theme + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 18)) +
  scale_color_manual(values = colors, name = "Confidence Interval") + # Legend
  theme(panel.grid.major.x = element_blank(), # vertical grids
        panel.grid.minor.y = element_line(color = "gray89", size = .01))+  # horizontal grid
  ylab("Change in Monthly Probability of Pledge") + xlab("Months Since Bilateral Public Criticism") +  #XYnames
  labs(caption = "Model: Two-way Fixed-Effects LPM. \nCovariates lagged to previous month.\nConfidence Intervals: Thickest=p$<$0.1, thinner=p$<$0.05; thinnest=p$<$0.01.")

### -------------------- ###
###   TWFE Diagnostics   ###
### -------------------- ###

### Purpose: 
### We run two diagnostics: we (i) check for negative weights in treated units and 
### (ii) check for treatment effect homogeneity.

## Prepare data:
locscon_full_diag <- locscon %>% filter(!is.na(idea_criticize_1mo_t1) & !is.na(hostguest_id) & !is.na(IDMonthly))

## Run model:
trt_resid_primary <- lm(idea_criticize_1mo_t1 ~ hostguest_id + factor(IDMonthly), data = locscon_full_diag)

## Extract residuals and weights:
fpe_primary_weights <- locscon_full_diag %>%
  mutate(treatment_resid = residuals(trt_resid_primary)) %>%
  mutate(treatment_weight = treatment_resid / sum(treatment_resid^2))

library(tidyverse)     
library(broom)         
library(scales)
fpe_primary_weights %>%
  dplyr::summarize(twfe_beta_primary = sum(PA_transition * treatment_weight))

## Task A: Do treated observations receive negative weights? -------------------
# Total treated in primary data
n_treated_primary <- fpe_primary_weights %>%
  filter(idea_criticize_1mo_t1 == 1) %>%
  nrow()

# Negatively weighted treated observations in the primary data
n_treated_negative_primary <- fpe_primary_weights %>%
  filter(treatment_weight < 0 & idea_criticize_1mo_t1 == 1) %>%
  nrow()

negative_weights_df <- data.frame("Negative weights" = n_treated_negative_primary,
                                  "Negative weights per Treatment group" = n_treated_negative_primary/n_treated_primary)

## § Print: Table  .............................................................                       
#Negative weights received by treated observations (BilateralPublicCriticism(t+1)=1)"
options(knitr.kable.NA = '', knitr.table.format = "latex") # Remove ugly NAs
negative_weights_latex <- kable(negative_weights_df, format = 'latex', 
                                booktabs = TRUE,
                                bottomrule = '',
                                toprule = '',
                                midrule = '') %>%
  row_spec(0, hline_after = TRUE) %>% # Make line under first row
  row_spec(row = 0, # Make first row bold
           bold = TRUE) %>%
  column_spec(column = 1, # Add border
              border_left = TRUE) %>%
  column_spec(column = ncol(negative_weights_df), # Add border
              border_right = TRUE) %>%
  collapse_rows(columns = 1, valign = "middle") %>%
  add_header_above(bold = TRUE, border_left = TRUE, border_right = TRUE) %>%
  kable_styling(font_size = 9) 

### ----------------------------------------------------------------------------

## --> There are no observations that receive negative weights
## Task A completed ------------------------------------------------------------

## Task B: Is the homogeneity assumption violated? -----------------------------


## B.I: Graphical representation -----------------------------------------------
## Add residuals to data with weights:
out_resid_primary <- lm(idea_criticize_1mo_t1 ~ hostguest_id + factor(IDMonthly), data = locscon_full_diag)
fpe_primary_weights <- fpe_primary_weights %>%
  mutate(out_resid = residuals(out_resid_primary))

# Graphical: -------------------------------------------------------------------
windowsFonts(Palatino=windowsFont("Palatino Linotype"))
My_Theme = theme(
  title = element_text(size = 8, family="Palatino"),
  plot.title.position = "plot",
  plot.caption = element_text(size = 7, hjust = 0,family="Palatino", face = "italic"),
  plot.caption.position = "plot",
  axis.title.x = element_text(size = 10, family="Palatino", face = "italic"),
  axis.text.x = element_text(size = 8, family="Palatino"),
  axis.text.y = element_text(size = 8, family="Palatino"),
  axis.title.y = element_text(size = 8, family="Palatino"),
  legend.title = element_text(size = 11, family="Palatino", face = "italic"),
  legend.text = element_text(size = 10, family="Palatino"),
  strip.text.x = element_text(size = 13, family = "Palatino"))
# Reduce the opacity of the grid lines ("gray92"):
col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255)

(plot_out_trt_primary <- ggplot(fpe_primary_weights,
                                aes(x = treatment_resid, y = out_resid, color = factor(idea_criticize_1mo_t1))) +
    geom_point(size = 0.75, alpha = 0.5) +
    geom_smooth(aes(linetype = "Loess"), method = "loess", size = 1, se = FALSE, alpha = 0.5) +
    geom_smooth(aes(linetype = "OLS"), method = "lm", se = FALSE) +
    scale_color_viridis_d(option = "rocket", begin = 0.2, end = 0.8,
                          labels = c("Untreated", "Treated")) +
    scale_linetype_manual(values = c("OLS" = "solid", "Loess" = "21"),
                          guide = guide_legend(override.aes = list(color = "grey30"))) +
    labs(x = "Residualised treatment", y = "Residualised outcome",
         title = "Criticism Onset (1 months lag)", color = NULL, linetype = NULL) +
    theme_minimal() + My_Theme + 
    theme(legend.position = "bottom"))

## § Print: Figure .............................................................
## --> The lines for treated and untreated observations look like they have similar slopes. 
## Task B.I completed ----------------------------------------------------------

### -------------------------- ###
###   Full regression results
### -------------------------- ###

suffixes <- c( "le3", "le2", "le1", "t", "t1", "t2", "t3", "t4", "t5", "t6")

for (suffix in suffixes) {
  model_name <- paste0("PA_", suffix, ".sum")
  data_name <- paste0("PA_", suffix, ".twfe.df")
  
  assign(model_name,
         get(data_name) %>%
           mutate(Variable = rownames(.)) %>%
           select(Variable, Estimate, `Pr(>|t|)`) %>%
           rename(p = `Pr(>|t|)`,
                  Est = Estimate))
}

library(purrr)
# List of model data frames
PA_sum <- mget(paste0("PA_", suffixes, ".sum"))

# Merge all data frames by "Variable" column
PA_sum_df <- reduce(PA_sum, full_join, by = "Variable")

# Define the desired order of suffixes
PA_varorder <- paste0("factor(idea_criticize_1mo_", suffixes, ")1")
# Find unique values in the "Variable" column of PA_sum_df
additional_variables <- setdiff(unique(PA_sum_df$Variable), PA_varorder)

# Add the additional variables to PA_varorder
PA_varorder <- c(PA_varorder, additional_variables)

# Find the indices of the rows in PA_sum_df according to PA_varorder
row_indices <- match(PA_sum_df$Variable, PA_varorder)

# Arrange the rows in PA_sum_df according to PA_varorder
PA_sum_df <- PA_sum_df[order(row_indices), ]

## Clean names:
PA_sum_df <- PA_sum_df %>% 
  mutate(Variable = case_when(Variable == "factor(idea_criticize_1mo_le5)1" ~ "Criticism (t−5)",
                              Variable == "factor(idea_criticize_1mo_le4)1" ~ "Criticism (t−4)",
                              Variable == "factor(idea_criticize_1mo_le3)1" ~ "Criticism (t−3)",
                              Variable == "factor(idea_criticize_1mo_le2)1" ~ "Criticism (t−2)",
                              Variable == "factor(idea_criticize_1mo_le1)1" ~ "Criticism (t−1)",
                              Variable == "factor(idea_criticize_1mo_t)1" ~ "Criticism (t)",
                              Variable == "factor(idea_criticize_1mo_t1)1" ~ "Criticism (t+1)",
                              Variable == "factor(idea_criticize_1mo_t2)1" ~ "Criticism (t+2)",
                              Variable == "factor(idea_criticize_1mo_t3)1" ~ "Criticism (t+3)",
                              Variable == "factor(idea_criticize_1mo_t4)1" ~ "Criticism (t+4)",
                              Variable == "factor(idea_criticize_1mo_t5)1" ~ "Criticism (t+5)",
                              Variable == "factor(idea_criticize_1mo_t6)1" ~ "Criticism (t+6)",
                              Variable == "factor(idea_criticize_1mo_t7)1" ~ "Criticism (t+7)",
                              Variable == "factor(idea_criticize_1mo_t8)1" ~ "Criticism (t+8)",
                              Variable == "factor(idea_criticize_1mo_t9)1" ~ "Criticism (t+9)",
                              Variable == "factor(idea_criticize_1mo_t10)1" ~ "Criticism (t+10)",
                              Variable == "factor(idea_criticize_1mo_t11)1" ~ "Criticism (t+11)",
                              Variable == "factor(idea_criticize_1mo_t12)1" ~ "Criticism (t+12)",
                              Variable == "idea_criticize_1mo_t1" ~ "Criticism (t+1)",
                              Variable == "host_rpe_gdp_lag" ~"Political Extraction", 
                              Variable == "host_vdem_svstterr_lag" ~"Territorial Authority",
                              Variable == "relpow_lag" ~  "Relative Power",
                              Variable == "mo_battles_n_lag" ~ "Monthly Battles", 
                              Variable == "gtd_attacks_n_lag" ~ "Monthly Terror",
                              Variable == "rebhost_troopratio_lag" ~ "Troop Ratio",
                              Variable == "host_vdem_polyarchy_lag" ~ "Democracy Score",
                              Variable == "host_anyTEK_lag" ~ "Relevant TEK", 
                              Variable == "conf_poly1_lag" ~ "Conflict Time", 
                              Variable == "gsv_mo_osv_lag" ~ "Sanctuary Violence", 
                              TRUE ~ Variable)) 

### Round to X decimals:
PA_sum_df <- PA_sum_df %>%
  mutate_at(vars(starts_with("p")), ~ round(., 5)) %>%
  mutate_at(vars(starts_with("Est")), ~ round(., 3)) 

# Replace NA with ""
PA_sum_df <- PA_sum_df %>%
  mutate_all(~ replace(., is.na(.), ""))


# Function to merge values from Est and p columns
merge_values <- function(df) {
  # Initialize an empty list to store modified column names
  new_columns <- list()
  
  # Loop through each column
  for (col in names(df)) {
    # Check if the column starts with "Est"
    if (startsWith(col, "Est")) {
      # Get the index of the corresponding p column
      p_index <- which(names(df) == gsub("Est", "p", col))
      
      # If a corresponding p column exists
      if (length(p_index) > 0) {
        # Merge values from Est and p columns, wrapping p value in parentheses
        new_col <- paste(df[[col]], " (", df[[names(df)[p_index]]], ")", sep = "")
      } else {
        # If no corresponding p column exists, leave the value unchanged
        new_col <- df[[col]]
      }
    } else {
      # If the column doesn't start with "Est", leave the value unchanged
      new_col <- df[[col]]
    }
    
    # Add the modified column to the list
    new_columns[[col]] <- new_col
  }
  
  # Combine the modified columns into a new data frame
  new_df <- as.data.frame(new_columns)
  return(new_df)
}

# Apply the function to your data frame
PA_sum_def_modified <- merge_values(PA_sum_df)

# Change the name:
names(PA_sum_def_modified) <- gsub("Est", "Est (p)", names(PA_sum_def_modified))

# Remove p columns:
PA_sum_def_modified <- PA_sum_def_modified %>%
  select(-starts_with("p"))


## Remove merging labels:
names(PA_sum_def_modified) <- gsub("[xy\\.]", "", names(PA_sum_def_modified))

# Remove emptly parantheses:
PA_sum_def_modified <- apply(PA_sum_def_modified, c(1, 2), function(x) gsub("\\(\\)", "", x))

## Remove the row names:
rownames(PA_sum_def_modified) <- NULL



## § Print: Table  .............................................................                       
# Regression Results: The effect of Bilateral Public Criticism on the monthly probability of new Antirebel Pledge
options(knitr.kable.NA = '', knitr.table.format = "latex") # Remove ugly NAs
PA_sum_df_latex <- kable(PA_sum_def_modified, format = 'latex', 
                         booktabs = TRUE,
                         bottomrule = '',
                         toprule = '',
                         midrule = '') %>%
  row_spec(0, hline_after = TRUE) %>% # Make line under first row
  row_spec(row = 0, # Make first row bold
           bold = TRUE) %>%
  column_spec(column = 1, # Add border
              border_left = TRUE) %>%
  column_spec(column = ncol(PA_sum_def_modified), # Add border
              border_right = TRUE) %>%
  collapse_rows(columns = 1, valign = "middle") %>%
  add_header_above(bold = TRUE, border_left = TRUE, border_right = TRUE) %>%
  kable_styling(font_size = 6)

### ----------------------------------------------------------------------------



### ------------------------------- ###
###          Robustness 1           ###
### ------------------------------- ###

### ------------------- ###
###    Rare logit
### ------------------- ###

## Purpose:
## Fit a logistic regression model using Firth's bias reduction method, equivalent to penalization of the 
## log-likelihood by the Jeffreys.  Firth's method was proposed as ideal
## solution to the problem of separation in logistic regression, see Heinze and Schemper (2002).
library(logistf)
rare_logit <-logistf(data=locscon_p, PA_transition ~ idea_criticize_1mo_t1 + 
                       rebhost_troopratio_lag +
                       gsv_mo_osv_lag + 
                       conf_poly1_lag +
                       host_anyTEK_lag +
                       mo_battles_n_lag +
                       gtd_attacks_n_lag +
                       host_vdem_polyarchy_lag +
                       host_rpe_gdp_lag +
                       host_vdem_svstterr_lag +
                       relpow_lag)

summary(rare_logit)

rare_logit_summary <- summary(rare_logit)
rare_logit_summary$ci.lower
rare_logit_summary_df <- data.frame("var" = rare_logit_summary$terms, 
                                    "coef" = rare_logit_summary$coefficients, 
                                    "ci.low95" = rare_logit_summary$ci.lower, 
                                    "ci.upr95" = rare_logit_summary$ci.upper)

## Edit summary: ---------------------------------------------------------------
## Add stars:
rare_logit_summary_df <- rare_logit_summary_df %>% 
  mutate(p = case_when((ci.low95 < 0 & ci.upr95< 0) |
                         (ci.low95 > 0 & ci.upr95> 0) ~ "***", TRUE ~ ""),
         var = case_when(var == "idea_criticize_1mo_t1" ~ "Bilateral Public Criticism (t+1)",
                         var == "host_rpe_gdp_lag" ~"Political Extraction (Host, t+1)", 
                         var == "host_vdem_svstterr_lag" ~"Territorial Authority (Host, t+1)",
                         var == "relpow_lag" ~  "Relative Power (Host-Target, t+1)",
                         var == "mo_battles_n_lag" ~ "Monthly Battles (Rebels-Target, t+1)", 
                         var == "gtd_attacks_n_lag" ~ "Monthly Terror (Rebels-Target, t+1)",
                         var == "rebhost_troopratio_lag" ~ "Troop Ratio (Rebels-Host, t+1)",
                         var == "host_vdem_polyarchy_lag" ~ "Democracy Score (Host, t+1)",
                         var == "host_anyTEK_lag" ~ "Politically Relevant Kin (Host, t+1)", 
                         var == "conf_poly1_lag" ~ "Conflict Time  (t+1)", 
                         var == "gsv_mo_osv_lag" ~ "Sanctuary Violence (t+1)", 
                         TRUE ~ var)) %>%
  select(-c(ci.low95, ci.upr95)) %>%
  rename(Variable = var)

# Remove the row names
rownames(rare_logit_summary_df) <- NULL

## § Print: Table  .............................................................   
options(knitr.kable.NA = '', knitr.table.format = "latex") # Remove ugly NAs
rare_logit_summary_latex <- kable(rare_logit_summary_df, format = 'latex', 
                                  booktabs = TRUE,
                                  bottomrule = '',
                                  toprule = '',
                                  midrule = '') %>%
  row_spec(0, hline_after = TRUE) %>% # Make line under first row
  row_spec(row = 0, # Make first row bold
           bold = TRUE) %>%
  column_spec(column = 1, # Add border
              border_left = TRUE) %>%
  column_spec(column = ncol(rare_logit_summary_df), # Add border
              border_right = TRUE) %>%
  collapse_rows(columns = 1, valign = "middle") %>%
  add_header_above(bold = TRUE, border_left = TRUE, border_right = TRUE) %>%
  kable_styling(font_size = 9)

### ----------------------------------------------------------------------------


### ------------------------------- ###
###          Robustness 2           ###
### ------------------------------- ###

### ------------------- ###
###    Different FE
### ------------------- ###

library(plm)

# Define the full model with host-fixed effects:
main_hostfixed <- plm(PA_transition ~ idea_criticize_1mo_t1 +
                        rebhost_troopratio_lag +
                        gsv_mo_osv_lag + 
                        conf_poly1_lag +
                        mo_battles_n_lag +
                        gtd_attacks_n_lag +
                        host_vdem_polyarchy_lag +
                        host_rpe_gdp_lag +
                        host_vdem_svstterr_lag +
                        relpow_lag, 
                      model = "within", effect = "twoways",
                      index = c("host_name"), data = locscon)


# Define the full model with target-fixed effects:
main_targetfixed <- plm(PA_transition ~ idea_criticize_1mo_t1 +
                          rebhost_troopratio_lag +
                          gsv_mo_osv_lag + 
                          conf_poly1_lag +
                          mo_battles_n_lag +
                          gtd_attacks_n_lag +
                          host_vdem_polyarchy_lag +
                          host_rpe_gdp_lag +
                          host_vdem_svstterr_lag +
                          relpow_lag, 
                        model = "within", effect = "twoways",
                        index = c("target_name"), data = locscon)

# Define the full model with host-fixed effects:
main_rebelfixed <- plm(PA_transition ~ idea_criticize_1mo_t1 +
                         rebhost_troopratio_lag +
                         gsv_mo_osv_lag + 
                         conf_poly1_lag +
                         mo_battles_n_lag +
                         gtd_attacks_n_lag +
                         host_vdem_polyarchy_lag +
                         host_rpe_gdp_lag +
                         host_vdem_svstterr_lag +
                         relpow_lag, 
                       model = "within", effect = "twoways",
                       index = c("IDActorID"), data = locscon)


summary(main_hostfixed)
summary(main_targetfixed)
summary(main_rebelfixed)



### ------------------------------- ###
###          Sensitivity 1          ###
### ------------------------------- ###

### ------------------- ###
###    Influential dyads
### ------------------- ###

library(plm)

# Define the full model (for baseline metrics):
baseline_mod <- plm(PA_transition ~ idea_criticize_1mo_t1 +
                      rebhost_troopratio_lag +
                      gsv_mo_osv_lag + 
                      conf_poly1_lag +
                      mo_battles_n_lag +
                      gtd_attacks_n_lag +
                      host_vdem_polyarchy_lag +
                      host_rpe_gdp_lag +
                      host_vdem_svstterr_lag +
                      relpow_lag, 
                    model = "within", effect = "twoways",
                    index = c("hostguest_id"), data = locscon_p)
#summary(baseline_mod) # exclude 0: n=5341

# Define your model formula
formula_lag <- PA_transition ~ idea_criticize_1mo_t1 +
  rebhost_troopratio_lag +
  gsv_mo_osv_lag + 
  conf_poly1_lag +
  mo_battles_n_lag +
  gtd_attacks_n_lag +
  host_vdem_polyarchy_lag +
  host_rpe_gdp_lag +
  host_vdem_svstterr_lag +
  relpow_lag

# Fit the full model to the entire dataset
full_model <- plm(formula_lag,
                  model = "within", effect = "twoways",
                  index = c("hostguest_id"), data = locscon_p)

# Compute absolute residuals
abs_residuals <- abs(resid(full_model))

# Create a dataframe linking residuals with hostguest_id
residuals_df <- data.frame(hostguest_id = index(full_model)$hostguest_id, 
                           residual = abs_residuals)

# Aggregate the maximum residual per dyad to determine influence
influential_dyads <- aggregate(residual ~ hostguest_id, data = residuals_df, max)

# Rank dyads by their maximum residual
influential_dyads <- influential_dyads[order(-influential_dyads$residual), ]

# Function to exclude top N influential dyads and rerun the model
run_exclusion_model <- function(exclude_n = 0) {
  # Identify dyads to exclude
  dyads_to_exclude <- influential_dyads$hostguest_id[1:exclude_n]
  
  # Exclude these dyads from the dataset
  data_excluded <- locscon_p[!locscon_p$hostguest_id %in% dyads_to_exclude, ]
  
  # Rerun the model on the reduced dataset
  model_excluded <- plm(formula_lag,
                        model = "within", effect = "twoways",
                        index = c("hostguest_id"),
                        data = data_excluded)
  
  return(summary(model_excluded))
}

#  Exclude the top 4 most influential dyads and rerun the model
results_excluded <- run_exclusion_model(8)

# Print the results
print(results_excluded) 
#4546/5341 = 85%


### ------------------------------- ###
###          Testing H2             ###
### ------------------------------- ###

## Upload duration spells for tolerated dyad-months: ---------------------------
locscon_active_sup <- readRDS(
  "[Insert your folder path here]/locs_durationspells_tolerated_v1.Rdata"
)

library(coxme)
library(ehahelper)
library(broom)
options(scipen=999)


### CoxMe: [A]  ----------------------------------------------------------------
A_switch_coxme <- coxme(Surv(ep_t0, ep_t1, as.numeric(suppression_switch)) ~ sanct_violence_ep2_sw + 
                          idea_mo_criticize_1mo_ep2_sw +
                          host_rpe_gdp_lag + 
                          host_vdem_svstterr_lag +
                          mo_battles_lag + 
                          gtd_attacks_n_lag +
                          host_vdem_polyarchy_lag +
                          rebhost_troopratio_lag +
                          relpow_lag
                        + (1|rebelid_iso3), # Two-way random effects 
                        locscon_active_sup)


### Extract results: [A]
A_switch_coxme_tidy <- tidy(A_switch_coxme)
A_switch_coxme_tidy$HR <- exp(A_switch_coxme_tidy$estimate)
A_switch_coxme_tidy <- A_switch_coxme_tidy %>%
  rename(Variable=term, coef=estimate, p=p.value) %>%
  mutate(val = case_when(p<0.01 ~ "***",
                         p<0.05 ~ "**",
                         p<0.1 ~ "*", 
                         TRUE ~ "")) %>%
  select(Variable, HR, p, val)
print(A_switch_coxme_tidy)


### CoxMe: [B]  ----------------------------------------------------------------
C_switch_coxme <- coxme(Surv(ep_t0, ep_t1, as.numeric(suppression_switch_dis)) ~ sanct_violence_ep2_sw + 
                          idea_mo_criticize_1mo_ep2_sw +
                          host_rpe_gdp_lag + 
                          host_vdem_svstterr_lag +
                          mo_battles_lag + 
                          gtd_attacks_n_lag +
                          host_vdem_polyarchy_lag +
                          rebhost_troopratio_lag +
                          relpow_lag
                        + (1|rebelid_iso3), # Two-way random effects 
                        locscon_active_sup)


### Extract results: [A]
C_switch_coxme_tidy <- tidy(C_switch_coxme)
C_switch_coxme_tidy$HR <- exp(C_switch_coxme_tidy$estimate)
C_switch_coxme_tidy <- C_switch_coxme_tidy %>%
  rename(Variable=term, coef=estimate, p=p.value) %>%
  mutate(val = case_when(p<0.01 ~ "***",
                         p<0.05 ~ "**",
                         p<0.1 ~ "*", 
                         TRUE ~ "")) %>%
  select(Variable, HR, p, val)
print(C_switch_coxme_tidy)

### Task 2: Check proportionality assumption: ----------------------------------

### Load font:
windowsFonts(Palatino=windowsFont("Palatino Linotype"))

### A: Test model 1: -----------------------------------------------------------
library(survminer)
coxass_Aswitch <- ggcoxzph(cox.zph(A_switch_coxme)) 

coxass_Aswitch$`1`$labels$y <- "Sanctuary Violence"
coxass_Aswitch$`2`$labels$y <- "Criticism"
coxass_Aswitch$`3`$labels$y <- "Extraction"
coxass_Aswitch$`4`$labels$y <- "Authority"
coxass_Aswitch$`5`$labels$y <- "Battles"
coxass_Aswitch$`6`$labels$y <- "Terror"
coxass_Aswitch$`7`$labels$y <- "Democracy"
coxass_Aswitch$`8`$labels$y <- "Troop Ratio"
coxass_Aswitch$`9`$labels$y <- "Relative Power"


# § Print: .....................................................................
ggpar(coxass_Aswitch, 
      font.family = "Palatino",
      font.x = c(7, "plain", "black"),
      font.y = c(7, "italic", "black"),
      font.main = c(8, "plain", "black"),
      font.title = c(8, "plain", "black"),
      font.submain = c(8, "plain", "black"),
      font.subtitle = c(7, "plain", "black"),
      font.tickslab = c(7, "plain", "black"),
      x.text.angle = c(90),
      rotate = F
)
# ..............................................................................
### Task A complete


### B: Test model 2: -----------------------------------------------------------
coxass_Cswitch <- ggcoxzph(cox.zph(C_switch_coxme)) 

coxass_Cswitch$`1`$labels$y <- "Sanctuary Violence"
coxass_Cswitch$`2`$labels$y <- "Criticism"
coxass_Cswitch$`3`$labels$y <- "Extraction"
coxass_Cswitch$`4`$labels$y <- "Authority"
coxass_Cswitch$`5`$labels$y <- "Battles"
coxass_Cswitch$`6`$labels$y <- "Terror"
coxass_Cswitch$`7`$labels$y <- "Democracy"
coxass_Cswitch$`8`$labels$y <- "Troop Ratio"
coxass_Cswitch$`9`$labels$y <- "Relative Power"

# § Print: .....................................................................
ggpar(coxass_Cswitch, 
      font.family = "Palatino",
      font.x = c(7, "plain", "black"),
      font.y = c(7, "italic", "black"),
      font.main = c(8, "plain", "black"),
      font.title = c(8, "plain", "black"),
      font.submain = c(8, "plain", "black"),
      font.subtitle = c(7, "plain", "black"),
      font.tickslab = c(7, "plain", "black"),
      x.text.angle = c(90),
      rotate = F
)


### Task complete --------------------------------------------------------------

### ----------------------------###
###       Plot   curves         ###
###-----------------------------###

### Define models:
# (A):
A_switch_clust <- coxph(Surv(ep_t0, ep_t1,  as.numeric(suppression_switch)) ~ sanct_violence_ep2_sw +
                          idea_mo_criticize_1mo_ep2_sw +
                          host_rpe_gdp_lag + 
                          host_vdem_svstterr_lag +
                          mo_best_lag + 
                          gtd_attacks_n_lag +
                          host_vdem_polyarchy_lag +
                          rebhost_troopratio_lag +
                          relpow_lag
                        + cluster(hostguest_id),
                        data=locscon_active_sup)


# (C):
C_switch_clust <- coxph(Surv(ep_t0, ep_t1,  as.numeric(suppression_switch_dis)) ~ sanct_violence_ep2_sw +
                          idea_mo_criticize_1mo_ep2_sw +
                          host_rpe_gdp_lag + 
                          host_vdem_svstterr_lag +
                          mo_best_lag + 
                          gtd_attacks_n_lag +
                          host_vdem_polyarchy_lag +
                          rebhost_troopratio_lag +
                          relpow_lag
                        + cluster(hostguest_id),
                        data=locscon_active_sup)

### Pred frame -----------------------------------------------------------------
A_switch_clust.preddf <- with(locscon_active_sup, 
                              data.frame(sanct_violence_ep2_sw=c(0,1),
                                         idea_mo_criticize_1mo_ep2_sw=rep(mean(idea_mo_criticize_1mo_ep2_sw, trim = 0.1, na.rm=T), 2),
                                         host_rpe_gdp_lag=rep(mean(host_rpe_gdp_lag, trim = 0.1, na.rm=T), 2),
                                         host_vdem_svstterr_lag=rep(mean(host_vdem_svstterr_lag, trim = 0.1, na.rm=T), 2),
                                         mo_best_lag=rep(mean(mo_best_lag, trim = 0.1, na.rm=T), 2),
                                         gtd_attacks_n_lag =rep(mean(gtd_attacks_n_lag, trim = 0.1, na.rm=T), 2),
                                         host_vdem_polyarchy_lag=rep(mean(host_vdem_polyarchy_lag, trim = 0.1, na.rm=T), 2),
                                         rebhost_troopratio_lag=rep(mean(rebhost_troopratio_lag, trim = 0.1, na.rm=T), 2),
                                         relpow_lag=rep(mean(relpow_lag,trim = 0.1, na.rm=T), 2)
                              ))

C_switch_clust.preddf <- with(locscon_active_sup, 
                              data.frame(sanct_violence_ep2_sw=c(0,1),
                                         idea_mo_criticize_1mo_ep2_sw=rep(mean(idea_mo_criticize_1mo_ep2_sw, trim = 0.1, na.rm=T), 2),
                                         host_rpe_gdp_lag=rep(mean(host_rpe_gdp_lag, trim = 0.1, na.rm=T), 2),
                                         host_vdem_svstterr_lag=rep(mean(host_vdem_svstterr_lag, trim = 0.1, na.rm=T), 2),
                                         mo_best_lag=rep(mean(mo_best_lag, trim = 0.1, na.rm=T), 2),
                                         gtd_attacks_n_lag =rep(mean(gtd_attacks_n_lag, trim = 0.1, na.rm=T), 2),
                                         host_vdem_polyarchy_lag=rep(mean(host_vdem_polyarchy_lag, trim = 0.1, na.rm=T), 2),
                                         rebhost_troopratio_lag=rep(mean(rebhost_troopratio_lag, trim = 0.1, na.rm=T), 2),
                                         relpow_lag=rep(mean(relpow_lag,trim = 0.1, na.rm=T), 2)
                              ))


## Define colours --------------------------------------------------------------
col_0 <- "dodgerblue4"
col_1 <- "wheat3"
col_2 <- "red"
col_3 <- "#CC6600"

## Define colours: Main Line
col_0 <- "dodgerblue4"
col_1 <- "wheat3"

###-----------------------------------------------------------------------------
## Load font
windowsFonts(Palatino=windowsFont("Palatino Linotype"))
par( ### Set standards
  mgp = c(2, 0.4, 0), # size of space between lables (axis title, axis labels, axis line)
  mfrow = c(1, 2),
  tcl = -0.3, ## Reduce the size of the tick marks.
  family="Palatino", font=1) ## Set font
plot(survfit(A_switch_clust, ## Model
             newdata=A_switch_clust.preddf), ## pdataframe
     conf.int=FALSE,
     lty=c(5, 1), ## type line 1, line 2
     lwd=c(1,1), ## size line 1, line 2
     ylim=c(0, 1.4), ## Y lim
     xlim=c(0,63), ## X lim
     cex.axis = c(0.8),
     cex.lab=c(0.8),
     xlab="Months Since Conflict Started",
     col=c(col_0,col_1),
     ylab="Share without Crackdowns", 
     bty = "n",  ## Remove the box around the plot.
     xaxt = "n", yaxt = "n")  ## Remove default x and y axis.
# Add horizontal grid lines
abline(h = seq(0, 1, by = 0.1), col = "gray88", lty = "dotted", lwd = .1)

# Add vertical grid lines
abline(v = c(0, 12, 24, 36, 48, 60), col = "gray88", lty = "dotted", lwd = .1)

# Draw box around the plot
box("plot", bty = "l", lwd = 2)

# Customize the x-axis with specified ticks and labels
axis(side = 1, at = c(0, 12, 24, 36, 48, 60), labels = c("0", "12", "24", "36", "48", "60"), 
     lwd = 0, lwd.ticks = 2, mgp = c(2.5, 0.2, 0))

# Customize the y-axis
axis(side = 2, at = seq(0, 1, by = 0.1), labels = seq(0, 1, by = 0.1),
     lwd = 0, lwd.ticks = 2, las = 2)


# Add legend
legend(x=1, y=1.05, # Adjust these values as needed
       legend=c("Sanctuary Violence=0", "Sanctuary Violence=1"), 
       col=c(col_0, col_1),
       lty=c(5, 1),
       x.intersp = 1.1, y.intersp = 1.35,
       xjust=0, yjust=0,
       cex=0.8)


### (2)
plot(survfit(C_switch_clust, newdata=C_switch_clust.preddf), 
     conf.int=FALSE,
     lty=c(5, 1), 
     lwd=c(1,1),
     ylim=c(0, 1.4),
     xlim=c(0,63),
     cex.axis = c(0.8),
     cex.lab=c(0.8),
     xlab="Months Since Conflict Started",
     col=c(col_0, col_1),
     ylab="Share without Successful Crackdown",
     bty = "n",
     xaxt = "n", yaxt = "n")

# Add horizontal grid lines
abline(h = seq(0, 1, by = 0.1), col = "gray88", lty = "dotted", lwd = .1)

# Add vertical grid lines
abline(v = c(0, 12, 24, 36, 48, 60), col = "gray88", lty = "dotted", lwd = .1)

# Draw box around the plot
box("plot", bty = "l", lwd = 2)

# Customize the x-axis with specified ticks and labels
axis(side = 1, at = c(0, 12, 24, 36, 48, 60), labels = c("0", "12", "24", "36", "48", "60"), 
     lwd = 0, lwd.ticks = 2, mgp = c(2.5, 0.2, 0))

# Customize the y-axis
axis(side = 2, at = seq(0, 1, by = 0.1), labels = seq(0, 1, by = 0.1),
     lwd = 0, lwd.ticks = 2, las = 2)

# Add legend:
legend(x=1, y=1.05, # Adjust these values as needed
       legend=c("Sanctuary Violence=0", "Sanctuary Violence=1"), 
       col=c(col_0, col_1),
       lty=c(5, 1),
       x.intersp = 1.1, y.intersp = 1.35,
       xjust=0, yjust=0,
       cex=0.8)



###-----------------------------------------------------------------------------


### ------------------------------- ###
###           Robustness 1          ###
### ------------------------------- ###

### ---------------------- ###
###    Pooled Cox PH
### ---------------------- ###
# (A):
A_switch_clust_tek <- coxph(Surv(ep_t0, ep_t1,  as.numeric(suppression_switch)) ~ sanct_violence_ep2_sw +
                              idea_mo_criticize_1mo_ep2_sw +
                              host_rpe_gdp_lag + 
                              host_vdem_svstterr_lag +
                              lag(host_anyTEK) +
                              mo_best_lag + 
                              gtd_attacks_n_lag +
                              host_vdem_polyarchy_lag +
                              rebhost_troopratio_lag +
                              relpow_lag
                            + cluster(hostguest_id),
                            data=locscon_active_sup)

summary(A_switch_clust_tek)


### ------------------------------- ###
###           Sensitivity 1         ###
### ------------------------------- ###

### ----------------------------- ###
###   Remove strong host states: All
### ----------------------------- ###

###  Upload V-Dem 


# Load V-Dem data:


## Make quantiles for Extraction:
statecap_qtls <- quantile(locscon_active_sup$host_rpe_gdp_lag, na.rm = T)
statecap_75th <- statecap_qtls[[4:4]]
locscon_active_sup_weak_gdp <- subset(locscon_active_sup, host_rpe_gdp_lag < statecap_75th)

## Make quantiles for Authority:
stateauth_qtls <- quantile(locscon_active_sup$host_vdem_svstterr_lag, na.rm = T)
stateauth_75th <- stateauth_qtls[[4:4]]
locscon_active_sup_weak_auth <- subset(locscon_active_sup, host_vdem_svstterr_lag < stateauth_75th)

# (A): Lower State capacity (Extraction):
A_switch_statecap <- coxme(Surv(ep_t0, ep_t1, as.numeric(suppression_switch)) ~ sanct_violence_ep2_sw + 
                             idea_mo_criticize_1mo_ep2_sw +
                             host_rpe_gdp_lag + 
                             host_vdem_svstterr_lag +
                             mo_battles_lag + 
                             gtd_attacks_n_lag +
                             host_vdem_polyarchy_lag +
                             rebhost_troopratio_lag +
                             relpow_lag
                           + (1|rebelid_iso3), # Two-way random effects 
                           locscon_active_sup_weak_gdp)

# (B): Lower State capacity (Authority):
A_switch_stateauth <- coxme(Surv(ep_t0, ep_t1, as.numeric(suppression_switch)) ~ sanct_violence_ep2_sw + 
                              idea_mo_criticize_1mo_ep2_sw +
                              host_rpe_gdp_lag + 
                              host_vdem_svstterr_lag +
                              mo_battles_lag + 
                              gtd_attacks_n_lag +
                              host_vdem_polyarchy_lag +
                              rebhost_troopratio_lag +
                              relpow_lag
                            + (1|rebelid_iso3), # Two-way random effects 
                            locscon_active_sup_weak_auth)

summary(A_switch_statecap)
summary(A_switch_stateauth)


### ------------------------------- ###
###           Sensitivity 2         ###
### ------------------------------- ###

### ----------------------------- ###
###   Remove strong host states: Dislodged
### ----------------------------- ###


# (A): Lower State capacity (Extraction):
A_switch_statecap_2 <- coxme(Surv(ep_t0, ep_t1, as.numeric(suppression_switch_dis)) ~ sanct_violence_ep2_sw + 
                               idea_mo_criticize_1mo_ep2_sw +
                               host_rpe_gdp_lag + 
                               host_vdem_svstterr_lag +
                               mo_battles_lag + 
                               gtd_attacks_n_lag +
                               host_vdem_polyarchy_lag +
                               rebhost_troopratio_lag +
                               relpow_lag
                             + (1|rebelid_iso3), # Two-way random effects 
                             locscon_active_sup_weak_gdp)

# (B): Lower State capacity (Authority):
A_switch_stateauth_2 <- coxme(Surv(ep_t0, ep_t1, as.numeric(suppression_switch_dis)) ~ sanct_violence_ep2_sw + 
                                idea_mo_criticize_1mo_ep2_sw +
                                host_rpe_gdp_lag + 
                                host_vdem_svstterr_lag +
                                mo_battles_lag + 
                                gtd_attacks_n_lag +
                                host_vdem_polyarchy_lag +
                                rebhost_troopratio_lag +
                                relpow_lag
                              + (1|rebelid_iso3), # Two-way random effects 
                              locscon_active_sup_weak_auth)

summary(A_switch_statecap_2)
summary(A_switch_stateauth_2)


### ------------------------------- ###
###          Sensitivity 3         ###
### ------------------------------- ###

### ---------------------- ###
###    Influential dyads
### ---------------------- ###

library(survival)
library(coxme)

# Fit the Cox PH model with random effects
A_switch_coxme <- coxme(Surv(ep_t0, ep_t1, as.numeric(suppression_switch)) ~ sanct_violence_ep2_sw +
                          idea_mo_criticize_1mo_ep2_sw +
                          host_rpe_gdp_lag + 
                          host_vdem_svstterr_lag +
                          mo_battles_lag + 
                          gtd_attacks_n_lag +
                          host_vdem_polyarchy_lag +
                          rebhost_troopratio_lag +
                          relpow_lag +
                          (1|rebelid_iso3),
                        locscon_active_sup)
A_switch_coxme # exclude 0: n=41, 3881

# Extract random effects
random_effects <- ranef(A_switch_coxme)$rebelid_iso3

# Function to exclude top N influential groups based on random effects and rerun the Cox PH model:
run_exclusion_model_coxph <- function(exclude_n = 0) {
  # Identify groups with the largest absolute random effects
  influential_groups <- names(sort(abs(random_effects), decreasing = TRUE)[1:exclude_n])
  
  # Exclude these groups from the dataset:
  data_excluded <- locscon_active_sup[!locscon_active_sup$rebelid_iso3 %in% influential_groups, ]
  
  # Rerun the Cox PH model on the reduced dataset:
  model_excluded <- coxme(Surv(ep_t0, ep_t1, as.numeric(suppression_switch)) ~ sanct_violence_ep2_sw +
                            idea_mo_criticize_1mo_ep2_sw +
                            host_rpe_gdp_lag + 
                            host_vdem_svstterr_lag +
                            mo_battles_lag + 
                            gtd_attacks_n_lag +
                            host_vdem_polyarchy_lag +
                            rebhost_troopratio_lag +
                            relpow_lag +
                            (1|rebelid_iso3),
                          data_excluded)
  
  return(summary(model_excluded))
}

# Exclude the top n most influential groups based on random effects and rerun the model:
results_excluded <- run_exclusion_model_coxph(8) # exclud 8 : n=29, 2841

# Print the results
print(results_excluded)






