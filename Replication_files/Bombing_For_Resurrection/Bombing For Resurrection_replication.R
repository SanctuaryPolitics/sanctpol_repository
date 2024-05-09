### ------------------------------- ###
###   Bombing For Resurrection      ###
###       Replication data          ###
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


### ------------------------------- ###
###         Figure 1                ###
### ------------------------------- ###

## Read in file of ULFA's attacks in GTD:
gtd_ulfa <- readRDS(
  "[Insert folder path here]/gtd_ulfa.Rdata"
)

## Complete panel:
gtd_ulfa <- gtd_ulfa %>%
  arrange(IDMonthly) %>%
  complete(IDMonthly = seq(as.yearmon("Jan 1989"), as.yearmon("Dec 2021"), by = 1 / 12), fill = list(rate = NA)) 

## Assign 0 to NA:
gtd_ulfa[c('attack_n_mo', 'nkill_n_mo', 'victims_n_mo')][is.na(
  gtd_ulfa[c('attack_n_mo', 'nkill_n_mo', 'victims_n_mo')])] <- 0

## Load packages:
library(ggplot2)
library(zoo) 

## Convert IDMonthly from yearmon to Date (assuming the first day of each month)
gtd_ulfa$IDMonthly <- as.Date(as.yearmon(gtd_ulfa$IDMonthly))

## Restrict X:
gtd_ulfa <- gtd_ulfa %>% filter(IDMonthly < "2006-12-01")


## Plot settings:
windowsFonts(Palatino=windowsFont("Palatino Linotype"))
My_Theme = theme(
  title = element_text(size = 9, family="Palatino"),
  plot.title.position = "plot",
  plot.caption = element_text(size = 5, hjust = 0,family="Palatino", face = "italic"),
  plot.caption.position = "plot",
  axis.title.x = element_text(size = 9.5, family="Palatino"),
  axis.text.x = element_text(size = 9, family="Palatino"),
  axis.text.y = element_text(size = 9, family="Palatino"),
  axis.title.y = element_text(size = 9.5, family="Palatino"),
  legend.title = element_text(size = 10, family="Palatino", face = "italic"),
  legend.text = element_text(size = 9, family="Palatino"),
  strip.text.x = element_text(size = 9, family = "Palatino"))
# Reduce the opacity of the grid lines ("gray92"):
col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255)

ulfa_plot <- ggplot(gtd_ulfa, aes(x = IDMonthly)) +
  #  geom_line(aes(y = attack_n_mo), color = "blue") + # Straight line for attacks
  geom_line(aes(y = victims_n_mo), color = "gray18", linetype = "dotted", size = 0.75) + # Dashed line for kills
  scale_x_date(limits = c(as.Date("1989-01-01"), as.Date("2005-12-01")), 
               date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20)) + # Adjusting y-axis
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # Removing grid lines
  labs(x = "Time", y = "Monthly Victims Count")

ulfa_plot + My_Theme

### ------------------------------- ###
###         Figure 2                ###
### ------------------------------- ###

## Read in file of summary data for LOCS, UCDP, and GTD (group numbers, battles, attacks) (Feb23_BFR_Trends):
trendframe <- readRDS(
  "[Insert folder path here]/locs_summarydata.Rdata"
)


### Settings -------------------------------------------------------------------
windowsFonts(Palatino=windowsFont("Palatino Linotype"))
My_Theme = theme(
  title = element_text(size = 9, family="Palatino"),
  plot.title.position = "plot",
  plot.caption = element_text(size = 6, hjust = 0,family="Palatino", face = "italic"),
  plot.caption.position = "plot",
  axis.title.x = element_text(size = 9.5, family="Palatino"),
  axis.text.x = element_text(size = 9, family="Palatino",  face = "italic"),
  axis.text.y = element_text(size = 9, family="Palatino"),
  axis.title.y = element_text(size = 9, family="Palatino"),
  legend.title = element_text(size = 9, family="Palatino", face = "italic"),
  legend.text = element_text(size = 9, family="Palatino"),
  strip.text.x = element_text(size = 9, family = "Palatino"))
# Reduce the opacity of the grid lines ("gray92"):
col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255)

### Implement-------------------------------------------------------------------
colors <- c("Purely Domestic" = "slategrey","Transnational" = 'rosybrown')
ggsoil <- ggplot(trendframe, aes(x=Cat)) +   
  geom_bar(aes(y= Hundred, fill = "Purely Domestic"), stat = "identity") + 
  geom_bar(aes(y= Transnational_share, fill = "Transnational"), stat = "identity") + 
  geom_text(aes(y = Hundred, label =scales::comma(Domestic_n)), vjust = 1.1, colour = "white", size = 2) + 
  geom_text(aes(y = Transnational_share, label = scales::comma(Transnational_n)), vjust = 1.1, colour = "white", size = 2) + 
  theme_classic() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(0, 1))  + 
  scale_fill_manual(values = colors, breaks = c("Purely Domestic", "Transnational")) + # Specify legend order here
  geom_hline(yintercept = 0.485, linetype="dotted", # Origo
             color = "white", size=0.5,) +
  guides(fill = guide_legend(title = "Type of Rebel Group")) +
  scale_x_discrete(position = "top") # Puts x axis on top

ggsoil + My_Theme + labs(x=NULL, y = "Share of Total",
                         color = "Type of Rebel Group")

### ------------------------------- ###
###         Figure 4                ###
### ------------------------------- ###


## Read in file of summary data for GTD attacks (Feb23_BFR_Trends):
terrlist_frame <- readRDS(
  "[Insert folder path here]/gtd_summarydata.Rdata"
)

## Prefer aggregate count of "Other" and sum up:
library(scales)
terrlist_frame <- terrlist_frame %>%
  mutate(Hundred = 1,
         Bombing_share_cum = Other_share+Hostage_share+Armed_share+Bombing_share,
         Armed_share_cum = Other_share+Hostage_share+Armed_share,
         Hostage_share_cum = Other_share+Hostage_share,
         Other_share_cum = Other_share)


### Settings -------------------------------------------------------------------
windowsFonts(Palatino=windowsFont("Palatino Linotype"))
My_Theme = theme(
  title = element_text(size = 9, family="Palatino"),
  plot.title.position = "plot",
  plot.caption = element_text(size = 6, hjust = 0,family="Palatino", face = "italic"),
  plot.caption.position = "plot",
  axis.title.x = element_text(size = 9.5, family="Palatino"),
  axis.text.x = element_text(size = 9, family="Palatino",  face = "italic"),
  axis.text.y = element_text(size = 9, family="Palatino"),
  axis.title.y = element_text(size = 9, family="Palatino"),
  legend.title = element_text(size = 9, family="Palatino", face = "italic"),
  legend.text = element_text(size = 9, family="Palatino"),
  strip.text.x = element_text(size = 9, family = "Palatino"))
# Reduce the opacity of the grid lines ("gray92"):
col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255)


# Ensure the colors are defined in the order you want them to appear in the legend
colors <- c("Armed Assault" = 'slategrey', "Bombing/Explosion" = 'rosybrown', "Hostage Taking" = 'wheat3', "Other" = 'gray80')
ggterr <- ggplot(terrlist_frame, aes(x=Cat)) +   
  geom_bar(aes(y= Hundred, fill = "Bombing/Explosion"), stat = "identity") + 
  geom_bar(aes(y= Armed_share_cum, fill = "Armed Assault"), stat = "identity") + 
  geom_bar(aes(y= Hostage_share_cum, fill = "Hostage Taking"), stat = "identity") + 
  geom_bar(aes(y= Other_share_cum, fill = "Other"), stat = "identity") + 
  theme_classic() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(0, 1)) +
  scale_fill_manual(values = colors, breaks = c("Bombing/Explosion", "Armed Assault", 
                                                "Hostage Taking", "Other")) + # Specify legend order here
  guides(fill = guide_legend(title = "Attack Type")) +
  scale_x_discrete(position = "top")

ggterr + My_Theme + labs(x = "Sampled Terror Attacks",
                         y = "Share of Total")

### ------------------------------- ###
###             H1                  ###
### ------------------------------- ###

### Upload data ----------------------------------------------------------------

## Read in file of 1989-2020 complete panel (Feb23_Bombing)
locscon <- readRDS(
  "[Insert folder path here]/locscon_dy_panel_v1.Rdata"
)

### Make panel -----------------------------------------------------------------
locscon_full <- pdata.frame(locscon, index=c("hostguest_id","IDMonthly"))


### ------------------------------- ###
###  Finding baseline probability   ###
### ------------------------------- ###

# Make the key model of interest:
PA_t1.twfe_pooled <- plm(targ_gtd_attacks_any ~ host_vdem_svstterr +
                           last_sanctuary +
                           mo_deaths_a+ 
                           dom_sanct+ 
                           target_vdem_polyarchy+ 
                           rebhost_troopratio +
                           rebtarg_troopratio,
                         model = "pooling", index = c("hostguest_id"),
                         data = locscon_full)


# Look at the model
summary(PA_t1.twfe_pooled)

# Make scenario data 
pframe_pooled <- data.frame(host_vdem_svstterr=mean(locscon_full$host_vdem_svstterr, na.rm=TRUE),
                            last_sanctuary=mean(locscon_full$last_sanctuary, na.rm=TRUE),
                            mo_deaths_a=mean(locscon_full$mo_deaths_a, na.rm=TRUE),
                            dom_sanct=mean(locscon_full$dom_sanct, na.rm=TRUE),
                            target_vdem_polyarchy=mean(locscon_full$target_vdem_polyarchy, na.rm=TRUE),
                            rebhost_troopratio=mean(locscon_full$rebhost_troopratio, na.rm=TRUE),
                            rebtarg_troopratio=mean(locscon_full$rebtarg_troopratio, na.rm=TRUE)
)


# Predict the probability at the mean level of covariates
baseline_probability <- predict(PA_t1.twfe_pooled, newdata = pframe_pooled, type = "response")
print(baseline_probability)

### ------------------------------- ###
###          Testing H1             ###
### ------------------------------- ###

### Definition:
### Two-way fixed-effects linear probability model (TWFE-LPM).

# Define all time lags including both 'mo' (lags) and 'lead' (leads):
time_lags <- c("6lead", "5lead","4lead", "3lead", "2lead", "1lead", "0mo", "1mo", "2mo", "3mo", 
               "4mo", "5mo", "6mo", "7mo", "8mo", "9mo", "10mo", "11mo", "12mo", 
               "13mo", "14mo", "15mo", "16mo", "17mo", "18mo")

# Initialise an empty list to store data frames
estframe_SR_list <- list()

# Loop to process each time lag
for (lag in time_lags) {
  iv_part <- ifelse(grepl("le", lag), paste0("factor(supponsetclean", lag, ")"), paste0("factor(supponsetclean", lag, ")"))
  
  # Construct the full model formula dynamically
  formula_lag <- as.formula(paste0("targ_gtd_attacks_any ~ ", iv_part, " + 
                                      lag(host_vdem_svstterr) +
                                      lag(last_sanctuary) +
                                      lag(mo_deaths_a)+ 
                                      lag(dom_sanct)+ 
                                      lag(target_vdem_polyarchy)+ 
                                      lag(leader_ideology)+
                                      lag(rebhost_troopratio) +
                                      lag(rebtarg_troopratio)"))
  
  # Model estimation
  model_name <- paste0("SR_", lag, ".twfe")
  eval(parse(text = paste0(model_name, " <- plm(formula_lag, model = 'within', effect = 'twoways', index = c('hostguest_id'), data = locscon_full)")))
  
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
  estframe_SR_list[[lag]] <- get(paste0(model_name, ".df"))
}

### ---------------------------------- ###
### Combine all data frames into one   ###
### ---------------------------------- ###

# Initialize vectors to store the extracted values
est <- lwr95 <- upr95 <- lwr90 <- upr90 <- lwr99 <- upr99 <- numeric(length(time_lags))

# Loop over the suffixes to extract values from the corresponding data frames
for (i in 1:length(time_lags)) {
  df_name <- paste0("SR_", time_lags[i], ".twfe.df")
  est[i] <- get(df_name)$Estimate[1]
  lwr95[i] <- get(df_name)$conf.low95[1]
  upr95[i] <- get(df_name)$conf.high95[1]
  lwr90[i] <- get(df_name)$conf.low90[1]
  upr90[i] <- get(df_name)$conf.high90[1]
  lwr99[i] <- get(df_name)$conf.low99[1]
  upr99[i] <- get(df_name)$conf.high99[1]
}

# Create the data frame
estframe_SR <- data.frame(
  month = c(-6:18),
  est = est,
  lwr95 = lwr95,
  upr95 = upr95,
  lwr90 = lwr90,
  upr90 = upr90,
  lwr99 = lwr99,
  upr99 = upr99
)

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

SR4_anyterr_plot <- ggplot(data=estframe_SR, 
                           aes(x = month, y = est)) +
  geom_hline(aes(yintercept = 0), # Origo
             colour = "black", size = 0.5, lty = 2, alpha = 0.7) +
  geom_vline(aes(xintercept = 0), # Origo
             colour = "black", size = 0.5, lty = 2, alpha = 0.7) +
  geom_linerange(aes(y=est, 
                     ymin=lwr90, ymax=upr90),
                 color =  ifelse((estframe_SR$lwr99 > 0 & estframe_SR$upr99 > 0) | 
                                   (estframe_SR$lwr99 < 0 & estframe_SR$upr99 < 0), "gray8",
                                 ifelse((estframe_SR$lwr95 > 0 & estframe_SR$upr95 > 0) | 
                                          (estframe_SR$lwr95 < 0 & estframe_SR$upr95 < 0), "gray56",
                                        "gray56")),
                 alpha=0.8, lwd=3, position=position_dodge(0.1)) +
  geom_linerange(aes(y=est, 
                     ymin=lwr95, ymax=upr95),
                 color =  ifelse((estframe_SR$lwr99 > 0 & estframe_SR$upr99 > 0) | 
                                   (estframe_SR$lwr99 < 0 & estframe_SR$upr99 < 0), "gray8",
                                 ifelse((estframe_SR$lwr95 > 0 & estframe_SR$upr95 > 0) | 
                                          (estframe_SR$lwr95 < 0 & estframe_SR$upr95 < 0), "gray56",
                                        "gray56")),
                 alpha=0.8, lwd=1.5, position=position_dodge(0.1)) +
  geom_linerange(aes(y=est, 
                     ymin=lwr99, ymax=upr99),
                 color =  ifelse((estframe_SR$lwr99 > 0 & estframe_SR$upr99 > 0) | 
                                   (estframe_SR$lwr99 < 0 & estframe_SR$upr99 < 0), "gray8",
                                 ifelse((estframe_SR$lwr95 > 0 & estframe_SR$upr95 > 0) | 
                                          (estframe_SR$lwr95 < 0 & estframe_SR$upr95 < 0), "gray56",
                                        "gray56")),
                 alpha=0.8, lwd=0.75, position=position_dodge(0.1)) +
  geom_point(aes(x=month, y=est), 
             colour = "black", fill = "white", size = 2, pch=21) 
#  + geom_line() 

# Plot:
SR4_anyterr_plot  +  theme_prism() + My_Theme + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 18)) +
  scale_color_manual(values = colors, name = "Confidence Interval") + # Legend
  theme(panel.grid.major.x = element_blank(), # vertical grids
        panel.grid.minor.y = element_line(color = "gray89", size = .01))+  # horizontal grid
  ylab("Change in Monthly Probability of Terror") + xlab("Months Since Crackdown Onset") +  #XYnames
  labs(caption = "Model: Two-way Fixed-Effects LPM. \nCovariates lagged to previous month.\nConfidence Intervals: Thickest=p$<$0.1, thinner=p$<$0.05; thinnest=p$<$0.01.")

### -------------------------- ###
###   Full regression results
###         H1 Main
### -------------------------- ###

suffixes <- c("3lead", "2lead", "1lead", "0mo", 
              "1mo", "2mo", "3mo", 
              "4mo", "5mo", "6mo")

for (suffix in suffixes) {
  model_name <- paste0("SR_", suffix, ".sum")
  data_name <- paste0("SR_", suffix, ".twfe.df")
  
  assign(model_name,
         get(data_name) %>%
           mutate(Variable = rownames(.)) %>%
           select(Variable, Estimate, `Pr(>|t|)`) %>%
           rename(p = `Pr(>|t|)`,
                  Est = Estimate))
}

library(purrr)
# List of model data frames
SR_sum <- mget(paste0("SR_", suffixes, ".sum"))

# Merge all data frames by "Variable" column
SR_sum_df <- reduce(SR_sum, full_join, by = "Variable")

# Define the desired order of suffixes
SR_varorder <- paste0("factor(supponsetclean", suffixes, ")1")
# Find unique values in the "Variable" column of SR_sum_df
additional_variables <- setdiff(unique(SR_sum_df$Variable), SR_varorder)

# Add the additional variables
SR_varorder <- c(SR_varorder, additional_variables)

# Find the indices of the rows according to varorder
row_indices <- match(SR_sum_df$Variable, SR_varorder)

# Arrange the rows in PA_sum_df according to PA_varorder
SR_sum_df <- SR_sum_df[order(row_indices), ]

## Clean names:
SR_sum_df <- SR_sum_df %>% 
  mutate(Variable = case_when(Variable == 'factor(supponsetclean3lead)1' ~ 'Crackdown Onset (t-3)',
                              Variable == 'factor(supponsetclean2lead)1' ~ 'Crackdown Onset (t-2)',
                              Variable == 'factor(supponsetclean1lead)1' ~ 'Crackdown Onset (t-1)',
                              Variable == 'factor(supponsetclean0mo)1' ~ 'Crackdown Onset (t)',
                              Variable == 'factor(supponsetclean1mo)1' ~ 'Crackdown Onset (t+1)',
                              Variable == 'factor(supponsetclean2mo)1' ~ 'Crackdown Onset (t+2)',
                              Variable == 'factor(supponsetclean3mo)1' ~ 'Crackdown Onset (t+3)',
                              Variable == 'factor(supponsetclean4mo)1' ~ 'Crackdown Onset (t+4)',
                              Variable == 'factor(supponsetclean5mo)1' ~ 'Crackdown Onset (t+5)',
                              Variable == 'factor(supponsetclean6mo)1' ~ 'Crackdown Onset (t+6)',
                              Variable == 'lag(host_vdem_svstterr)' ~ 'Host Territorial Reach',
                              Variable == 'lag(last_sanctuary)' ~ 'Last Sanctuary',
                              Variable == 'lag(mo_deaths_a)' ~ 'State Battle Deaths',
                              Variable == 'lag(dom_sanct)' ~ 'Domestic Territorial Control', 
                              Variable == 'lag(target_vdem_polyarchy)' ~ 'Democracy Score',
                              Variable == 'lag(leader_ideology)leftist' ~ 'Target State Ideology=Left',
                              Variable == 'lag(leader_ideology)other'  ~ 'Target State Ideology=Other',
                              Variable == 'lag(leader_ideology)rightist'  ~ 'Target State Ideology=Right',
                              Variable == 'lag(rebhost_troopratio)' ~ 'Rebel-Host Troop Ratio',
                              Variable == 'lag(rebtarg_troopratio)' ~ 'Rebel-Target Troop Ratio',
                              TRUE ~ Variable)) 


### Round to X decimals:
SR_sum_df <- SR_sum_df %>%
  mutate_at(vars(starts_with("p")), ~ round(., 5)) %>%
  mutate_at(vars(starts_with("Est")), ~ round(., 3)) 

# Replace NA with ""
SR_sum_df <- SR_sum_df %>%
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
SR_sum_def_modified <- merge_values(SR_sum_df)

# Change the name:
names(SR_sum_def_modified) <- gsub("Est", "Est (p)", names(SR_sum_def_modified))

# Remove p columns:
SR_sum_def_modified <- SR_sum_def_modified %>%
  select(-starts_with("p"))


## Remove merging labels:
names(SR_sum_def_modified) <- gsub("[xy\\.]", "", names(SR_sum_def_modified))

# Remove emptly parantheses:
SR_sum_def_modified <- apply(SR_sum_def_modified, c(1, 2), function(x) gsub("\\(\\)", "", x))

## Remove the row names:
rownames(SR_sum_def_modified) <- NULL

## § Print: Table  .............................................................                       
# Regression Results: The effect of Bilateral Public Criticism on the monthly probability of new Antirebel Pledge
options(knitr.kable.NA = '', knitr.table.format = "latex") # Remove ugly NAs
SR_sum_df_latex <- kable(SR_sum_def_modified, format = 'latex', 
                         booktabs = TRUE,
                         bottomrule = '',
                         toprule = '',
                         midrule = '') %>%
  row_spec(0, hline_after = TRUE) %>% # Make line under first row
  row_spec(row = 0, # Make first row bold
           bold = TRUE) %>%
  column_spec(column = 1, # Add border
              border_left = TRUE) %>%
  column_spec(column = ncol(SR_sum_def_modified), # Add border
              border_right = TRUE) %>%
  collapse_rows(columns = 1, valign = "middle") %>%
  add_header_above(bold = TRUE, border_left = TRUE, border_right = TRUE) %>%
  kable_styling(font_size = 6) %>% 
  footnote(alphabet = c("\\\\scriptsize{Model: Two-way Fixed-Effects Linear Panel Model.}", 
                        "\\\\scriptsize{All covariates lagged to previous month.}"), escape = FALSE) 

### ----------------------------------------------------------------------------

### -------------------- ###
###   TWFE Diagnostics   ###
### -------------------- ###

### Purpose: 
### We run two diagnostics: we (i) check for negative weights in treated units and 
### (ii) check for treatment effect homogeneity.

## Prepare data:
locscon_full_diag <- locscon %>% filter(!is.na(supponsetclean4mo) & !is.na(hostguest_id) & !is.na(IDMonthly))

## Run model:
trt_resid_primary <- lm(supponsetclean4mo ~ hostguest_id + factor(IDMonthly), data = locscon_full_diag)

## Extract residuals and weights:
fpe_primary_weights <- locscon_full_diag %>%
  mutate(treatment_resid = residuals(trt_resid_primary)) %>%
  mutate(treatment_weight = treatment_resid / sum(treatment_resid^2))

library(tidyverse)     # For ggplot2, dplyr, and friends
library(broom)         # For converting model objects to data frames
library(scales)


fpe_primary_weights %>%
  dplyr::summarize(twfe_beta_primary = sum(targ_gtd_attacks_any * treatment_weight))

## Task A: Do treated observations receive negative weights? -------------------
# Total treated in primary data
n_treated_primary <- fpe_primary_weights %>%
  filter(supponsetclean4mo == 1) %>%
  nrow()

# Negatively weighted treated observations in the primary data:
n_treated_negative_primary <- fpe_primary_weights %>%
  filter(treatment_weight < 0 & supponsetclean4mo == 1) %>%
  nrow()


negative_weights_df <- data.frame("Negative weights" = n_treated_negative_primary,
                                  "Negative weights per Treatment group" = n_treated_negative_primary/n_treated_primary)

## § Print: Table  .............................................................                       
#Negative weights received by treated observations:
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

## Task A completed ------------------------------------------------------------

## Task B: Is the homogeneity assumption violated? -----------------------------


## B.I: Graphical representation -----------------------------------------------
## Add residuals to data with weights:
out_resid_primary <- lm(supponsetclean4mo ~ hostguest_id + factor(IDMonthly), data = locscon_full_diag)
fpe_primary_weights <- fpe_primary_weights %>%
  mutate(out_resid = residuals(out_resid_primary))

# Graphical: -------------------------------------------------------------------
windowsFonts(Palatino=windowsFont("Palatino Linotype"))
My_Theme = theme(
  title = element_text(size = 8, family="Palatino"),
  plot.title.position = "plot",
  plot.caption = element_text(size = 8, hjust = 0,family="Palatino", face = "italic"),
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

plot_out_trt_primary <- ggplot(fpe_primary_weights,
                               aes(x = treatment_resid, y = out_resid, color = factor(supponsetclean4mo))) +
  # geom_point(size = 0.75, alpha = 0.5) +
  geom_smooth(aes(linetype = "Trend"), method = "loess", size = 1, se = FALSE, alpha = 0.5) +
  # geom_smooth(aes(linetype = "OLS"), method = "lm", se = FALSE) +
  scale_color_viridis_d(option = "rocket", begin = 0.2, end = 0.8,
                        labels = c("Untreated", "Treated")) +
  scale_linetype_manual(values = c("OLS" = "solid", "Trend" = "21"),
                        guide = guide_legend(override.aes = list(color = "grey30"))) +
  labs(x = "Residualised treatment", y = "Residualised outcome", color = NULL, linetype = NULL) +
  theme_minimal() + My_Theme + 
  theme(legend.position = "bottom")

## § Print: Figure .............................................................
plot_out_trt_primary
## .............................................................................

### ------------------ ###
###   Sensitivity 1    ###
### ------------------ ###

### --------------------------------------------------------------
###   Excluding (a) decapitating and (b) successful crackdowns
### --------------------------------------------------------------


options(scipen=999) # decimals
SR_excludesuccess.twfe <- plm(targ_gtd_attacks_any ~ factor(supponsetclean4mo_nondisl) + 
                                lag(host_vdem_svstterr) +
                                lag(last_sanctuary) +
                                lag(mo_deaths_a)+ 
                                lag(dom_sanct)+ 
                                lag(target_vdem_polyarchy)+ 
                                lag(leader_ideology)+
                                lag(rebhost_troopratio) +
                                lag(rebtarg_troopratio),
                              model = 'within', effect = 'twoways', 
                              index = c('hostguest_id',"IDMonthly"), data = locscon_full)

options(scipen=999) # decimals
SR_excludeleaderdecap.twfe <- plm(targ_gtd_attacks_any ~ factor(supponsetclean4mo_LC) + 
                                    lag(host_vdem_svstterr) +
                                    lag(last_sanctuary) +
                                    lag(mo_deaths_a)+ 
                                    lag(dom_sanct)+ 
                                    lag(target_vdem_polyarchy)+ 
                                    lag(leader_ideology)+
                                    lag(rebhost_troopratio) +
                                    lag(rebtarg_troopratio),
                                  model = 'within', effect = 'twoways', 
                                  index = c('hostguest_id',"IDMonthly"), data = locscon_full)


## Task 1: Full regression results:
summary(SR_excludesuccess.twfe)
summary(SR_excludeleaderdecap.twfe)


### ------------------ ###
###      Figure 6      ###
### ------------------ ###

# Define the relevant models:
time_lags_sens <- c("4mo_nondisl", "4mo_LC")

# Initialise an empty list to store data frames:
estframe_SR_list_sens <- list()

# Loop to process each model:
for (lag in time_lags_sens) {
  iv_part <- ifelse(grepl("le", lag), paste0("factor(supponsetclean", lag, ")"), paste0("factor(supponsetclean", lag, ")"))
  
  # Construct the full model formula dynamically
  formula_lag <- as.formula(paste0("targ_gtd_attacks_any ~ ", iv_part, " + 
                                      lag(host_vdem_svstterr) +
                                      lag(last_sanctuary) +
                                      lag(mo_deaths_a)+ 
                                      lag(dom_sanct)+ 
                                      lag(target_vdem_polyarchy)+ 
                                      lag(leader_ideology)+
                                      lag(rebhost_troopratio) +
                                      lag(rebtarg_troopratio)"))
  
  # Model estimation
  model_name <- paste0("SR_", lag, ".twfe")
  eval(parse(text = paste0(model_name, " <- plm(formula_lag, model = 'within', effect = 'twoways', index = c('hostguest_id'), data = locscon_full)")))
  
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
  estframe_SR_list_sens[[lag]] <- get(paste0(model_name, ".df"))
}

### ---------------------------------- ###
### Combine the data frames into one   ###
### ---------------------------------- ###

# Initialize vectors to store the extracted values
est <- lwr95 <- upr95 <- lwr90 <- upr90 <- lwr99 <- upr99 <- numeric(length(time_lags_sens))

# Loop over the suffixes to extract values from the corresponding data frames
for (i in 1:length(time_lags_sens)) {
  df_name <- paste0("SR_", time_lags_sens[i], ".twfe.df")
  est[i] <- get(df_name)$Estimate[1]
  lwr95[i] <- get(df_name)$conf.low95[1]
  upr95[i] <- get(df_name)$conf.high95[1]
  lwr90[i] <- get(df_name)$conf.low90[1]
  upr90[i] <- get(df_name)$conf.high90[1]
  lwr99[i] <- get(df_name)$conf.low99[1]
  upr99[i] <- get(df_name)$conf.high99[1]
}

# Create the data frame
estframe_sens <- data.frame(
  month = c("$t$+4", "$t$+4"),
  est = est,
  lwr95 = lwr95,
  upr95 = upr95,
  lwr90 = lwr90,
  upr90 = upr90,
  lwr99 = lwr99,
  upr99 = upr99
)
# Split in two
estframe_sens_decap <- estframe_sens[2, ]
estframe_sens_dislo <- estframe_sens[1, ]


windowsFonts(Palatino=windowsFont("Palatino Linotype"))
My_Theme = theme(
  title = element_text(size = 10, family="Palatino"),
  plot.title.position = "plot",
  plot.caption = element_text(size = 7, hjust = 0,family="Palatino", face = "italic"),
  plot.caption.position = "plot",
  axis.title.x = element_text(size = 8, family="Palatino", face = "bold"),
  axis.text.x = element_text(size = 8, family="Palatino"),
  axis.text.y = element_text(size = 8, family="Palatino"),
  axis.title.y = element_text(size = 8, family="Palatino"),
  legend.title = element_text(size = 11, family="Palatino", face = "italic"),
  legend.text = element_text(size = 10, family="Palatino"),
  strip.text.x = element_text(size = 13, family = "Palatino"))
# Reduce the opacity of the grid lines ("gray92"):
col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255)

colors <- c("CI 99" = "gray8", "CI 95" = "gray50")


## (A) Make Decapitation plot: -------------------------------------------------
SR4sens1_anyterr_plot <- ggplot(data=estframe_sens_decap, 
                                aes(x = month, y = est)) +
  geom_hline(aes(yintercept = 0), # Origo
             colour = "black", size = 0.5, lty = 2, alpha = 0.7) +
  # geom_vline(aes(xintercept = 0), # Origo
  #                 colour = "black", size = 0.5, lty = 2, alpha = 0.7) +
  geom_linerange(aes(y=est, 
                     ymin=lwr90, ymax=upr90),
                 color =  ifelse((estframe_sens_decap$lwr99 > 0 & estframe_sens_decap$upr99 > 0) | 
                                   (estframe_sens_decap$lwr99 < 0 & estframe_sens_decap$upr99 < 0), "gray8",
                                 ifelse((estframe_sens_decap$lwr95 > 0 & estframe_sens_decap$upr95 > 0) | 
                                          (estframe_sens_decap$lwr95 < 0 & estframe_sens_decap$upr95 < 0), "gray56",
                                        "gray56")),
                 alpha=0.8, lwd=3, position=position_dodge(0.1)) +
  geom_linerange(aes(y=est, 
                     ymin=lwr95, ymax=upr95),
                 color =  ifelse((estframe_sens_decap$lwr99 > 0 & estframe_sens_decap$upr99 > 0) | 
                                   (estframe_sens_decap$lwr99 < 0 & estframe_sens_decap$upr99 < 0), "gray8",
                                 ifelse((estframe_sens_decap$lwr95 > 0 & estframe_sens_decap$upr95 > 0) | 
                                          (estframe_sens_decap$lwr95 < 0 & estframe_sens_decap$upr95 < 0), "gray56",
                                        "gray56")),
                 alpha=0.8, lwd=1.5, position=position_dodge(0.1)) +
  geom_linerange(aes(y=est, 
                     ymin=lwr99, ymax=upr99),
                 color =  ifelse((estframe_sens_decap$lwr99 > 0 & estframe_sens_decap$upr99 > 0) | 
                                   (estframe_sens_decap$lwr99 < 0 & estframe_sens_decap$upr99 < 0), "gray8",
                                 ifelse((estframe_sens_decap$lwr95 > 0 & estframe_sens_decap$upr95 > 0) | 
                                          (estframe_sens_decap$lwr95 < 0 & estframe_sens_decap$upr95 < 0), "gray56",
                                        "gray56")),
                 alpha=0.8, lwd=0.75, position=position_dodge(0.1)) +
  geom_point(aes(x=month, y=est), 
             colour = "black", fill = "white", size = 2, pch=21) +
  theme_prism() + My_Theme + 
  scale_color_manual(values = colors, name = "Confidence Interval") + # Legend
  scale_y_continuous(limits = c(-0.2, 0.3),breaks = scales::pretty_breaks(n = 10)) +
  theme(panel.grid.major.x = element_blank(), # vertical grids
        panel.grid.minor.y = element_line(color = "gray89", size = .01))+  # horizontal grid
  ylab("Change in Monthly Probability of Terror") + xlab("(A) Excluding Decapitation") +  #XYnames
  labs(caption = "Model: Two-way Fixed-Effects LPM. \nCovariates lagged to previous month.\nConfidence Intervals: Thickest=p$<$0.1, thinner=p$<$0.05; thinnest=p$<$0.01.")




## (B) Make Dislodged plot: ----------------------------------------------------
SR4sens2_anyterr_plot <- ggplot(data=estframe_sens_dislo, 
                                aes(x = month, y = est)) +
  geom_hline(aes(yintercept = 0), # Origo
             colour = "black", size = 0.5, lty = 2, alpha = 0.7) +
  # geom_vline(aes(xintercept = 0), # Origo
  #                 colour = "black", size = 0.5, lty = 2, alpha = 0.7) +
  geom_linerange(aes(y=est, 
                     ymin=lwr90, ymax=upr90),
                 color =  ifelse((estframe_sens_decap$lwr99 > 0 & estframe_sens_decap$upr99 > 0) | 
                                   (estframe_sens_decap$lwr99 < 0 & estframe_sens_decap$upr99 < 0), "gray8",
                                 ifelse((estframe_sens_decap$lwr95 > 0 & estframe_sens_decap$upr95 > 0) | 
                                          (estframe_sens_decap$lwr95 < 0 & estframe_sens_decap$upr95 < 0), "gray56",
                                        "gray56")),
                 alpha=0.8, lwd=3, position=position_dodge(0.1)) +
  geom_linerange(aes(y=est, 
                     ymin=lwr95, ymax=upr95),
                 color =  ifelse((estframe_sens_decap$lwr99 > 0 & estframe_sens_decap$upr99 > 0) | 
                                   (estframe_sens_decap$lwr99 < 0 & estframe_sens_decap$upr99 < 0), "gray8",
                                 ifelse((estframe_sens_decap$lwr95 > 0 & estframe_sens_decap$upr95 > 0) | 
                                          (estframe_sens_decap$lwr95 < 0 & estframe_sens_decap$upr95 < 0), "gray56",
                                        "gray56")),
                 alpha=0.8, lwd=1.5, position=position_dodge(0.1)) +
  geom_linerange(aes(y=est, 
                     ymin=lwr99, ymax=upr99),
                 color =  ifelse((estframe_sens_decap$lwr99 > 0 & estframe_sens_decap$upr99 > 0) | 
                                   (estframe_sens_decap$lwr99 < 0 & estframe_sens_decap$upr99 < 0), "gray8",
                                 ifelse((estframe_sens_decap$lwr95 > 0 & estframe_sens_decap$upr95 > 0) | 
                                          (estframe_sens_decap$lwr95 < 0 & estframe_sens_decap$upr95 < 0), "gray56",
                                        "gray56")),
                 alpha=0.8, lwd=0.75, position=position_dodge(0.1)) +
  geom_point(aes(x=month, y=est), 
             colour = "black", fill = "white", size = 2, pch=21) +  
  theme_prism() + 
  My_Theme + 
  scale_color_manual(values = colors, name = "Confidence Interval") + # Legend
  scale_y_continuous(limits = c(-0.2, 0.3),breaks = scales::pretty_breaks(n = 10)) +
  theme(panel.grid.major.x = element_blank(), # vertical grids
        panel.grid.minor.y = element_line(color = "gray89", size = .01))+  # horizontal grid
  ylab("") + xlab("(B) Excluding Successful Crackdowns")   +  #XYnames
  labs(caption = " \n \n")#XYnames


library(gridExtra)
grid.arrange(SR4sens1_anyterr_plot, SR4sens2_anyterr_plot, ncol = 2)

### -------------------------- ###
###        Hypotheis 2         ###
### -------------------------- ###

### -------------------------- ###
###         Table 5
### -------------------------- ###

## Upload event data set at attack-level: --------------------------------------
gtdgeo_events  <- readRDS(
  "[Insert folder path here]/gtdgeo_events_cov_v1.Rdata"
)


## Disable scientific notation:
options(scipen=999)

## Run unit-FE LPaM
events.plm <- plm(mindist ~ CON_suppressed +
                    gtd_other_last6_any + 
                    capdist + 
                    gcp_mer + 
                    urban_ih + 
                    ttime_mean + 
                    gedbatt_mindist_momean_imputed_fill +
                    last_sanctuary + 
                    dom_sanct + 
                    long_1 +
                    long_2 + 
                    long_3 + 
                    lat_1 + 
                    lat_2 + 
                    lat_3, 
                  data = gtdgeo_events, 
                  index = "hostguest_id",  # Specify the individual index
                  model = "within",        # Use "within" model for unit-fixed effects
                  effect = "individual")   # Specify individual effects 
summary(events.plm)

### -------------------- ###
###   TWFE Diagnostics   ###
### -------------------- ###

### Purpose: 
### We run two diagnostics: we (i) check for negative weights in treated units and 
### (ii) check for treatment effect homogeneity.
gtdgeo_events_diag <- gtdgeo_events %>% filter(!is.na(CON_suppressed) & !is.na(hostguest_id) & !is.na(IDMonthly))

trt_resid_primary <- lm(CON_suppressed ~ hostguest_id + IDMonthly, data = gtdgeo_events_diag)

fpe_primary_weights <- gtdgeo_events_diag %>%
  mutate(treatment_resid = residuals(trt_resid_primary)) %>%
  mutate(treatment_weight = treatment_resid / sum(treatment_resid^2))

fpe_primary_weights %>%
  dplyr::summarize(twfe_beta_primary = sum(mindist * treatment_weight))


## Task A: Do treated observations receive negative weights? -------------------
n_treated_primary <- fpe_primary_weights %>%
  filter(CON_suppressed == 1) %>%
  nrow()

# Negatively weighted treated observations in the primary data
n_treated_negative_primary <- fpe_primary_weights %>%
  filter(treatment_weight < 0 & CON_suppressed == 1) %>%
  nrow()


negative_weights_df <- data.frame("Negative weights" = n_treated_negative_primary,
                                  "Negative weights per Treatment group" = n_treated_negative_primary/n_treated_primary)
negative_weights_df

## Task B: Is the homogeneity assumption violated? -----------------------------

## B.I: Graphical representation -----------------------------------------------
## Add residuals to data with weights:
out_resid_primary <- lm(CON_suppressed ~ hostguest_id + factor(IDMonthly), data = gtdgeo_events_diag)
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
                                aes(x = treatment_resid, y = out_resid, color = factor(CON_suppressed))) +
    #geom_point(size = 0.75, alpha = 0.5) +
    # geom_smooth(aes(linetype = "Loess"), method = "loess", size = 1, se = FALSE, alpha = 0.5) +
    geom_smooth(aes(linetype = "Trend"), method = "lm", se = FALSE) +
    scale_color_viridis_d(option = "rocket", begin = 0.2, end = 0.8,
                          labels = c("Untreated", "Treated")) +
    scale_linetype_manual(values = c("Trend" = "solid"),
                          guide = guide_legend(override.aes = list(color = "grey30"))) +
    labs(x = "Residualised treatment", y = "Residualised outcome",color = NULL, linetype = NULL) +
    theme_minimal() + My_Theme + 
    theme(legend.position = "bottom"))

## § Print: Figure .............................................................
#plot_out_trt_primary
## .............................................................................

### ------------------ ###
###   Robustness 1     ###
### ------------------ ###

### ------------------------------
###   Matching 
### ------------------------------

## Definition:
## Matching with Weighted Difference-in-Differences with Propensity Score Matches created with 6 lags.

## Task 1: Inspect data: -------------------------------------------------------
library(PanelMatch)

## Check for dupes:
dupes <- locscon_full %>% 
  group_by(hostguest_id, samplestart_tae) %>% 
  filter(n()>1) %>%
  ungroup()
ifelse(nrow(dupes) != 0, stop("Prob with dupes"), "No dupes")

## Make new ID:
locscon_full <- locscon_full %>%
  mutate(hostguest_id_num = as.numeric(gsub("-", "", hostguest_id)))


## Task 2: Perform matching: ---------------------------------------------------

# Call PanelMatch without any refinement (for comparison):
PM.results.none <- PanelMatch(lag = 6,  unit.id = "hostguest_id_num", time.id = "samplestart_tae",
                              treatment = "crackdown_onset", refinement.method = "none", 
                              data = locscon_full, match.missing = TRUE, 
                              size.match = 7, qoi = "att" ,outcome.var = "targ_gtd_attacks_any",
                              lead = 0, forbid.treatment.reversal = FALSE)

# Call PanelMatch with Mahalanobis matching (for comparison):
PM.results.maha <- PanelMatch(lag = 6, time.id = "samplestart_tae", unit.id = "hostguest_id_num",
                              treatment = "crackdown_onset", refinement.method = "mahalanobis",
                              data = locscon_full, match.missing = TRUE, covs.formula = ~ rebtarg_troopratio,
                              size.match = 7, qoi = "att" , outcome.var = "targ_gtd_attacks_any",
                              lead = 0:6, forbid.treatment.reversal = FALSE,
                              use.diagonal.variance.matrix = TRUE)

# Call PanelMatch with Propsensity Score weighted matching (for use):
PM.results.ps.weight <- PanelMatch(lag = 6, time.id = "samplestart_tae", unit.id = "hostguest_id_num",
                                   treatment = "crackdown_onset", refinement.method = "ps.weight",
                                   data = locscon_full, match.missing = FALSE, listwise.delete = TRUE,
                                   covs.formula = ~ I(lag(rebtarg_troopratio, 1:6)) + I(lag(targ_gtd_attacks_any, 1:6)),
                                   size.match = 7, qoi = "att", outcome.var = "targ_gtd_attacks_any",
                                   lead = 0:6, forbid.treatment.reversal = FALSE)



#Extract the matched.set objects
msets.none.weight <- PM.results.none$att
msets.maha.weight <- PM.results.maha$att
msets.ps.weight <- PM.results.ps.weight$att


## Visualise set sizes ---------------------------------------------------------

# § Print Figure ...............................................................
plot(msets.ps.weight)  # Plot distribution
# ..............................................................................
# Task complete.

## Task 3: Print results: ------------------------------------------------------
#summary(msets.ps.weight) # Summary: 97 treated units 


PE.results <- PanelEstimate(sets = PM.results.ps.weight, data = locscon_full,
                            se.method = "bootstrap",
                            number.iterations = 1000,
                            confidence.level = .95)

## Convert to data frame:
PE_results.df <- data.frame("Est" = PE.results[["estimates"]],
                            "")
PE_results.df <- summary(PE.results)$summary
PE_results.df <- data.frame(PE_results.df)

## Clean data frame:
PE_results.df <- PE_results.df %>%
  rename(Est_2.5 = X2.5.,
         Est_97.5 = X97.5.,
         Est = estimate,
         Std.Error = std.error) %>%
  mutate(p = case_when((Est_2.5 < 0 & Est_97.5< 0) |
                         (Est_2.5 > 0 & Est_97.5> 0) ~ "***", TRUE ~ ""))

## § Print: Table  .............................................................   
options(knitr.kable.NA = '', knitr.table.format = "latex") # Remove ugly NAs
PE_results.df_latex <- kable(PE_results.df, format = 'latex', 
                             booktabs = TRUE,
                             bottomrule = '',
                             toprule = '',
                             midrule = '') %>%
  row_spec(0, hline_after = TRUE) %>% # Make line under first row
  row_spec(row = 0, # Make first row bold
           bold = TRUE) %>%
  column_spec(column = 1, # Add border
              border_left = TRUE) %>%
  column_spec(column = ncol(PE_results.df), # Add border
              border_right = TRUE) %>%
  collapse_rows(columns = 1, valign = "middle") %>%
  add_header_above(bold = TRUE, border_left = TRUE, border_right = TRUE) %>%
  kable_styling(font_size = 9) %>% 
  footnote(alphabet = c("\\\\scriptsize{*** = p$<$0.05 (computed with conditional method.)}",
                        "\\\\scriptsize{Est = Average Treatment Effect on the Treated (ATT) by Period.}",
                        "\\\\scriptsize{Method: Propensity Score Matches (97 treated units).}"), escape = FALSE) 
### ----------------------------------------------------------------------------
### Task complete.
### Task 4: Plot results -------------------------------------------------------


# § Print Figure ...............................................................
plot(PE.results)
# ..............................................................................
### Task complete. 

### ---------------------------- ###
###   Robustness Hypothesis 1    ###
### ---------------------------- ###

### ------------------ ###
###   Sensitivity 1    ###
### ------------------ ###

### ------------------------------------------------------------------------------
###   Excluding (a) weak rebels and (b) rebels with alternative sanctuaries
### ------------------------------------------------------------------------------

### Upload data ----------------------------------------------------------------

## Read in file of 1989-2020 complete panel (Feb23_Bombing)
locscon <- readRDS(
  "[Insert folder path here]/locscon_dy_panel_v1.Rdata"
)


### Make subsets

## Exclude dyadmonths where the rebels were in the lowest quantile of relative strength (new n=13279):
locscon_full_strongreb <- locscon %>%
  filter(rebtarg_troopratio < quantile(rebtarg_troopratio, 0.75, na.rm = TRUE))

locscon_full_altsanct <- locscon %>%
  filter(last_sanctuary==0)

### Make panels:
locscon_full_strongreb <- pdata.frame(locscon_full_strongreb, index=c("hostguest_id","IDMonthly"))
locscon_full_altsanct <- pdata.frame(locscon_full_altsanct, index=c("hostguest_id","IDMonthly"))


options(scipen=999) # decimals
SR_strongreb.twfe <- plm(targ_gtd_attacks_any ~ factor(supponsetclean4mo) + 
                           lag(host_vdem_svstterr) +
                           lag(last_sanctuary) +
                           lag(dom_sanct) + 
                           lag(mo_deaths_a)+ 
                           lag(target_vdem_polyarchy)+ 
                           lag(leader_ideology)+
                           lag(rebhost_troopratio),
                         model = 'within', effect = 'twoways', 
                         index = c('hostguest_id',"IDMonthly"), data = locscon_full_strongreb)

options(scipen=999) # decimals
SR_altsanct.twfe <- plm(targ_gtd_attacks_any ~ factor(supponsetclean4mo) + 
                          lag(host_vdem_svstterr) +
                          lag(mo_deaths_a)+ 
                          lag(dom_sanct) + 
                          lag(target_vdem_polyarchy)+ 
                          lag(leader_ideology)+
                          lag(rebhost_troopratio) + 
                          lag(rebtarg_troopratio),
                        model = 'within', effect = 'twoways', 
                        index = c('hostguest_id',"IDMonthly"), data = locscon_full_altsanct)
summary(SR_strongreb.twfe)
summary(SR_altsanct.twfe)

### ------------------------------- ###
###          Sensitivity 2          ###
### ------------------------------- ###

### ----------------------------- ###
###   Exclude Influential dyads
### ----------------------------- ###

library(plm)

# Define the full model (for baseline metrics):
baseline_mod <- plm(targ_gtd_attacks_any ~ factor(supponsetclean4mo) + 
                      lag(host_vdem_svstterr) +
                      lag(last_sanctuary) +
                      lag(mo_deaths_a)+ 
                      lag(dom_sanct)+ 
                      lag(target_vdem_polyarchy)+ 
                      lag(leader_ideology)+
                      lag(rebhost_troopratio) +
                      lag(rebtarg_troopratio),
                    model = 'within', effect = 'twoways', 
                    index = c('hostguest_id',"IDMonthly"), data = locscon_full)
summary(baseline_mod) # exclude 0: n=15414

# Define your model formula
formula_lag <- targ_gtd_attacks_any ~ factor(supponsetclean4mo) + 
  lag(host_vdem_svstterr) +
  lag(last_sanctuary) +
  lag(mo_deaths_a)+ 
  lag(dom_sanct)+ 
  lag(target_vdem_polyarchy)+ 
  lag(leader_ideology)+
  lag(rebhost_troopratio) +
  lag(rebtarg_troopratio)

# Fit the full model to the entire dataset
full_model <- plm(formula_lag,
                  model = "within", effect = "twoways",
                  index = c("hostguest_id"), data = locscon_full)

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
  data_excluded <- locscon_full[!locscon_full$hostguest_id %in% dyads_to_exclude, ]
  
  # Rerun the model on the reduced dataset
  model_excluded <- plm(formula_lag,
                        model = "within", effect = "twoways",
                        index = c("hostguest_id"),
                        data = data_excluded)
  
  return(summary(model_excluded))
}

#  Exclude the top 10% most influential dyads and rerun the model
results_excluded <- run_exclusion_model(9)

# Print the results
print(results_excluded) 

### ---------------------------- ###
###   Robustness Hypothesis 2    ###
### ---------------------------- ###

## Upload event data set at attack-level: --------------------------------------
gtdgeo_events  <- readRDS(
  "[Insert folder path here]/gtdgeo_events_cov_v1.Rdata"
)


## Disable scientific notation:
options(scipen=999)

### ------------------------------- ###
###          Sensitivity 1          ###
### ------------------------------- ###


### --------------------------------- ###
###  Alternative geo-specifications   ###
### --------------------------------- ###

## Make dummy for attacks last month:
gtdgeo_events <- gtdgeo_events %>%
  mutate(gtd_other_last1_any = ifelse(gtd_other_1mo > 0, 1, 0))


## Run unit-FE LPaM for (1) one-month coding:
events_1mo.plm <- plm(mindist ~ CON_suppressed +
                        gtd_other_last1_any + 
                        capdist + 
                        gcp_mer + 
                        urban_ih + 
                        ttime_mean + 
                        gedbatt_mindist_momean_imputed_fill +
                        last_sanctuary + 
                        dom_sanct + 
                        long_1 +
                        long_2 + 
                        long_3 + 
                        lat_1 + 
                        lat_2 + 
                        lat_3, 
                      data = gtdgeo_events, 
                      index = "hostguest_id",  # Specify the individual index
                      model = "within",        # Use "within" model for unit-fixed effects
                      effect = "individual")   # Specify individual effects 

## Run unit-FE LPaM for (2) twelve-month coding:
events_12mo.plm <- plm(mindist ~ CON_suppressed +
                         gtd_other_last12_any + 
                         capdist + 
                         gcp_mer + 
                         urban_ih + 
                         ttime_mean + 
                         gedbatt_mindist_momean_imputed_fill +
                         last_sanctuary + 
                         dom_sanct + 
                         long_1 +
                         long_2 + 
                         long_3 + 
                         lat_1 + 
                         lat_2 + 
                         lat_3, 
                       data = gtdgeo_events, 
                       index = "hostguest_id",  # Specify the individual index
                       model = "within",        # Use "within" model for unit-fixed effects
                       effect = "individual")   # Specify individual effects 

summary(events_1mo.plm)
summary(events_12mo.plm)

### ------------------------------- ###
###          Sensitivity 2          ###
### ------------------------------- ###

### -------------------- ###
###  Negative weights    ###
### -------------------- ###

### Purpose: 
### We run two diagnostics: we (i) check for negative weights in treated units and 
### (ii) check for treatment effect homogeneity.
gtdgeo_events_diag <- gtdgeo_events %>% filter(!is.na(CON_suppressed) & !is.na(hostguest_id) & !is.na(IDMonthly))

trt_resid_primary <- lm(CON_suppressed ~ hostguest_id + IDMonthly, data = gtdgeo_events_diag)

fpe_primary_weights <- gtdgeo_events_diag %>%
  mutate(treatment_resid = residuals(trt_resid_primary)) %>%
  mutate(treatment_weight = treatment_resid / sum(treatment_resid^2))

fpe_primary_weights %>%
  dplyr::summarize(twfe_beta_primary = sum(mindist * treatment_weight))


## Task A: Do treated observations receive negative weights? -------------------
n_treated_primary <- fpe_primary_weights %>%
  filter(CON_suppressed == 1) %>%
  nrow()

# Negatively weighted treated observations in the primary data
n_treated_negative_primary <- fpe_primary_weights %>%
  filter(treatment_weight < 0 & CON_suppressed == 1) %>%
  nrow()


negative_weights_df <- data.frame("Negative weights" = n_treated_negative_primary,
                                  "Negative weights per Treatment group" = n_treated_negative_primary/n_treated_primary)
negative_weights_df

### -------------------- ###
###      Replicate       ###
### -------------------- ###

## Definition:
## Replicate results of Table 5 without attacks that received negative weights.   

## Identify attacks with negative weights:
gtdgeo_events_negwe <- fpe_primary_weights %>%
  filter(treatment_weight < 0 & CON_suppressed == 1) %>%
  select(eventid_iso3, everything())

## Remove these attacks from our sample:
gtdgeo_events_poswe <- filter(gtdgeo_events, !(eventid_iso3 %in% gtdgeo_events_negwe$eventid_iso3))

## Disable scientific notation:
options(scipen=999)

## Run unit-FE LPaM
events_poswe.plm <- plm(mindist ~ CON_suppressed +
                          gtd_other_last6_any + 
                          capdist + 
                          gcp_mer + 
                          urban_ih + 
                          ttime_mean + 
                          gedbatt_mindist_momean_imputed_fill +
                          last_sanctuary + 
                          dom_sanct + 
                          long_1 +
                          long_2 + 
                          long_3 + 
                          lat_1 + 
                          lat_2 + 
                          lat_3, 
                        data = gtdgeo_events_poswe, 
                        index = "hostguest_id",  # Specify the individual index
                        model = "within",        # Use "within" model for unit-fixed effects
                        effect = "individual")   # Specify individual effects 
summary(events_poswe.plm)

## Task B: Is the homogeneity assumption violated? -----------------------------


### -------------------- ###
###   TWFE Diagnostics   ###
### -------------------- ###

### Purpose: 
### We run two diagnostics: we (i) check for negative weights in treated units and 
### (ii) check for treatment effect homogeneity.
gtdgeo_events_diag <- gtdgeo_events_poswe %>% filter(!is.na(CON_suppressed) & !is.na(hostguest_id) & !is.na(IDMonthly))

trt_resid_primary <- lm(CON_suppressed ~ hostguest_id + IDMonthly, data = gtdgeo_events_diag)

fpe_primary_weights <- gtdgeo_events_diag %>%
  mutate(treatment_resid = residuals(trt_resid_primary)) %>%
  mutate(treatment_weight = treatment_resid / sum(treatment_resid^2))

fpe_primary_weights %>%
  dplyr::summarize(twfe_beta_primary = sum(mindist * treatment_weight))


## B.I: Graphical representation -----------------------------------------------
## Add residuals to data with weights:
out_resid_primary <- lm(CON_suppressed ~ hostguest_id, data = gtdgeo_events_poswe)
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
                                aes(x = treatment_resid, y = out_resid, color = factor(CON_suppressed))) +
    geom_smooth(aes(linetype = "Trend"), method = "lm", se = FALSE) +
    scale_color_viridis_d(option = "rocket", begin = 0.2, end = 0.8,
                          labels = c("Untreated", "Treated")) +
    scale_linetype_manual(values = c("Trend" = "solid"),
                          guide = guide_legend(override.aes = list(color = "grey30"))) +
    labs(x = "Residualised treatment", y = "Residualised outcome",
         color = NULL, linetype = NULL) +
    theme_minimal() + My_Theme + 
    theme(legend.position = "bottom"))

## § Print: Figure .............................................................
#plot_out_trt_primary
## .............................................................................
