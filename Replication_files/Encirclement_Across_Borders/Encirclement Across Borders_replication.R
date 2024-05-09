### ------------------------------- ###
###   Encirclement Across Borders   ###
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

# Load packages ----------------------------------------------------------------
library(patchwork)
library(cowplot)

### Upload data ----------------------------------------------------------------
## Read in file of 1989-2019 summary statistics for conflict termination (Oct23_DurationTrends):
trendframe_duration <- readRDS(
  "[Insert folder path here]/locs_spells_outcomes.Rdata"
)


## Melt
melted_trendframe_duration <- melt(trendframe_duration, id.vars = "Cat")

# Filter the melted dataframe to exclude "Transnational_pro" and "Domestic_pro"
melted_trendframe_duration <- melted_trendframe_duration %>%
  filter(variable != "Transnational_pro" & variable != "Domestic_pro") %>%
  mutate(value = as.numeric(value))

## Reorder
desired_order <- c("Rebel victory or \n Peace agreement", 
                   "Ceasefire", 
                   "Incumbent victory", 
                   "Average \n conflict \n duration", 
                   "Median \n conflict \n duration")


# Set the "Cat" column as a factor with the desired order
melted_trendframe_duration$Cat <- factor(melted_trendframe_duration$Cat, levels = desired_order)

# Reorder the dataframe based on the "Cat" column
melted_trendframe_duration <- melted_trendframe_duration[order(melted_trendframe_duration$Cat), ]

# If needed, reset row names
rownames(melted_trendframe_duration) <- NULL

melted_trendframe_duration$grid <- ifelse(melted_trendframe_duration$Cat == "Average \n conflict \n duration" | melted_trendframe_duration$Cat == "Median \n conflict \n duration",
                                          "Duration", "Outcome")

# Check
summary(trendframe_duration)


### Settings -------------------------------------------------------------------
windowsFonts(Palatino=windowsFont("Palatino Linotype"))
My_Theme = theme(
  title = element_text(size = 9, family="Palatino", face = "bold"),
  plot.title.position = "plot",
  plot.caption = element_text(size = 5, hjust = 0,family="Palatino", face = "italic"),
  plot.caption.position = "plot",
  axis.title.x = element_text(size = 9, family="Palatino", face = "italic"),
  axis.text.x = element_text(size = 9, family="Palatino"),
  axis.text.y = element_text(size = 9, family="Palatino"),
  axis.title.y = element_text(size = 9, family="Palatino"),
  legend.title = element_text(size = 9, family="Palatino", face = "italic"),
  legend.text = element_text(size = 8, family="Palatino"),
  strip.text.x = element_text(size = 8, family = "Palatino"))
# Reduce the opacity of the grid lines ("gray92"):
col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255)


# Now, you can use melted_trendframe_duration in your ggplot code
colors <- c("Domestic_share" = "slategrey","Transnational_share" = 'wheat3')


# Rearrange the "grid" variable so that "Outcome" comes first
melted_trendframe_duration$grid <- factor(melted_trendframe_duration$grid, levels = c("Outcome", "Duration"))

melted_trendframe_duration_1 <- subset(melted_trendframe_duration, grid == "Outcome")
melted_trendframe_duration_2 <- subset(melted_trendframe_duration, grid == "Duration")

melted_trendframe_duration_1$variable <- factor(melted_trendframe_duration_1$variable, levels=c('Transnational_share', 'Domestic_share'))
melted_trendframe_duration_2$variable <- factor(melted_trendframe_duration_2$variable, levels=c('Transnational_share', 'Domestic_share'))


### ---------------- ###
###   Single plot:   ###
###    Outcomes      ###
### ---------------- ###

# Create the ggplot object with legend:
ggsoil_duration_1_legend <- ggplot(melted_trendframe_duration_1, aes(x = Cat, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  theme_classic() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(0, 100)) +
  scale_fill_manual(values = colors,  name = "Rebel group type", labels = c("Purely domestic", "Transnational")) +  # Set fill colors
  guides(fill = guide_legend(override.aes = list(fill = colors))) +
  My_Theme + labs(y = "Percentage Share", color = "Type of rebel group") + # Remove the legend 
  #theme(legend.position = "none") +
  # Make x-axis longer
  coord_cartesian(clip = "off") +
  coord_flip() + # Flip
  labs(x = NULL)  # Remove the x-axis label


ggsoil_duration_1_legend

### ---------------- ###
###   Single plot:   ###
###    Duration      ###
### ---------------- ###

ggsoil_duration_2_legend <- ggplot(melted_trendframe_duration_2, aes(x = Cat, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.4) +
  theme_classic() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(0, 60)) +
  scale_fill_manual(values = colors,  name = "Rebel group type", labels = c("Purely domestic", "Transnational")) +  # Set fill colors
  guides(fill = guide_legend(override.aes = list(fill = colors))) +
  My_Theme + labs(y = "Number of months", color = "Type of rebel group") +
  # theme(legend.position = "none") +
  coord_cartesian(clip = "off") +
  coord_flip() + # Flip
  labs(x = NULL)  # Remove the x-axis label


ggsoil_duration_2_legend

### ---------------- ###
### Combined plots   ###
### ---------------- ###

# Create the ggplot objects as subparts of one whole:
ggsoil_duration_1 <- ggplot(melted_trendframe_duration_1, aes(x = Cat, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  theme_classic() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(0, 100)) +
  scale_fill_manual(values = colors) +  # Set fill colors
  guides(fill = guide_legend(override.aes = list(fill = colors))) +
  My_Theme + labs(y = "Percentage Share", color = "Type of rebel group") + # Remove the legend 
  theme(legend.position = "none") +
  # Make x-axis longer
  coord_cartesian(clip = "off") +
  labs(x = NULL)  # Remove the x-axis label

ggsoil_duration_2 <- ggplot(melted_trendframe_duration_2, aes(x = Cat, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.4) +
  theme_classic() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(0, 60)) +
  scale_fill_manual(values = colors,  name = "Rebel group type", labels = c("Purely domestic", "Transnational")) +  # Set fill colors
  guides(fill = guide_legend(override.aes = list(fill = colors))) +
  My_Theme + labs(y = "Number of months", color = "Type of rebel group") +
  # theme(legend.position = "none") +
  coord_cartesian(clip = "off") +
  labs(x = NULL)  # Remove the x-axis label

# Add a title to each individual plot (inside labs)
ggsoil_duration_1 <- ggsoil_duration_1 + labs(title = "Outcome") + theme(plot.title = element_text(hjust = 0.6)) + 
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Adjust angle and hjust as needed
  theme(plot.margin = margin(0, 0, 0, 0))  # Reduce margins to make the plot more compact

ggsoil_duration_2 <- ggsoil_duration_2 + labs(title = "Duration") + theme(plot.title = element_text(hjust = 0.3)) + 
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Adjust angle and hjust as needed
  theme(plot.margin = margin(0, 0, 0, 0))  # Reduce margins to make the plot more compact


# Combine the ggplots with the titles
combined_plot <- ggsoil_duration_1 + ggsoil_duration_2

# Display the combined plot
combined_plot 


### ------------------------------- ###
###         Figure 4                ###
### ------------------------------- ###

### Upload data ----------------------------------------------------------------
## Read in file of 1989-2019 summary statistics for conflict crackdowns (Oct23_DurationTrends):
cracklist <- readRDS(
  "[Insert folder path here]/locs_crackdowns_summary.Rdata"
)



# Make a dataframe with cumilative stats:
cracklist_frame <- data.frame("Cat" = c("n=159", "Affected Rebel Groups"), 
                              "Host_share" = c(cracklist$Hostled_pct[1]/100, cracklist$Hostled_pct[2]/100),
                              "Host_n" = c(cracklist$Hostled[1], cracklist$Hostled[2]),
                              "Target_share" = c(cracklist$Targetled_pct[1]/100, cracklist$Targetled_pct[2]/100),
                              "Target_n" = c(cracklist$Targetled[1], cracklist$Targetled[2]),
                              "HostplusTarget_share" = c((cracklist$Hostled_pct[1]+cracklist$Targetled_pct[1])/100,
                                                         (cracklist$Hostled_pct[2]+cracklist$Targetled_pct[2])/100),
                              "Joint_share" = c(cracklist$Joint_pct[1]/100, cracklist$Joint_pct[2]/100),
                              "Joint_n" = c(cracklist$Joint[1], cracklist$Joint[2]))

library(scales)
cracklist_frame <- cracklist_frame %>%
  mutate(Hundred = 1,
         Total_n = c(159, 55), 
         Cat = fct_relevel(Cat, "n=159", "Affected Rebel Groups")) # Reorder sequence

# Subset the campaigns (scrap the rebel rates)
cracklist_frame_campaigns <- cracklist_frame[1,]

## Rearrange cols (for right mapping):
#cat(colnames(cracklist_frame_campaigns), sep = ", ")

cracklist_frame_campaigns <- cracklist_frame_campaigns %>%
  select(Cat,Joint_share, Joint_n,Target_share, Target_n, HostplusTarget_share,Host_share, Host_n, everything())

#############
### GRAPH ###
#############

### Settings -------------------------------------------------------------------
windowsFonts(Palatino=windowsFont("Palatino Linotype"))
My_Theme = theme(
  title = element_text(size = 9, family="Palatino"),
  plot.title.position = "plot",
  plot.caption = element_text(size = 6, hjust = 0,family="Palatino", face = "italic"),
  plot.caption.position = "plot",
  axis.title.x = element_text(size = 10.5, family="Palatino"),
  axis.text.x = element_text(size = 9, family="Palatino",  face = "italic"),
  axis.text.y = element_text(size = 9, family="Palatino"),
  axis.title.y = element_text(size = 9, family="Palatino"),
  legend.title = element_text(size = 9, family="Palatino", face = "italic"),
  legend.text = element_text(size = 9, family="Palatino"),
  strip.text.x = element_text(size = 9, family = "Palatino"))
# Reduce the opacity of the grid lines ("gray92"):
col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255)


# Ensure the colors are defined in the order you want them to appear in the legend
colors <- c("Disjointed" = 'slategrey', "Intrusive" = 'rosybrown', "Joint" = 'wheat3')

ggcrack <- ggplot(cracklist_frame_campaigns, aes(x=Cat)) +   
  geom_bar(aes(y= Hundred, fill = "Joint"), stat = "identity") + 
  geom_bar(aes(y= HostplusTarget_share, fill = "Intrusive"), stat = "identity") + 
  geom_bar(aes(y= Host_share, fill = "Disjointed"), stat = "identity") + 
  geom_text(aes(y = Host_share, label =Host_n), vjust = 1.5, colour = "white", size = 2.5) + 
  geom_text(aes(y = HostplusTarget_share, label = Target_n), vjust = 1.5, colour = "white", size = 2.5) + 
  geom_text(aes(y =Hundred, label = Joint_n), vjust = 1.5, colour = "white", size = 2.5) + 
  theme_classic() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(0, 1)) +
  scale_fill_manual(values = colors, breaks = c("Joint", "Intrusive", "Disjointed")) + # Specify legend order here
  guides(fill = guide_legend(title = "Operational Setup")) +
  scale_x_discrete(position = "top")

ggcrack + My_Theme + labs(x = "Crackdown Campaigns",
                          y = "Share of Total")

### ------------------------------- ###
###           Table 2               ###
### ------------------------------- ###


## Load packages: --------------------------------------------------------------
library(coxme)
library(ehahelper)
library(broom)
library(dplyr)
library(ggplot2)
library(knitr)
library(ciTools)
library(here)

### Upload data ----------------------------------------------------------------

## Read in file of 1989-2019 complete panel (Mar23_Duration):
locsurv_2_cmptble <- readRDS(
  "[Insert folder path here]/locs_durationspells_termination_v1.Rdata"
)

## Definition: -----------------------------------------------------------------
## Frailty Cox Proportional Hazard Model.

# Set seed and disable scientific notation
set.seed(20180925)
options(scipen=999)

## Task 1: Run the three models separately -------------------------------------

### Panel 1 (Concerted)---------------------------------------------------------
H1conc_duration_coxFE <- coxme(Surv(ep_t0, ep_t1, as.numeric(sp2_ended)) ~ sanct_encircled_global + 
                                 target_milper + dom_sanct + 
                                 ss_troop_dum + ss_operational_dum + ss_material_dum
                               + (1|rebelid_iso3), # Two-way random effects 
                               locsurv_2_cmptble)


### Extract results:
H1conc_tidy_extract_coxme <- tidy(H1conc_duration_coxFE)
H1conc_tidy_extract_coxme$HR <- exp(H1conc_tidy_extract_coxme$estimate)
H1conc_tidy_extract_coxme <- H1conc_tidy_extract_coxme %>%
  rename(Variable=term, coef=estimate, p=p.value) %>%
  mutate(val = case_when(p<0.01 ~ "***",
                         p<0.05 ~ "**",
                         p<0.1 ~ "*", 
                         TRUE ~ "")) %>%
  select(Variable, HR, p, val)
print(H1conc_tidy_extract_coxme)

### Panel 2 (Separated)---------------------------------------------------------

### Try with each separately
H1sep_duration_coxFE <- coxme(Surv(ep_t0, ep_t1, as.numeric(sp2_ended)) ~ sanct_encircled_global_boots_sole +
                                sanct_encircled_global_joint_sole +
                                target_milper + dom_sanct + 
                                ss_troop_dum + ss_operational_dum + ss_material_dum
                              + (1|rebelid_iso3), # Two-way random effects 
                              locsurv_2_cmptble)

### Extract results:
H1sep_tidy_extract_coxme <- tidy(H1sep_duration_coxFE)
H1sep_tidy_extract_coxme$HR <- exp(H1sep_tidy_extract_coxme$estimate)
H1sep_tidy_extract_coxme <- H1sep_tidy_extract_coxme %>%
  rename(Variable=term, coef=estimate, p=p.value) %>%
  mutate(val = case_when(p<0.01 ~ "***",
                         p<0.05 ~ "**",
                         p<0.1 ~ "*", 
                         TRUE ~ "")) %>%
  select(Variable, HR, p, val)
print(H1sep_tidy_extract_coxme)


### Panel 3 (Host-led)----------------------------------------------------------
H1host_duration_coxFE <- coxme(Surv(ep_t0, ep_t1, as.numeric(sp2_ended)) ~ sanct_suppressed_unilathost + 
                                 target_milper + dom_sanct + 
                                 ss_troop_dum + ss_operational_dum + ss_material_dum
                               + (1|rebelid_iso3), # Two-way random effects 
                               locsurv_2_cmptble)



### Extract results:
H1host_tidy_extract_coxme <- tidy(H1host_duration_coxFE)
H1host_tidy_extract_coxme$HR <- exp(H1host_tidy_extract_coxme$estimate)
H1host_tidy_extract_coxme <- H1host_tidy_extract_coxme %>%
  rename(Variable=term, coef=estimate, p=p.value) %>%
  mutate(val = case_when(p<0.01 ~ "***",
                         p<0.05 ~ "**",
                         p<0.1 ~ "*", 
                         TRUE ~ "")) %>%
  select(Variable, HR, p, val)
print(H1host_tidy_extract_coxme)

## Merge the two
H1_master <- merge(x=H1conc_tidy_extract_coxme, y=H1sep_tidy_extract_coxme, by="Variable", all=T)
H1_master <- merge(x=H1_master, y=H1host_tidy_extract_coxme, by="Variable", all=T)

# Reorder
H1_master <- H1_master %>% 
  mutate( # Make an order var
    Rank = ifelse(Variable == "sanct_encircled_global", 1,  
                  ifelse(Variable == "sanct_encircled_global_boots_sole", 2,  
                         ifelse(Variable == "sanct_encircled_global_joint_sole", 3,  
                                ifelse(Variable == "sanct_suppressed_unilathost", 4, 
                                       ifelse(Variable == "target_milper", 5, 
                                              ifelse(Variable == "dom_sanct", 6,
                                                     ifelse(Variable == "ss_troop_dum", 7,
                                                            ifelse(Variable == "ss_operational_dum", 8,
                                                                   ifelse(Variable == "ss_material_dum", 9, NA
                                                                   )))))))))) %>%
  arrange(Rank) %>% 
  select(-c(Rank)) %>%
  mutate(Variable = case_when(Variable == "sanct_encircled_global" ~ "Encircled Crackdown", 
                              Variable == "sanct_encircled_global_boots_sole" ~ "Intrusive Crackdown", 
                              Variable == "sanct_encircled_global_joint_sole" ~ "Joint Crackdown", 
                              Variable == "sanct_suppressed_unilathost" ~ "Disjointed Crackdown", 
                              Variable == "target_milper" ~ "Target-state soldiers", 
                              Variable == "dom_sanct" ~ "Domestic control", 
                              Variable == "ss_troop_dum" ~ "Troop support", 
                              Variable == "ss_operational_dum" ~ "Operational support", 
                              Variable == "ss_material_dum" ~ "Material support", 
                              TRUE ~ Variable))

# Round to 5 decimals
H1_master <- H1_master %>% mutate(across(where(is.numeric), ~ round(., 4)))

# Remove xy tags
colnames(H1_master) = gsub("\\.x\\b", "", colnames(H1_master))# Remove .x 
colnames(H1_master) = gsub("\\.y\\b", "", colnames(H1_master)) # Remove .y

### Print LATEX
options(knitr.kable.NA = '', knitr.table.format = "latex") # Remove ugly NAs
H1_master_kable <- kable(H1_master, format = 'latex', 
                         booktabs = T,
                         bottomrule = '',
                         toprule = '',
                         midrule = '') %>%
  row_spec(0, hline_after = T) %>% # Make line under first row
  row_spec(9, hline_after = T) %>% # Make line under last row
  row_spec(row = 0, # Make first row bold
           bold = TRUE) %>%
  column_spec(column = 1, # Add border
              border_left = TRUE) %>%
  column_spec(column = (H1_master %>% nrow()-1),
              border_right = TRUE) %>%
  collapse_rows(columns = 1,
                valign = "middle") %>%
  add_header_above(header = c(" " = 1, "Model 1" = 3, "Model 2" = 3, "Model 3" = 3),
                   bold = TRUE,
                   border_left = TRUE,
                   border_right = TRUE) %>%
  kable_styling(font_size = 9) %>% footnote(alphabet = c("\\\\scriptsize{$n$=1,101. Model: Cox PH. Test of violated PH assumption rejected ($a$=.05).}",
                                                         "\\\\scriptsize{***=p$<$0.01, **=p$<$0.05, *=p$<$0.1.}"), 
                                            escape = F)


## Task completed. -------------------------------------------------------------

### Task 2: Check proportionality assumption: ----------------------------------

### Load font:
windowsFonts(Palatino=windowsFont("Palatino Linotype"))

### A: Test model 1: -----------------------------------------------------------
library(survminer)
coxass_h1conc <- ggcoxzph(cox.zph(H1conc_duration_coxFE), point.col = "slateblue") # Main IV

coxass_h1conc$`1`$labels$y <- "Encircled Crackdown"
coxass_h1conc$`2`$labels$y <- "Target-state soldiers"
coxass_h1conc$`3`$labels$y <- "Domestic control"
coxass_h1conc$`4`$labels$y <- "Troop support"
coxass_h1conc$`5`$labels$y <- "Operational support"
coxass_h1conc$`6`$labels$y <- "Material support"

# § Print: .....................................................................
ggpar(coxass_h1conc, 
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


### B: Test model 2: -----------------------------------------------------------
coxass_h1sep <- ggcoxzph(cox.zph(H1sep_duration_coxFE), point.col = "slateblue") # Main IV

coxass_h1sep$`1`$labels$y <- "Intrusive Crackdown"
coxass_h1sep$`2`$labels$y <- "Joint Crackdown"
coxass_h1sep$`3`$labels$y <- "Target-state soldiers"
coxass_h1sep$`4`$labels$y <- "Domestic control"
coxass_h1sep$`5`$labels$y <- "Troop support"
coxass_h1sep$`6`$labels$y <- "Operational support"
coxass_h1sep$`7`$labels$y <- "Material support"

# § Print: .....................................................................
ggpar(coxass_h1sep, 
      font.family = "Palatino",
      font.x = c(7, "plain", "black"),
      font.y = c(7, "italic", "black"),
      font.main = c(8, "plain", "black"),
      font.title = c(8, "plain", "black"),
      font.submain = c(7, "plain", "black"),
      font.subtitle = c(7, "plain", "black"),
      font.tickslab = c(7, "plain", "black"),
      x.text.angle = c(90),
      rotate = F
)

### C: Test model 3: -----------------------------------------------------------
coxass_h1host <- ggcoxzph(cox.zph(H1host_duration_coxFE), point.col = "slateblue") # Main IV

coxass_h1host$`1`$labels$y <- "Disjointed Crackdown"
coxass_h1host$`2`$labels$y <- "Target-state soldiers"
coxass_h1host$`3`$labels$y <- "Domestic control"
coxass_h1host$`4`$labels$y <- "Troop support"
coxass_h1host$`5`$labels$y <- "Operational support"
coxass_h1host$`6`$labels$y <- "Material support"

# § Print: .....................................................................
ggpar(coxass_h1host, 
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

### Task complete --------------------------------------------------------------

### ------------------------------- ###
###           Figure 5              ###
### ------------------------------- ###

## Define model ----------------------------------------------------------------
H1PL_survcurve_mod <- coxph(Surv(ep_t0, ep_t1,  as.numeric(sp2_ended)) ~ sanct_encircled_global  
                            + target_milper + dom_sanct + 
                              ss_troop_dum + ss_operational_dum + ss_material_dum
                            + cluster(rebelid_iso3),
                            data=locsurv_2_cmptble)



### Prediction frame -----------------------------------------------------------
ossi.fin_curve <- with(locsurv_2_cmptble, 
                       data.frame(sanct_encircled_global=c(0, 1),
                                  dom_sanct=rep(mean(dom_sanct, na.rm=T), 2),
                                  ss_troop_dum=rep(mean(ss_troop_dum, na.rm=T), 2),
                                  ss_operational_dum=rep(mean(ss_operational_dum, na.rm=T), 2),
                                  ss_material_dum=rep(mean(ss_material_dum, na.rm=T), 2),
                                  target_milper =rep(mean(target_milper, na.rm=T), 2)
                       ))

## Define colours --------------------------------------------------------------
col_0 <- "dodgerblue4"
col_1 <- "red3"

col_0.5 <- "dodgerblue3"
col_1.5 <- "brown3"

###-----------------------------------------------------------------------------
## Load font
windowsFonts(Palatino=windowsFont("Palatino Linotype"))
#png(file="C:/Users/jakob/Downloads/R~homeland/DPhil/a_ops/Results/Pledges/coxph_misbehave_weakness_USE.png", ## Saves file as png
#    width=600, height=350) ## Set res
par( ### Set standards
  mgp = c(2, 0.4, 0), # size of space between lables (axis title, axis labels, axis line)
  tcl = -0.3, ## Reduce the size of the tick marks.
  cex.lab = 0.8,  # Adjust the size of the axis titles
  family="Palatino", font=1) ## Set font
plot(survfit(H1PL_survcurve_mod, ## Model
             newdata=ossi.fin_curve), ## pdataframe
     conf.int=.95,
     conf.offset = c(1.8),
     lty=c(5, 1), ## type line 1, line 2
     lwd=c(0.5,0.5), ## size line 1, line 2
     ylim=c(0, 1), ## Y lim
     xlim=c(0,120), ## X lim
     xlab="Months Since Conflict Erupted",
     col=c(col_0.5,col_1.5),
     ylab="Proportion Not Terminating", 
     bty = "n",  ## Remove the box around the plot.
     xaxt = "n", yaxt = "n")  ## Remove default x and y axis.
#grid(nx = 12, # X division
#     ny = 10, # Y division
#     col = "gray88", # Grid
#     lty = "dotted",
#     lwd = .1)
title(sub= expression(paste(italic("Note: \n(a) Thick lines: Hazard rates.\n(b) Thin lines: 95 pct. CIs.\n (c) Model: Cox PH clustered on rebel dyad with covariates at mean."))), cex.sub = 0.65, adj=0)
box("plot", ## Add 'box' lines to the bottom and left of the plot.
    bty = "l",  
    lwd = 2) ## Increase width of box lines.
axis(side = 1, lwd = 0, lwd.ticks = 2, ## X Axis
     mgp = c(2.5, 0.2, 0)) ## 2nd element = tick labels closer to the axis line.
axis(side = 2, lwd = 0, lwd.ticks = 2, ## Y axis
     las = 2) ## Rotate tick labels prependicular to the axis.
legend("topright", 
       legend=c("No/Disjointed Crackdowns","Intrusive/Joint Crackdowns"), 
       col=c(col_0,col_1),
       lty=c(5 ,1), # line 1, line 2
       lwd=c(1.5, 1.5), ## size line 1, line 2
       cex = 0.8,  # Adjust the size of the legend text
       inset=0.02,
       x.intersp = 1.1, y.intersp = 1.1) ## Increase the spacing in the x and y directions.
minor.tick(nx=5, # n of small ticks between major ticks (X)
           ny=2, # n of small ticks between major ticks (Y)
           tick.ratio=0.75) # ratio of length of ticks to major
par(new=TRUE)
plot(survfit(H1PL_survcurve_mod, ## Model replication without conf bands
             newdata=ossi.fin_curve), ## pdataframe
     conf.int=FALSE,
     conf.offset = c(1.8),
     lty=c(5, 1), ## type line 1, line 2
     lwd=c(2,2), ## size line 1, line 2
     ylim=c(0, 1), ## Y lim
     xlim=c(0,120), ## X lim
     col=c(col_0,col_1),
     bty = "n",  ## Remove the box around the plot.
     xaxt = "n", yaxt = "n")  ## Remove default x and y axis.
#mtext(note_fig_H2_cox_weaknes, side = 1, line = 6, cex = 0.8, adj = 0) 


### ----------------------------###
###      Robustness 1           ###
###-----------------------------###

### One-year rule --------------------------------------------------------------

## Load packages: --------------------------------------------------------------
library(coxme)
library(ehahelper)
library(broom)
library(dplyr)
library(ggplot2)
library(knitr)
library(ciTools)
library(here)

### Upload data ----------------------------------------------------------------

## Read in file of 1989-2019 complete panel (Feb23_Bombing):
locsurv_1_cmptble <- readRDS(
  "[Insert folder path here]/locs_durationspells_termination_1yr.Rdata"
)

## Definition: -----------------------------------------------------------------
## Frailty Cox Proportional Hazard Model.

# Set seed and disable scientific notation
set.seed(20180925)
options(scipen=999)

## Task 1: Run the three models separately -------------------------------------

### Panel 1 (Concerted)---------------------------------------------------------
H1conc_duration_coxFE <- coxme(Surv(ep_t0, ep_t1, as.numeric(sp1_ended)) ~ sanct_encircled_global + 
                                 target_milper + dom_sanct + 
                                 ss_troop_dum + ss_operational_dum + ss_material_dum
                               + (1|rebelid_iso3), # Two-way random effects 
                               locsurv_1_cmptble)


### Extract results:
H1conc_tidy_extract_coxme <- tidy(H1conc_duration_coxFE)
H1conc_tidy_extract_coxme$HR <- exp(H1conc_tidy_extract_coxme$estimate)
H1conc_tidy_extract_coxme <- H1conc_tidy_extract_coxme %>%
  rename(Variable=term, coef=estimate, p=p.value) %>%
  mutate(val = case_when(p<0.01 ~ "***",
                         p<0.05 ~ "**",
                         p<0.1 ~ "*", 
                         TRUE ~ "")) %>%
  select(Variable, HR, p, val)
print(H1conc_tidy_extract_coxme)

### Panel 2 (Separated)---------------------------------------------------------

### Try with each separately
H1sep_duration_coxFE <- coxme(Surv(ep_t0, ep_t1, as.numeric(sp1_ended)) ~ sanct_encircled_global_boots_sole +
                                sanct_encircled_global_joint_sole +
                                target_milper + dom_sanct + 
                                ss_troop_dum + ss_operational_dum + ss_material_dum
                              + (1|rebelid_iso3), # Two-way random effects 
                              locsurv_1_cmptble)

### Extract results:
H1sep_tidy_extract_coxme <- tidy(H1sep_duration_coxFE)
H1sep_tidy_extract_coxme$HR <- exp(H1sep_tidy_extract_coxme$estimate)
H1sep_tidy_extract_coxme <- H1sep_tidy_extract_coxme %>%
  rename(Variable=term, coef=estimate, p=p.value) %>%
  mutate(val = case_when(p<0.01 ~ "***",
                         p<0.05 ~ "**",
                         p<0.1 ~ "*", 
                         TRUE ~ "")) %>%
  select(Variable, HR, p, val)
print(H1sep_tidy_extract_coxme)


### Panel 3 (Host-led)----------------------------------------------------------
H1host_duration_coxFE <- coxme(Surv(ep_t0, ep_t1, as.numeric(sp1_ended)) ~ sanct_suppressed_unilathost + 
                                 target_milper + dom_sanct + 
                                 ss_troop_dum + ss_operational_dum + ss_material_dum
                               + (1|rebelid_iso3), # Two-way random effects 
                               locsurv_1_cmptble)



### Extract results:
H1host_tidy_extract_coxme <- tidy(H1host_duration_coxFE)
H1host_tidy_extract_coxme$HR <- exp(H1host_tidy_extract_coxme$estimate)
H1host_tidy_extract_coxme <- H1host_tidy_extract_coxme %>%
  rename(Variable=term, coef=estimate, p=p.value) %>%
  mutate(val = case_when(p<0.01 ~ "***",
                         p<0.05 ~ "**",
                         p<0.1 ~ "*", 
                         TRUE ~ "")) %>%
  select(Variable, HR, p, val)
print(H1host_tidy_extract_coxme)

## Merge the two
H1_master <- merge(x=H1conc_tidy_extract_coxme, y=H1sep_tidy_extract_coxme, by="Variable", all=T)
H1_master <- merge(x=H1_master, y=H1host_tidy_extract_coxme, by="Variable", all=T)

# Reorder
H1_master <- H1_master %>% 
  mutate( # Make an order var
    Rank = ifelse(Variable == "sanct_encircled_global", 1,  
                  ifelse(Variable == "sanct_encircled_global_boots_sole", 2,  
                         ifelse(Variable == "sanct_encircled_global_joint_sole", 3,  
                                ifelse(Variable == "sanct_suppressed_unilathost", 4, 
                                       ifelse(Variable == "target_milper", 5, 
                                              ifelse(Variable == "dom_sanct", 6,
                                                     ifelse(Variable == "ss_troop_dum", 7,
                                                            ifelse(Variable == "ss_operational_dum", 8,
                                                                   ifelse(Variable == "ss_material_dum", 9, NA
                                                                   )))))))))) %>%
  arrange(Rank) %>% 
  select(-c(Rank)) %>%
  mutate(Variable = case_when(Variable == "sanct_encircled_global" ~ "Encircled Crackdown", 
                              Variable == "sanct_encircled_global_boots_sole" ~ "Intrusive Crackdown", 
                              Variable == "sanct_encircled_global_joint_sole" ~ "Joint Crackdown", 
                              Variable == "sanct_suppressed_unilathost" ~ "Disjointed Crackdown", 
                              Variable == "target_milper" ~ "Target-state soldiers", 
                              Variable == "dom_sanct" ~ "Domestic control", 
                              Variable == "ss_troop_dum" ~ "Troop support", 
                              Variable == "ss_operational_dum" ~ "Operational support", 
                              Variable == "ss_material_dum" ~ "Material support", 
                              TRUE ~ Variable))

# Round to 5 decimals
H1_master <- H1_master %>% mutate(across(where(is.numeric), ~ round(., 4)))

# Remove xy tags
colnames(H1_master) = gsub("\\.x\\b", "", colnames(H1_master))# Remove .x 
colnames(H1_master) = gsub("\\.y\\b", "", colnames(H1_master)) # Remove .y

### Print LATEX
options(knitr.kable.NA = '', knitr.table.format = "latex") # Remove ugly NAs
H1_master_kable <- kable(H1_master, format = 'latex', 
                         booktabs = T,
                         bottomrule = '',
                         toprule = '',
                         midrule = '') %>%
  row_spec(0, hline_after = T) %>% # Make line under first row
  row_spec(9, hline_after = T) %>% # Make line under last row
  row_spec(row = 0, # Make first row bold
           bold = TRUE) %>%
  column_spec(column = 1, # Add border
              border_left = TRUE) %>%
  column_spec(column = (H1_master %>% nrow()-1),
              border_right = TRUE) %>%
  collapse_rows(columns = 1,
                valign = "middle") %>%
  add_header_above(header = c(" " = 1, "Model 1" = 3, "Model 2" = 3, "Model 3" = 3),
                   bold = TRUE,
                   border_left = TRUE,
                   border_right = TRUE) %>%
  kable_styling(font_size = 9) %>% footnote(alphabet = c("\\\\scriptsize{$n$=1,101. Model: Cox PH. Test of violated PH assumption rejected ($a$=.05).}",
                                                         "\\\\scriptsize{***=p$<$0.01, **=p$<$0.05, *=p$<$0.1.}"), 
                                            escape = F)

## Task completed. -------------------------------------------------------------

### ----------------------------###
###      Robustness 2           ###
###-----------------------------###

### Three-year rule --------------------------------------------------------------

## Load packages: --------------------------------------------------------------
library(coxme)
library(ehahelper)
library(broom)
library(dplyr)
library(ggplot2)
library(knitr)
library(ciTools)
library(here)

### Upload data ----------------------------------------------------------------

## Read in file of 1989-2019 complete panel (Feb23_Bombing):
locsurv_3_cmptble <- readRDS(
  "[Insert folder path here]/locs_durationspells_termination_3yr.Rdata"
)

## Definition: -----------------------------------------------------------------
## Frailty Cox Proportional Hazard Model.

# Set seed and disable scientific notation
set.seed(20180925)
options(scipen=999)

## Task 1: Run the three models separately -------------------------------------

### Panel 1 (Concerted)---------------------------------------------------------
H1conc_duration_coxFE <- coxme(Surv(ep_t0, ep_t1, as.numeric(sp3_ended)) ~ sanct_encircled_global + 
                                 target_milper + dom_sanct + 
                                 ss_troop_dum + ss_operational_dum + ss_material_dum
                               + (1|rebelid_iso3), # Two-way random effects 
                               locsurv_3_cmptble)


### Extract results:
H1conc_tidy_extract_coxme <- tidy(H1conc_duration_coxFE)
H1conc_tidy_extract_coxme$HR <- exp(H1conc_tidy_extract_coxme$estimate)
H1conc_tidy_extract_coxme <- H1conc_tidy_extract_coxme %>%
  rename(Variable=term, coef=estimate, p=p.value) %>%
  mutate(val = case_when(p<0.01 ~ "***",
                         p<0.05 ~ "**",
                         p<0.1 ~ "*", 
                         TRUE ~ "")) %>%
  select(Variable, HR, p, val)
print(H1conc_tidy_extract_coxme)

### Panel 2 (Separated)---------------------------------------------------------

### Try with each separately
H1sep_duration_coxFE <- coxme(Surv(ep_t0, ep_t1, as.numeric(sp3_ended)) ~ sanct_encircled_global_boots_sole +
                                sanct_encircled_global_joint_sole +
                                target_milper + dom_sanct + 
                                ss_troop_dum + ss_operational_dum + ss_material_dum
                              + (1|rebelid_iso3), # Two-way random effects 
                              locsurv_3_cmptble)

### Extract results:
H1sep_tidy_extract_coxme <- tidy(H1sep_duration_coxFE)
H1sep_tidy_extract_coxme$HR <- exp(H1sep_tidy_extract_coxme$estimate)
H1sep_tidy_extract_coxme <- H1sep_tidy_extract_coxme %>%
  rename(Variable=term, coef=estimate, p=p.value) %>%
  mutate(val = case_when(p<0.01 ~ "***",
                         p<0.05 ~ "**",
                         p<0.1 ~ "*", 
                         TRUE ~ "")) %>%
  select(Variable, HR, p, val)
print(H1sep_tidy_extract_coxme)


### Panel 3 (Host-led)----------------------------------------------------------
H1host_duration_coxFE <- coxme(Surv(ep_t0, ep_t1, as.numeric(sp3_ended)) ~ sanct_suppressed_unilathost + 
                                 target_milper + dom_sanct + 
                                 ss_troop_dum + ss_operational_dum + ss_material_dum
                               + (1|rebelid_iso3), # Two-way random effects 
                               locsurv_3_cmptble)



### Extract results:
H1host_tidy_extract_coxme <- tidy(H1host_duration_coxFE)
H1host_tidy_extract_coxme$HR <- exp(H1host_tidy_extract_coxme$estimate)
H1host_tidy_extract_coxme <- H1host_tidy_extract_coxme %>%
  rename(Variable=term, coef=estimate, p=p.value) %>%
  mutate(val = case_when(p<0.01 ~ "***",
                         p<0.05 ~ "**",
                         p<0.1 ~ "*", 
                         TRUE ~ "")) %>%
  select(Variable, HR, p, val)
print(H1host_tidy_extract_coxme)

## Merge the two
H1_master <- merge(x=H1conc_tidy_extract_coxme, y=H1sep_tidy_extract_coxme, by="Variable", all=T)
H1_master <- merge(x=H1_master, y=H1host_tidy_extract_coxme, by="Variable", all=T)

# Reorder
H1_master <- H1_master %>% 
  mutate( # Make an order var
    Rank = ifelse(Variable == "sanct_encircled_global", 1,  
                  ifelse(Variable == "sanct_encircled_global_boots_sole", 2,  
                         ifelse(Variable == "sanct_encircled_global_joint_sole", 3,  
                                ifelse(Variable == "sanct_suppressed_unilathost", 4, 
                                       ifelse(Variable == "target_milper", 5, 
                                              ifelse(Variable == "dom_sanct", 6,
                                                     ifelse(Variable == "ss_troop_dum", 7,
                                                            ifelse(Variable == "ss_operational_dum", 8,
                                                                   ifelse(Variable == "ss_material_dum", 9, NA
                                                                   )))))))))) %>%
  arrange(Rank) %>% 
  select(-c(Rank)) %>%
  mutate(Variable = case_when(Variable == "sanct_encircled_global" ~ "Encircled Crackdown", 
                              Variable == "sanct_encircled_global_boots_sole" ~ "Intrusive Crackdown", 
                              Variable == "sanct_encircled_global_joint_sole" ~ "Joint Crackdown", 
                              Variable == "sanct_suppressed_unilathost" ~ "Disjointed Crackdown", 
                              Variable == "target_milper" ~ "Target-state soldiers", 
                              Variable == "dom_sanct" ~ "Domestic control", 
                              Variable == "ss_troop_dum" ~ "Troop support", 
                              Variable == "ss_operational_dum" ~ "Operational support", 
                              Variable == "ss_material_dum" ~ "Material support", 
                              TRUE ~ Variable))

# Round to 5 decimals
H1_master <- H1_master %>% mutate(across(where(is.numeric), ~ round(., 4)))

# Remove xy tags
colnames(H1_master) = gsub("\\.x\\b", "", colnames(H1_master))# Remove .x 
colnames(H1_master) = gsub("\\.y\\b", "", colnames(H1_master)) # Remove .y

### Print LATEX
options(knitr.kable.NA = '', knitr.table.format = "latex") # Remove ugly NAs
H1_master_kable <- kable(H1_master, format = 'latex', 
                         booktabs = T,
                         bottomrule = '',
                         toprule = '',
                         midrule = '') %>%
  row_spec(0, hline_after = T) %>% # Make line under first row
  row_spec(9, hline_after = T) %>% # Make line under last row
  row_spec(row = 0, # Make first row bold
           bold = TRUE) %>%
  column_spec(column = 1, # Add border
              border_left = TRUE) %>%
  column_spec(column = (H1_master %>% nrow()-1),
              border_right = TRUE) %>%
  collapse_rows(columns = 1,
                valign = "middle") %>%
  add_header_above(header = c(" " = 1, "Model 1" = 3, "Model 2" = 3, "Model 3" = 3),
                   bold = TRUE,
                   border_left = TRUE,
                   border_right = TRUE) %>%
  kable_styling(font_size = 9) %>% footnote(alphabet = c("\\\\scriptsize{$n$=1,101. Model: Cox PH. Test of violated PH assumption rejected ($a$=.05).}",
                                                         "\\\\scriptsize{***=p$<$0.01, **=p$<$0.05, *=p$<$0.1.}"), 
                                            escape = F) 

## Task completed. -------------------------------------------------------------

### ----------------------------###
###      Robustness 3           ###
###-----------------------------###

### Define:
### Cox PH with cluster method

### ----------------###
###   Model 1       ###
###-----------------###
H1_coxclust <- coxph(Surv(ep_t0, ep_t1, as.numeric(sp2_ended)) ~ sanct_encircled_global + 
                       target_milper + dom_sanct + 
                       ss_troop_dum + ss_operational_dum + ss_material_dum
                     + cluster(rebelid_iso3),
                     data=locsurv_2_cmptble)


H1_coxclust_df <- cox_as_data_frame( # Make data.frame
  H1_coxclust,
  unmangle_dict = NULL,
  factor_id_sep = ":",
  sort_by = NULL
)

### Edit summary
H1_coxclust_df <- H1_coxclust_df %>%
  select(-c(factor.id, factor.value, Lower_CI, Upper_CI, Inv_Lower_CI, Inv_Upper_CI, Inv_HR)) %>%
  rename(Variable = factor.name, 
         "exp(Coef)" = HR) %>%
  mutate(val = case_when(p<0.01 ~ "***",
                         p<0.05 ~ "**",
                         p<0.1 ~ "*", 
                         TRUE ~ ""))

### ----------------###
###   Model 2       ###
###-----------------###
H1_coxclustiso <- coxph(Surv(ep_t0, ep_t1, as.numeric(sp2_ended)) ~ sanct_encircled_global_boots_sole +
                          sanct_encircled_global_joint_sole +
                          target_milper + dom_sanct + 
                          ss_troop_dum + ss_operational_dum + ss_material_dum
                        + cluster(rebelid_iso3),
                        data=locsurv_2_cmptble)


H1_coxclustiso_df <- cox_as_data_frame( # Make data.frame
  H1_coxclustiso,
  unmangle_dict = NULL,
  factor_id_sep = ":",
  sort_by = NULL
)

### Edit summary
H1_coxclustiso_df <- H1_coxclustiso_df %>%
  select(-c(factor.id, factor.value, Lower_CI, Upper_CI, Inv_Lower_CI, Inv_Upper_CI, Inv_HR)) %>%
  rename(Variable = factor.name, 
         "exp(Coef)" = HR) %>%
  mutate(val = case_when(p<0.01 ~ "***",
                         p<0.05 ~ "**",
                         p<0.1 ~ "*", 
                         TRUE ~ ""))

### ----------------###
###   Model 3       ###
###-----------------###
H1_coxclusthost <- coxph(Surv(ep_t0, ep_t1, as.numeric(sp2_ended)) ~ sanct_suppressed_unilathost +
                           sanct_encircled_global_joint_sole +
                           target_milper + dom_sanct + 
                           ss_troop_dum + ss_operational_dum + ss_material_dum
                         + cluster(rebelid_iso3),
                         data=locsurv_2_cmptble)


H1_coxclusthost_df <- cox_as_data_frame( # Make data.frame
  H1_coxclusthost,
  unmangle_dict = NULL,
  factor_id_sep = ":",
  sort_by = NULL
)

### Edit summary
H1_coxclusthost_df <- H1_coxclusthost_df %>%
  select(-c(factor.id, factor.value, Lower_CI, Upper_CI, Inv_Lower_CI, Inv_Upper_CI, Inv_HR)) %>%
  rename(Variable = factor.name, 
         "exp(Coef)" = HR) %>%
  mutate(val = case_when(p<0.01 ~ "***",
                         p<0.05 ~ "**",
                         p<0.1 ~ "*", 
                         TRUE ~ ""))


### ----------------###
###      Bind       ###
###-----------------###

## Merge:
H1_coxclust_master <- merge(x=H1_coxclust_df, y = H1_coxclustiso_df, by="Variable", all = T)
H1_coxclust_master <- merge(x=H1_coxclust_master, y = H1_coxclusthost_df, by="Variable", all = T)

## Clean up:
H1_coxclust_master <- H1_coxclust_master %>% 
  mutate( # Make an order var
    Rank = ifelse(Variable == "sanct_encircled_global", 1,  
                  ifelse(Variable == "sanct_encircled_global_boots_sole", 2,  
                         ifelse(Variable == "sanct_encircled_global_joint_sole", 3,  
                                ifelse(Variable == "sanct_suppressed_unilathost", 4, 
                                       ifelse(Variable == "target_milper", 5, 
                                              ifelse(Variable == "dom_sanct", 6,
                                                     ifelse(Variable == "ss_troop_dum", 7,
                                                            ifelse(Variable == "ss_operational_dum", 8,
                                                                   ifelse(Variable == "ss_material_dum", 9, NA
                                                                   )))))))))) %>%
  arrange(Rank) %>% 
  select(-c(Rank)) %>%
  mutate(Variable = case_when(Variable == "sanct_encircled_global" ~ "Encircled Crackdown", 
                              Variable == "sanct_encircled_global_boots_sole" ~ "Intrusive Crackdown", 
                              Variable == "sanct_encircled_global_joint_sole" ~ "Joint Crackdown", 
                              Variable == "sanct_suppressed_unilathost" ~ "Disjointed Crackdown", 
                              Variable == "target_milper" ~ "Target-state soldiers", 
                              Variable == "dom_sanct" ~ "Domestic control", 
                              Variable == "ss_troop_dum" ~ "Troop support", 
                              Variable == "ss_operational_dum" ~ "Operational support", 
                              Variable == "ss_material_dum" ~ "Material support", 
                              TRUE ~ Variable))

# Round to 5 decimals
H1_coxclust_master <- H1_coxclust_master %>% mutate(across(where(is.numeric), ~ round(., 4)))

# Remove xy tags
colnames(H1_coxclust_master) = gsub("\\.x\\b", "", colnames(H1_master))# Remove .x 
colnames(H1_coxclust_master) = gsub("\\.y\\b", "", colnames(H1_master)) # Remove .y

### Print LATEX
options(knitr.kable.NA = '', knitr.table.format = "latex") # Remove ugly NAs
H1_master_kable <- kable(H1_coxclust_master, format = 'latex', 
                         booktabs = T,
                         bottomrule = '',
                         toprule = '',
                         midrule = '') %>%
  row_spec(0, hline_after = T) %>% # Make line under first row
  row_spec(9, hline_after = T) %>% # Make line under last row
  row_spec(row = 0, # Make first row bold
           bold = TRUE) %>%
  column_spec(column = 1, # Add border
              border_left = TRUE) %>%
  column_spec(column = (H1_master %>% nrow()-1),
              border_right = TRUE) %>%
  collapse_rows(columns = 1,
                valign = "middle") %>%
  add_header_above(header = c(" " = 1, "Model 1" = 3, "Model 2" = 3, "Model 3" = 3),
                   bold = TRUE,
                   border_left = TRUE,
                   border_right = TRUE) %>%
  kable_styling(font_size = 9) %>% footnote(alphabet = c("\\\\scriptsize{$n$=1,101. Model: Cox PH. Test of violated PH assumption rejected ($a$=.05).}",
                                                         "\\\\scriptsize{***=p$<$0.01, **=p$<$0.05, *=p$<$0.1.}"), 
                                            escape = F) 


###-----------------------------###
###      Sensitivity 1          ###
###-----------------------------###

### Define:
### Remove strong host states

## Load packages: --------------------------------------------------------------
library(coxme)
library(ehahelper)
library(broom)
library(dplyr)
library(ggplot2)
library(knitr)
library(ciTools)
library(here)

### Upload data ----------------------------------------------------------------

## Read in file of 1989-2019 complete panel (Feb23_Bombing):
locsurv_2_cmptble <- readRDS(
  "[Insert folder path here]/locs_durationspells_termination_v1.Rdata"
)

## Read in file of 1990-2018 RPE data (Jan23_Pledges):
rpe <- readRDS(
  "[Insert folder path here]/rpr_gdp.Rdata"
)

## Pair the two (Old n=8,514, New n=5,805):
locsurv_2_cmptble_rpe <- merge(x=locsurv_2_cmptble, y=rpe, by=c("IDMonthly", "rebelid_iso3"), all.x = F, all.y = F)

### Make sample cut ------------------------------------------------------------

## Make quantiles for Extraction:
statecap_qtls <- quantile(locsurv_2_cmptble_rpe$hosts_rpr_gdp_av, na.rm = T)
statecap_75th <- statecap_qtls[[4:4]]
locsurv_2_cmptble_weak_gdp <- subset(locsurv_2_cmptble_rpe, hosts_rpr_gdp_av < statecap_75th) # New n=4,354


# Disable scientific notation
options(scipen=999)

## Task 1: Run the three models separately -------------------------------------

### Panel 1 (Concerted)---------------------------------------------------------
H1conc_duration_coxFE <- coxme(Surv(ep_t0, ep_t1, as.numeric(sp2_ended)) ~ sanct_encircled_global + 
                                 target_milper + dom_sanct + 
                                 ss_troop_dum + ss_operational_dum + ss_material_dum
                               + (1|rebelid_iso3), # Two-way random effects 
                               locsurv_2_cmptble_weak_gdp)


### Extract results:
H1conc_tidy_extract_coxme <- tidy(H1conc_duration_coxFE)
H1conc_tidy_extract_coxme$HR <- exp(H1conc_tidy_extract_coxme$estimate)
H1conc_tidy_extract_coxme <- H1conc_tidy_extract_coxme %>%
  rename(Variable=term, coef=estimate, p=p.value) %>%
  mutate(val = case_when(p<0.01 ~ "***",
                         p<0.05 ~ "**",
                         p<0.1 ~ "*", 
                         TRUE ~ "")) %>%
  select(Variable, HR, p, val)
print(H1conc_tidy_extract_coxme)

### Panel 2 (Separated)---------------------------------------------------------

### Try with each separately
H1sep_duration_coxFE <- coxme(Surv(ep_t0, ep_t1, as.numeric(sp2_ended)) ~ sanct_encircled_global_boots_sole +
                                sanct_encircled_global_joint_sole +
                                target_milper + dom_sanct + 
                                ss_troop_dum + ss_operational_dum + ss_material_dum
                              + (1|rebelid_iso3), # Two-way random effects 
                              locsurv_2_cmptble_weak_gdp)

### Extract results:
H1sep_tidy_extract_coxme <- tidy(H1sep_duration_coxFE)
H1sep_tidy_extract_coxme$HR <- exp(H1sep_tidy_extract_coxme$estimate)
H1sep_tidy_extract_coxme <- H1sep_tidy_extract_coxme %>%
  rename(Variable=term, coef=estimate, p=p.value) %>%
  mutate(val = case_when(p<0.01 ~ "***",
                         p<0.05 ~ "**",
                         p<0.1 ~ "*", 
                         TRUE ~ "")) %>%
  select(Variable, HR, p, val)
print(H1sep_tidy_extract_coxme)


### Panel 3 (Host-led)----------------------------------------------------------
H1host_duration_coxFE <- coxme(Surv(ep_t0, ep_t1, as.numeric(sp2_ended)) ~ sanct_suppressed_unilathost + 
                                 target_milper + dom_sanct + 
                                 ss_troop_dum + ss_operational_dum + ss_material_dum
                               + (1|rebelid_iso3), # Two-way random effects 
                               locsurv_2_cmptble_weak_gdp)



### Extract results:
H1host_tidy_extract_coxme <- tidy(H1host_duration_coxFE)
H1host_tidy_extract_coxme$HR <- exp(H1host_tidy_extract_coxme$estimate)
H1host_tidy_extract_coxme <- H1host_tidy_extract_coxme %>%
  rename(Variable=term, coef=estimate, p=p.value) %>%
  mutate(val = case_when(p<0.01 ~ "***",
                         p<0.05 ~ "**",
                         p<0.1 ~ "*", 
                         TRUE ~ "")) %>%
  select(Variable, HR, p, val)
print(H1host_tidy_extract_coxme)

## Merge the two
H1_master <- merge(x=H1conc_tidy_extract_coxme, y=H1sep_tidy_extract_coxme, by="Variable", all=T)
H1_master <- merge(x=H1_master, y=H1host_tidy_extract_coxme, by="Variable", all=T)

# Reorder
H1_master <- H1_master %>% 
  mutate( # Make an order var
    Rank = ifelse(Variable == "sanct_encircled_global", 1,  
                  ifelse(Variable == "sanct_encircled_global_boots_sole", 2,  
                         ifelse(Variable == "sanct_encircled_global_joint_sole", 3,  
                                ifelse(Variable == "sanct_suppressed_unilathost", 4, 
                                       ifelse(Variable == "target_milper", 5, 
                                              ifelse(Variable == "dom_sanct", 6,
                                                     ifelse(Variable == "ss_troop_dum", 7,
                                                            ifelse(Variable == "ss_operational_dum", 8,
                                                                   ifelse(Variable == "ss_material_dum", 9, NA
                                                                   )))))))))) %>%
  arrange(Rank) %>% 
  select(-c(Rank)) %>%
  mutate(Variable = case_when(Variable == "sanct_encircled_global" ~ "Encircled Crackdown", 
                              Variable == "sanct_encircled_global_boots_sole" ~ "Intrusive Crackdown", 
                              Variable == "sanct_encircled_global_joint_sole" ~ "Joint Crackdown", 
                              Variable == "sanct_suppressed_unilathost" ~ "Disjointed Crackdown", 
                              Variable == "target_milper" ~ "Target-state soldiers", 
                              Variable == "dom_sanct" ~ "Domestic control", 
                              Variable == "ss_troop_dum" ~ "Troop support", 
                              Variable == "ss_operational_dum" ~ "Operational support", 
                              Variable == "ss_material_dum" ~ "Material support", 
                              TRUE ~ Variable))

# Round to 5 decimals
H1_master <- H1_master %>% mutate(across(where(is.numeric), ~ round(., 4)))

# Remove xy tags
colnames(H1_master) = gsub("\\.x\\b", "", colnames(H1_master))# Remove .x 
colnames(H1_master) = gsub("\\.y\\b", "", colnames(H1_master)) # Remove .y

### Print LATEX
options(knitr.kable.NA = '', knitr.table.format = "latex") # Remove ugly NAs
H1_master_kable <- kable(H1_master, format = 'latex', 
                         booktabs = T,
                         bottomrule = '',
                         toprule = '',
                         midrule = '') %>%
  row_spec(0, hline_after = T) %>% # Make line under first row
  row_spec(9, hline_after = T) %>% # Make line under last row
  row_spec(row = 0, # Make first row bold
           bold = TRUE) %>%
  column_spec(column = 1, # Add border
              border_left = TRUE) %>%
  column_spec(column = (H1_master %>% nrow()-1),
              border_right = TRUE) %>%
  collapse_rows(columns = 1,
                valign = "middle") %>%
  add_header_above(header = c(" " = 1, "Model 1" = 3, "Model 2" = 3, "Model 3" = 3),
                   bold = TRUE,
                   border_left = TRUE,
                   border_right = TRUE) %>%
  kable_styling(font_size = 9) %>% footnote(alphabet = c("\\\\scriptsize{$n$=1,101. Model: Cox PH. Test of violated PH assumption rejected ($a$=.01).}",
                                                         "\\\\scriptsize{***=p$<$0.01, **=p$<$0.05, *=p$<$0.1.}"), 
                                            escape = F) 


### ------------------------------- ###
###          Sensitivity 2         ###
### ------------------------------- ###

### ---------------------- ###
###    Influential dyads
### ---------------------- ###

library(survival)
library(coxme)

# Fit the Cox PH model with random effects
A_switch_coxme <- coxme(Surv(ep_t0, ep_t1, as.numeric(sp2_ended)) ~ sanct_encircled_global + 
                          target_milper + dom_sanct + 
                          ss_troop_dum + ss_operational_dum + ss_material_dum
                        + (1|rebelid_iso3), # Two-way random effects 
                        locsurv_2_cmptble)

A_switch_coxme 

# Extract random effects
random_effects <- ranef(A_switch_coxme)$rebelid_iso3

### ------------------ ###
###    Panel 1         ###
### ------------------ ###
# Function to exclude top N influential groups based on random effects and rerun the Cox PH model:
run_exclusion_model_coxph <- function(exclude_n = 0) {
  # Identify groups with the largest absolute random effects
  influential_groups <- names(sort(abs(random_effects), decreasing = TRUE)[1:exclude_n])
  
  # Exclude these groups from the dataset:
  data_excluded <- locsurv_2_cmptble[!locsurv_2_cmptble$rebelid_iso3 %in% influential_groups, ]
  
  # Rerun the Cox PH model on the reduced dataset:
  model_excluded <- coxme(Surv(ep_t0, ep_t1, as.numeric(sp2_ended)) ~ sanct_encircled_global + 
                            target_milper + dom_sanct + 
                            ss_troop_dum + ss_operational_dum + ss_material_dum
                          + (1|rebelid_iso3), # Two-way random effects 
                          data_excluded)
  
  return(summary(model_excluded))
}

# Exclude the top n most influential groups based on random effects and rerun the model:
results_excluded <- run_exclusion_model_coxph(8) 

# Print the results
print(results_excluded)


### ------------------ ###
###    Panel 2         ###
### ------------------ ###
# Function to exclude top N influential groups based on random effects and rerun the Cox PH model:
run2_exclusion_model_coxph <- function(exclude_n = 8) {
  # Identify groups with the largest absolute random effects
  influential_groups <- names(sort(abs(random_effects), decreasing = TRUE)[1:exclude_n])
  
  # Exclude these groups from the dataset:
  data_excluded <- locsurv_2_cmptble[!locsurv_2_cmptble$rebelid_iso3 %in% influential_groups, ]
  
  # Rerun the Cox PH model on the reduced dataset:
  model_excluded <- coxme(Surv(ep_t0, ep_t1, as.numeric(sp2_ended)) ~ sanct_encircled_global_boots_sole +
                            sanct_encircled_global_joint_sole + 
                            target_milper + dom_sanct + 
                            ss_troop_dum + ss_operational_dum + ss_material_dum
                          + (1|rebelid_iso3), # Two-way random effects 
                          data_excluded)
  
  return(summary(model_excluded))
}

# Exclude the top n most influential groups based on random effects and rerun the model:
results_excluded2 <- run2_exclusion_model_coxph(8) 


### ------------------ ###
###    Panel 3         ###
### ------------------ ###
# Function to exclude top N influential groups based on random effects and rerun the Cox PH model:
run3_exclusion_model_coxph <- function(exclude_n = 8) {
  # Identify groups with the largest absolute random effects
  influential_groups <- names(sort(abs(random_effects), decreasing = TRUE)[1:exclude_n])
  
  # Exclude these groups from the dataset:
  data_excluded <- locsurv_2_cmptble[!locsurv_2_cmptble$rebelid_iso3 %in% influential_groups, ]
  
  # Rerun the Cox PH model on the reduced dataset:
  model_excluded <- coxme(Surv(ep_t0, ep_t1, as.numeric(sp2_ended)) ~ sanct_suppressed_unilathost +
                            target_milper + dom_sanct + 
                            ss_troop_dum + ss_operational_dum + ss_material_dum
                          + (1|rebelid_iso3), # Two-way random effects 
                          data_excluded) # Use the filtered dataset here
  
  return(summary(model_excluded))
}

# Exclude the top n most influential groups based on random effects and rerun the model:
results_excluded3 <- run3_exclusion_model_coxph(8) 



