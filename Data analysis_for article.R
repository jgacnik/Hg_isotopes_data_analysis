###############################################################################
# DATA ANALYSIS AND VISUALIZATION for Gačnik et al. (2024)
# CITATION: Gačnik, J., Živković, I., Horvat, M.: Mercury isotopes in the atmosphere: synthesis, perspectives, and analytical considerations; Trends in Analytical Chemistry; 2024
# AUTHOR: JAN GAČNIK, JUNE 2024
# R version: 4.4.0
###############################################################################

# Do this step the first time only! Installation of needed packages
install.packages(c("tidyverse", "ggforce", "lmodel2", "multcomp", "ggpmisc", "readxl"))

# CHANGE THE FOLLOWING PARAMETERS:
path <- "ENTER_PATH_HERE" # Change to your working directory path, use "/" and not "\", make sure the literature data .xlsx file is in the same directory
data_filename <- "Literature data all.xlsx" # Enter file name of the data containing Hg isotope data (available in Supplement), has to be a .csv file
palette_fig1 <- c("honeydew3", "burlywood3", "bisque4", "grey20") # Color palette for figure 1 plot
palette_fig2a <- c("grey30", "olivedrab3") # Color palette for figure 2a plot
palette_fig2b <- c("black", "grey30", "dodgerblue2", "dodgerblue3") # Color palette for figure 2b plot
palette_fig3 <- c("burlywood3", "grey20") # Color palette for figure 3a and 3b plots
palette_fig4 <- c("#003f5c","#58508d","#bc5090", "#ff6361", "#ffa600") # Color palette for figure 4a and figure 4b plots
palette_fig5a <- c("#003f5c","#58508d","#bc5090", "#ff6361", "#ffa600") # Color palette for figure 5a plot
palette_fig5b <- c("#003f5c","#58508d","#bc5090", "#ff6361", "#ffa600") # Color palette for figure 5b plot
palette_fig6 <- c("#5E96A1", "#C3483C") # Color palette for figure 6 plot

###############################################################################
# CODE BELOW DOES NOT NEED ALTERING

###############################################################################
# READING, FILTERING and RESTRUCTURING data
setwd(path)
lapply(c("tidyverse", "ggforce", "lmodel2", "multcomp", "ggpmisc", "readxl"), require, character.only = TRUE)
data <- read_excel(data_filename, skip = 1) %>%
  mutate(Year = as.numeric(str_extract(Author,"(\\d+)")))

data_meta <- mutate(data, Species = case_when(
  Species == "GEM"  ~ "TGM/GEM",
  Species == "TGM"  ~ "TGM/GEM",
  TRUE ~ Species)) %>%
  mutate(Author = case_when(
  Author == "Tang 2017*"  ~ "Tang 2017",
  TRUE ~ Author)) %>%
  dplyr::select(Author, Species, Year) %>%
  unique()

data_ambient <- subset(data, data$Type != "Emission natural" & data$Type != "Emission anthropogenic"& data$Type != "Contaminated") %>%
  mutate(Species = case_when(
    Species == "GOM"  ~ "RM",
    Species == "PBM"  ~ "RM",
    Species == "GEM"  ~ "TGM/GEM",
    Species == "TGM"  ~ "TGM/GEM",
    TRUE ~ Species))

data_ambient_tgm <- subset(data, (data$Type != "Emission natural"& data$Type != "Emission anthropogenic") & (data$Species == "TGM" |  data$Species == "GEM") & data$Type != "Contaminated") %>%
  mutate(Species = case_when(
    Species == "GEM"  ~ "TGM",
    TRUE ~ Species))

data_ambient_tgm_noMB <- subset(data_ambient_tgm, data_ambient_tgm$Type != "Marine boundary")

data_ambient_rm <- subset(data, (data$Type != "Emission natural" & data$Type != "Emission anthropogenic") & (data$Species != "TGM" & data$Species != "GEM"))

data_emission <- subset(data, data$Type == "Emission natural" | data$Type == "Emission anthropogenic") %>%
  mutate(Species = case_when(
    Species == "GOM"  ~ "RM",
    Species == "PBM"  ~ "RM",
    Species == "GEM"  ~ "TGM/GEM",
    Species == "TGM"  ~ "TGM/GEM",
    TRUE ~ Species))

data_emission_2 <- subset(data, data$Type == "Emission anthropogenic") %>%
  mutate(Species = case_when(
    Species == "GOM"  ~ "RM",
    Species == "PBM"  ~ "RM",
    Species == "GEM"  ~ "TGM/GEM",
    Species == "TGM"  ~ "TGM/GEM",
    TRUE ~ Species)) %>% 
  mutate(Type = case_when(
    Author == "Tang 2017"  ~ "Tang 2017 - coal",
    Author == "Tang 2017*"  ~ "Tang 2017 - coal with WFGD",
    Author == "Li 2021"  ~ "Li 2021a - cement",
    Author == "Li 2021b"  ~ "Li 2021b - coal",
    TRUE ~ Type))

boxplot_data <- data_ambient_tgm %>%
  pivot_longer(cols = c(8, 22), names_to = "Delta type", values_to = "Value") 

boxplot_data_RM <- data_ambient %>%
  pivot_longer(cols = c(8, 22), names_to = "Delta type", values_to = "Value")

boxplot_data_PAS <- data_ambient_tgm %>%
  pivot_longer(cols = c(8, 10, 12, 14, 16, 18, 20, 22), names_to = "Delta type", values_to = "Value") %>%
  mutate(`Delta type` = case_when(
    `Delta type` == "d204Hg"  ~ "\u03b4^204*Hg",
    `Delta type` == "d202Hg"  ~ "\u03b4^202*Hg",
    `Delta type` == "d201Hg"  ~ "\u03b4^201*Hg",
    `Delta type` == "d200Hg"  ~ "\u03b4^200*Hg",
    `Delta type` == "d199Hg"  ~ "\u03b4^199*Hg",
    `Delta type` == "D204Hg"  ~ "\u0394^204*Hg",
    `Delta type` == "D201Hg"  ~ "\u0394^201*Hg",
    `Delta type` == "D200Hg"  ~ "\u0394^200*Hg",
    `Delta type` == "D199Hg"  ~ "\u0394^199*Hg",
    TRUE ~ `Delta type`))

boxplot_data_PAS_urban <- subset(boxplot_data_PAS, boxplot_data_PAS$Type == "Urban/industrial")

boxplot_data_PAS_remote <- subset(boxplot_data_PAS, boxplot_data_PAS$Type == "Remote")

# PLOTS and STATYSTICAL ANALYSES
###############################################################################
# Figure 1: Number of publications for atmospheric Hg isotopes
ggplot(data = data_meta) +
  geom_bar(aes(x = Year, fill = Species), width = 0.8) +
  labs(y = "Number of publications") +
  scale_fill_manual(values = palette_fig1) +
  scale_x_continuous(breaks = seq(2008, 2024, by = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 9, colour = "black", angle = 45, hjust = 1.2, vjust = 1.07),
        axis.title.x = element_blank(),
        axis.text.y =  element_text(size = 9, colour = "black"),
        axis.title.y = element_text(size = 9, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.key.size = unit(0.3, units = "cm"), 
        legend.title = element_blank(),
        legend.text = element_text(size = 8, colour = "black", margin = margin(l = 0, unit = "cm")),
        legend.margin = margin(b = -0.25, unit = "cm"),
        legend.position = "top")
ggsave(filename = "Figure_1.png", device = "png", width = 90, height = 70, units = "mm", dpi = 600)

###############################################################################
# Figure 2: Emission plots and connected statistical analyses
# Figure 2a: natural and anthropogenic
ggplot(data = data_emission) +
  geom_point(aes(x = D199Hg, y = d202Hg, color = Type, shape = Species), size = 1.5) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.2) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.2) +
  scale_x_continuous(limits = c(-1.5,0.8), labels = ~sub("-", "\u2212", .x)) +
  scale_y_continuous(limits = c(-6.5,3.5), labels = ~sub("-", "\u2212", .x)) +
  geom_mark_ellipse(aes(x = D199Hg, y = d202Hg, fill = Type, label = Type),
                    con.cap = 0,
                    label.fontsize = 6.5,
                    label.margin = margin(0.5, 2, 0.5, 2, "mm"),
                    linewidth = 0.2,
                    label.fill = NA,
                    alpha = 0.1,
                    label.buffer = unit(2, "mm"),
                    show.legend = F) +
  scale_color_manual(values = palette_fig2a) +
  scale_fill_manual(values = palette_fig2a) +
  scale_shape_manual(values = c(1, 2)) +
  labs(x = expression("\u0394"^"199"*"Hg (\u2030)"),
       y = expression("\u03B4"^"202"*"Hg (\u2030)")) + 
  guides(fill = "none", color = "none", shape = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7, colour = "black"),
        axis.title.x = element_text(size = 7, colour = "black"),
        axis.text.y = element_text(size = 7, colour = "black"),
        axis.title.y = element_text(size = 7, colour = "black"),
        legend.key = element_rect(fill = NA),
        legend.spacing = unit(0.7, units = "cm"),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.title = element_text(size = 6.5, colour = "black", hjust = 0.5,, margin = margin(r = 0, unit = "cm")),
        legend.title.position = "left",
        legend.text = element_text(size = 6.5, colour = "black", margin = margin(l = -0.1, unit = "cm")),
        legend.margin = margin(b = -0.3, unit = "cm"),
        legend.position = "top")
ggsave(filename = "Figure_2a.png", device = "png", width = 90, height = 80, units = "mm", dpi = 600)

# Emissions 2b: anthropogenic only
ggplot(data = data_emission_2) +
  geom_point(aes(x = D199Hg, y = d202Hg, color = Type, shape = Species), size = 1.5) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.2) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.2) +
  scale_x_continuous(limits = c(-1.5,0.8), labels = ~sub("-", "\u2212", .x)) +
  scale_y_continuous(limits = c(-6.5,3.5), labels = ~sub("-", "\u2212", .x)) +
  geom_mark_ellipse(aes(x = D199Hg, y = d202Hg, fill = Type, label = Type),
                    con.cap = 0,
                    label.fontsize = 6.5,
                    label.margin = margin(0.5, 2, 0.5, 2, "mm"),
                    linewidth = 0.2,
                    label.fill = NA,
                    alpha = 0.1,
                    label.buffer = unit(2, "mm"),
                    show.legend = F) +
  scale_color_manual(values = palette_fig2b) +
  scale_fill_manual(values = palette_fig2b) +
  scale_shape_manual(values = c(1, 2)) +
  labs(x = expression("\u0394"^"199"*"Hg (\u2030)"),
       y = expression("\u03B4"^"202"*"Hg (\u2030)")) + 
  guides(color = "none", shape = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7, colour = "black"),
        axis.title.x = element_text(size = 7, colour = "black"),
        axis.text.y = element_text(size = 7, colour = "black"),
        axis.title.y = element_text(size = 7, colour = "black"),
        legend.key.spacing.y = unit(-0.3, units = "cm"),
        legend.key = element_rect(fill = NA),
        legend.spacing = unit(0.7, units = "cm"),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.title = element_text(size = 6.5, colour = "black", hjust = 0.5,, margin = margin(r = 0, unit = "cm")),
        legend.title.position = "left",
        legend.text = element_text(size = 6.5, colour = "black", margin = margin(l = -0.1, unit = "cm")),
        legend.margin = margin(b = -0.3, unit = "cm"),
        legend.position = "top")
ggsave(filename = "Figure_2b.png", device = "png", width = 90, height = 80, units = "mm", dpi = 600)

# Stat tests for Figure 2 data, MANOVA tests with poshoc ANOVA for natural versus anthropogenic emissions
d202Hg <- data_emission$d202Hg
D199Hg <- data_emission$D199Hg
manova_emiss <- manova(cbind(d202Hg, D199Hg) ~ Type, data = data_emission)
summary(manova_emiss)
anova_emiss_d202Hg <- aov(d202Hg ~ Type, data = data_emission)
summary(anova_emiss_d202Hg)
anova_emiss_D199Hg <- aov(D199Hg ~ Type, data = data_emission)
summary(anova_emiss_D199Hg)

###############################################################################
# Figure 3: All ambient data (RM v TGM) plots and connected statistical analyses
# Figure 3a: boxplot
boxplot_data_RM$`Delta type` <- factor(boxplot_data_RM$`Delta type`, labels = c("\u0394^199*Hg", "\u03b4^202*Hg"))   #\u2030
ggplot(data = boxplot_data_RM, aes(x = Value, y = Species, color = Species)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_boxplot(outlier.size = 0.5, linewidth = 0.3) +
  scale_color_manual(values = palette_fig3) +
  scale_x_continuous(labels = ~sub("-", "\u2212", .x)) +
  facet_wrap(~ `Delta type`, labeller = label_parsed, ncol = 1) +
  xlab("\u2030") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, colour = "black"),
        axis.title.x = element_text(size = 8, colour = "black"),
        axis.text.y = element_text(size = 7, colour = "black"),
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.text = element_text(size = 8, colour = "black"),
        legend.position = "none")
ggsave(filename = "Figure_3a.png", device = "png", width = 75, height = 90, units = "mm", dpi = 600)

# Figure 3b: scatter
ggplot(data = data_ambient) +
  geom_point(aes(x = D199Hg, y = d202Hg, color = Species), size = 0.8) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.2) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.2) +
  scale_color_manual(values = palette_fig3) +
  scale_x_continuous(labels = ~sub("-", "\u2212", .x)) +
  scale_y_continuous(labels = ~sub("-", "\u2212", .x)) +
  facet_wrap(~Type, ncol = 2) +
  guides(color = guide_legend(byrow = TRUE)) +
  labs(x = expression("\u0394"^"199"*"Hg (\u2030)"),
       y = expression("\u03B4"^"202"*"Hg (\u2030)")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7, colour = "black"),
        axis.title.x = element_text(size = 7, colour = "black"),
        axis.text.y = element_text(size = 7, colour = "black"),
        axis.title.y = element_text(size = 7, colour = "black"),
        strip.text = element_text(size = 6.5, colour = "black"),
        panel.grid.minor = element_blank(),
        legend.key.spacing.y = unit(-0.2, units = "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 6.5, margin = margin(l = -0.15, unit = "cm")),
        legend.position = "inside",
        legend.position.inside = c(0.7, 0.15))
ggsave(filename = "Figure_3b.png", device = "png", width = 95, height = 90, units = "mm", dpi = 600)

# Supplementary table, creates a .csv file with medians/first quartiles/third quartiles  for RM and TGM/GEM data separated by sampling site type
median_ambient <- data_ambient %>%
  pivot_longer(cols = c("d202Hg", "D199Hg"), names_to = "Delta", values_to = "Value") %>%
  group_by(Species, Type, Delta) %>%
  summarise("Median" = median(Value, na.rm = TRUE), "First quartile" = quantile(Value, 0.25, na.rm = TRUE), "Third quartile" = quantile(Value, 0.75, na.rm = TRUE)) 
write.csv(median_ambient, "TGMGEM vs RM table.csv")

# Stat tests for Figure 3 data, t-tests
t.test(d202Hg ~ Species, data = data_ambient, var.equal = TRUE)
t.test(D199Hg ~ Species, data = data_ambient, var.equal = TRUE)

###############################################################################
# Figure 4: TGM/GEM data, d202 versus D199 plots and statistical analysis
# Figure 4a: scatter with enclosing ellipses
ggplot(data = data_ambient_tgm) +
  geom_point(aes(x = D199Hg, y = d202Hg, color = Type), size = 1) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_mark_ellipse(aes(x = D199Hg, y = d202Hg, fill = Type, label = Type),
                    con.cap = 0,
                    label.fontface = "plain",
                    label.fontsize = 6.5,
                    label.margin = margin(0.5, 2, 0.5, 2, "mm"),
                    linewidth = 0.2,
                    label.fill = NA,
                    alpha = 0.1,
                    label.buffer = unit(3, "mm"),
                    show.legend = F) +
  labs(x = expression("\u0394"^"199"*"Hg (\u2030)"),
       y = expression("\u03B4"^"202"*"Hg (\u2030)")) +
  scale_color_manual(values = palette_fig4) +
  scale_fill_manual(values = palette_fig4) +  
  scale_x_continuous(limits = c(-1.7, 2.2), labels = ~sub("-", "\u2212", .x)) +
  scale_y_continuous(limits = c(-7, 4), labels = ~sub("-", "\u2212", .x)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7, colour = "black"),
        axis.title.x = element_text(size = 7, colour = "black"),
        axis.text.y = element_text(size = 7, colour = "black"),
        axis.title.y = element_text(size = 7, colour = "black"),
        strip.text = element_text(size = 6.5, colour = "black"),
        legend.position = "none")
ggsave(filename = "Figure_4a.png", device = "png", width = 90, height = 90, units = "mm", dpi = 600)

# b) Boxplot
boxplot_data$`Delta type` <- factor(boxplot_data$`Delta type`, labels = c("\u0394^199*Hg", "\u03b4^202*Hg"))
ggplot(data = boxplot_data, aes(x = Value, y = Type, colour = Type)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_boxplot(outlier.size = 0.5, linewidth = 0.3) +
  scale_x_continuous(limits = c(-4, 2.5), labels = ~sub("-", "\u2212", .x)) +
  scale_color_manual(values = palette_fig4) +
  facet_wrap(~ `Delta type`, labeller = label_parsed, ncol = 1) +
  xlab("\u2030") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, colour = "black"),
        axis.title.x = element_text(size = 8, colour = "black"),
        axis.text.y = element_text(size = 7, colour = "black"),
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.text = element_text(size = 8, colour = "black"),
        legend.position = "none")
ggsave(filename = "Figure_4b.png", device = "png", width = 90, height = 90, units = "mm", dpi = 600)

# Stat tests for Figure 4 data, ANVOA with post-hoc Tukey to generate the compat letter display (CLD)
# Tests for d202
data_ambient_tgm$Type = as.factor(data_ambient_tgm$Type)
anova_d202Hg <- aov(d202Hg ~ Type, data = data_ambient_tgm)
summary(anova_d202Hg)
tukey_d202Hg <- TukeyHSD(anova_d202Hg)
a <- glht(anova_d202Hg, linfct = mcp(Type = "Tukey"))
cld(a)

# Tests for D199
anova_D199Hg <- aov(D199Hg ~ Type, data = tgm_data_ambient)
summary(anova_D199Hg)
tukey_D199Hg <- TukeyHSD(anova_D199Hg)
b <- glht(anova_D199Hg, linfct = mcp(Type = "Tukey"))
cld(b)

###############################################################################
# Figure 5: TGM/GEM data, D199 versus D201 plot and D200 versus D204 plot + statistical analysis
# Figure 5a: D199 versus D201 scatter and linear regression
ggplot(data = data_ambient_tgm) +
  geom_point(aes(x = D201Hg, y = D199Hg, color = Type), size = 1) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.2) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.2) +
  geom_abline(slope = 1, intercept = 0) +
  annotate(geom = "text", x = 1, y = 0.75, label = "1:1 line", size = 2.1) +
  scale_color_manual(values = palette_fig5a) + 
  scale_x_continuous(labels = ~sub("-", "\u2212", .x)) +
  scale_y_continuous(labels = ~sub("-", "\u2212", .x)) +
  stat_ma_line(aes(x = D201Hg, y = D199Hg), linewidth = 0.6, method = "RMA",
              range.y = "interval", range.x = "interval") +
  stat_ma_eq(mapping = use_label("eq", "R2", sep = "*`;`~", aes(x = D201Hg, y = D199Hg)), method = "RMA",
             range.y = "interval", range.x = "interval", size = 2.1) +
  facet_wrap(~ Type, ncol = 2) +
  labs(x = expression("\u0394"^"201"*"Hg (\u2030)"),
       y = expression("\u0394"^"199"*"Hg (\u2030)")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7, colour = "black"),
        axis.title.x = element_text(size = 7, colour = "black"),
        axis.text.y = element_text(size = 7, colour = "black"),
        axis.title.y = element_text(size = 7, colour = "black"),
        strip.text = element_text(size = 6.5, colour = "black"),
        panel.grid.minor = element_blank(),
        legend.position = "none")
ggsave(filename = "Figure_5a.png", device = "png", width = 90, height = 110, units = "mm", dpi = 600)

# Figure 5b: D200 versus D204 scatter and linear regression, no marine-boundary data
ggplot(data = data_ambient_tgm_noMB) + 
  geom_point(aes(x = D204Hg, y = D200Hg, color = Type), size = 1) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.2) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.2) +
  scale_color_manual(values = palette_fig5b) +
  scale_x_continuous(labels = ~sub("-", "\u2212", .x)) +
  scale_y_continuous(labels = ~sub("-", "\u2212", .x)) +
  stat_ma_line(aes(x = D204Hg, y = D200Hg), linewidth = 0.6, method = "RMA",
               range.y = "interval", range.x = "interval") +
  stat_ma_eq(mapping = use_label("eq", "R2", sep = "*`;`~", aes(x = D204Hg, y = D200Hg)), method = "RMA",
             range.y = "interval", range.x = "interval", size = 2.1) +
  facet_wrap(~Type, ncol = 2) + 
  labs(x = expression("\u0394"^"204"*"Hg (\u2030)"),
       y = expression("\u0394"^"200"*"Hg (\u2030)")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7, colour = "black"),
        axis.title.x = element_text(size = 7, colour = "black"),
        axis.text.y = element_text(size = 7, colour = "black"),
        axis.title.y = element_text(size = 7, colour = "black"),
        strip.text = element_text(size = 6.5, colour = "black"),
        panel.grid.minor = element_blank(),
        legend.position = "none")
ggsave(filename = "Figure_5b.png", device = "png", width = 90, height = 77, units = "mm", dpi = 600)

###############################################################################
# Figure 6: TGM data, passive versus active approaches, d202 versus D199
data_ambient_tgm <- data_ambient_tgm[order(data_ambient_tgm$Subspecies, decreasing= FALSE), ]
ggplot(data = data_ambient_tgm) +
  geom_point(aes(x = D199Hg, y = d202Hg, color = Subspecies, shape = Subspecies), size = 1) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.2) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.2) +
  scale_x_continuous(labels = ~sub("-", "\u2212", .x)) +
  scale_y_continuous(labels = ~sub("-", "\u2212", .x)) +
  scale_color_manual(values = palette_fig6) +
  facet_wrap(~Type, nrow = 2) +
  labs(x = expression("\u0394"^"199"*"Hg (\u2030)"),
       y = expression("\u03B4"^"202"*"Hg (\u2030)"),
       color = "Sampling type",
       shape = "Sampling type") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7, colour = "black"),
        axis.title.x = element_text(size = 7, colour = "black"),
        axis.text.y = element_text(size = 7, colour = "black"),
        axis.title.y = element_text(size = 7, colour = "black"),
        strip.text = element_text(size = 6.5, colour = "black"),
        legend.text = element_text(size = 6.5, margin = margin(l = -0.15, unit = "cm")),
        legend.background = element_blank(),
        legend.key.spacing.y = unit(-0.2, units = "cm"),
        legend.title = element_text(size = 6.5, colour = "black", margin = margin(b = 0.02, unit = "cm")),
        legend.position = "inside",
        legend.position.inside = c(0.82, 0.25))
ggsave(filename = "Figure_6.png", device = "png", width = 90, height = 70, units = "mm", dpi = 600)

# Supplementary table, creates a .csv file with medians/first quartiles/third quartiles  for passive versus active sampling at urban and remote sites
median_remote <- boxplot_data_PAS_remote %>%
  group_by(`Delta type`, Subspecies) %>%
  summarise("Median" = median(Value, na.rm = TRUE), "First quartile" = quantile(Value, 0.25, na.rm = TRUE), "Third quartile" = quantile(Value, 0.75, na.rm = TRUE)) %>%
  mutate("Site type" = "Remote", "Median" = sub("-", "\u2212", Median))
median_urban <- boxplot_data_PAS_urban %>%
  group_by(`Delta type`, Subspecies) %>%
  summarise("Median" = median(Value, na.rm = TRUE), "First quartile" = quantile(Value, 0.25, na.rm = TRUE), "Third quartile" = quantile(Value, 0.75, na.rm = TRUE)) %>%
  mutate("Site type" = "Urban", , "Median" = sub("-", "\u2212", Median))
median_all <- rbind(median_remote, median_urban)
write.csv(median_all, "Pass vs act table.csv")