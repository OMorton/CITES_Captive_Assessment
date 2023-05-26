.libPaths("C:/Packages") ## Set up for working from home.

library(tidyverse)
library(ggridges)
library(viridis)
library(ggpubr)

source("2_Assessment_Pipeline/Functions.R")
options(scipen = 999)

sim.dat <- expand.grid(vol = c(1, 10, 100, 1000, 10000, 100000), 
                       ED = c(0, 0.25, 0.5, 1, 2),
                       FS = c(0, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6)) %>%
  mutate(score = (log10(vol) + ED)*FS)

#### Data ####
AvesRept_checked_WOE <- data.table::fread("Outputs/Full_Check/BirdsRept/AvesRept_checked_WOE.csv", na.strings = "")
AvesRept_checked_TERM <- data.table::fread("Outputs/Full_Check/BirdsRept/AvesRept_checked_Term.csv", na.strings = "")
length(unique(AvesRept_checked_TERM$Exporter))

#### Scores ####

AvesRept_checked_WOE <- AvesRept_checked_WOE %>%
  mutate(Check_10 = ifelse(Appendix != "I", FALSE, Check_10),
         Check_11 = ifelse(Appendix != "I", FALSE, Check_11),
         Check_12 = ifelse(Appendix != "I", FALSE, Check_12),
         Check_13 = ifelse(Appendix != "I", FALSE, Check_13)) %>%
  mutate(Volume_trends_grp = case_when((Check_1 + Check_2 + Check_3) == 0 ~ 0,
                                       (Check_1 + Check_2 + Check_3) == 1 ~ 1,
                                       (Check_1 + Check_2 + Check_3) == 2 ~ 1.5,
                                       (Check_1 + Check_2 + Check_3) == 3 ~ 1.5),
         Code_switch_grp = case_when((Check_4 + Check_5 + Check_6) == 0 ~ 0,
                                       (Check_4 + Check_5 + Check_6) == 1 ~ 1,
                                       (Check_4 + Check_5 + Check_6) == 2 ~ 1.5,
                                       (Check_4 + Check_5 + Check_6) == 3 ~ 1.5),
         Legal_acq_grp = case_when((Check_7 + Check_8 + Check_9) == 0 ~ 0,
                                   (Check_7 + Check_8 + Check_9) == 1 ~ 1,
                                   (Check_7 + Check_8 + Check_9) == 2 ~ 1.5,
                                   (Check_7 + Check_8 + Check_9) == 3 ~ 1.5),
         Reporter_inc_grp = case_when(((Check_10 + Check_11 + Check_12 + Check_13) == 0 & Appendix == "I") ~ 0,
                                      ((Check_10 + Check_11 + Check_12 + Check_13) == 1 & Appendix == "I") ~ 1,
                                      ((Check_10 + Check_11 + Check_12 + Check_13) == 2 & Appendix == "I") ~ 1.5,
                                      ((Check_10 + Check_11 + Check_12 + Check_13) == 3 & Appendix == "I") ~ 1.5,
                                      ((Check_10 + Check_11 + Check_12 + Check_13) == 4 & Appendix == "I") ~ 1.5,
                                      ((Check_10 + Check_11 + Check_12 + Check_13) == 0 & Appendix != "I") ~ 0,
                                      ((Check_10 + Check_11 + Check_12 + Check_13) == 1 & Appendix != "I") ~ 0,
                                      ((Check_10 + Check_11 + Check_12 + Check_13) == 2 & Appendix != "I") ~ 0,
                                      ((Check_10 + Check_11 + Check_12 + Check_13) == 3 & Appendix != "I") ~ 0,
                                      ((Check_10 + Check_11 + Check_12 + Check_13) == 4 & Appendix != "I") ~ 0)) %>%
  mutate(Fail_score = Volume_trends_grp + Code_switch_grp + Legal_acq_grp + Reporter_inc_grp,
         #Thr_score = case_when(IUCN_code == "LC" ~ 0,
          #                     IUCN_code == "NT" ~ 0.5, 
           #                    IUCN_code == "VU" ~ 1,
            #                   IUCN_code == "EN" ~ 1.5,
             #                  IUCN_code == "CR" ~ 2,
              #                 IUCN_code == "EW" ~ 2,
               #                IUCN_code == "EX" ~ 2,
                #               IUCN_code == "Not assessed" ~ 2),
         Thr_score = case_when(IUCN_code == "LC" ~ 0,
                               IUCN_code == "NT" ~ 0.25, 
                               IUCN_code == "VU" ~ 0.5,
                               IUCN_code == "EN" ~ 1,
                               IUCN_code == "CR" ~ 2,
                               IUCN_code == "EW" ~ 2,
                               IUCN_code == "EX" ~ 2,
                               IUCN_code == "Not assessed" ~ 0.5,
                               IUCN_code == "DD" ~ 0.5),
         Ovr_score = (log10(Vol) + Thr_score) * Fail_score,
         Row = "A")

write.csv(AvesRept_checked_WOE, "Outputs/Full_Check/BirdsRept/AvesRept_checked_WOE_score.csv", na = "")

pts_score <- ggplot(AvesRept_checked_WOE, aes(Ovr_score, Vol, colour = Ovr_score)) +
  geom_point(alpha = .5, shape = 16) +
  scale_colour_viridis(option = "C") +
  scale_y_log10() +
  ylab("Volume exported") +
  #facet_wrap(~Class) +
  xlab("") +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank())

FS_score <- ggplot(AvesRept_checked_WOE, aes(Ovr_score, as.factor(Fail_score), colour = Ovr_score)) +
  geom_point(position = position_jitter(), alpha = .25, shape = 16) +
  geom_boxplot(outlier.shape = NA, fill = "white", alpha = .5) +
  scale_colour_viridis(option = "C") +
  coord_cartesian(xlim = c(0, 24), expand = FALSE) +
  #facet_wrap(~Class) +
  ylab("Check groups failed") +
  xlab("") +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank())

ED_score <- ggplot(AvesRept_checked_WOE, aes(Ovr_score, as.factor(Thr_score), colour = Ovr_score)) +
  geom_point(position = position_jitter(), alpha = .25, shape = 16) +
  geom_boxplot(outlier.shape = NA, fill = "white", alpha = .5) +
  scale_colour_viridis(option = "C") +
  #facet_wrap(~Class) +
  ylab("IUCN category") +
  xlab("Overall priority score") +
  scale_y_discrete(labels=c("0" = "LC", "0.25" = "NT", "0.5" = "VU/NE", "1" = "EN",
                            "2" = "CR")) +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank())

Distr_score <- ggplot(AvesRept_checked_WOE, aes(Ovr_score, Row, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 1, rel_min_height = 0.0001) +
  scale_fill_viridis(option = "C") +
  coord_cartesian(xlim = c(0, 24), expand = FALSE) +
  xlab("Overall priority score") +
  ylab("") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", axis.text.y = element_blank())

#### Arrangement 
Diag_plot <- ggarrange(Distr_score, nrow = 2, labels = c("A.", ""),
          ggarrange(pts_score, ED_score, FS_score, 
                    common.legend = TRUE, legend = "none", nrow = 1, labels = c("B.", "C.", "D.")),
          heights = c(0.75, 1))

ggsave("Summaries/Figures/Diagnostic_plt.png", Diag_plot, device = "png", 
       width = 10, height = 7, bg = "white")

#### Count per category ####

Sum_totalsI <- checks_summary(data = AvesRept_checked_WOE, groups = c("Appendix"), 
                              App1_only = TRUE, format = "long")  %>%
  mutate(Check = factor(Check, levels = c("Check_1", "Check_2", "Check_3", "Check_4", "Check_5",
                                          "Check_6", "Check_7", "Check_8", "Check_9", "Check_10",
                                          "Check_11", "Check_12", "Check_13")),
         prop = count/Total_count)
## 33937 total records
nrow(AvesRept_checked_WOE %>% filter(Appendix != "I"))

Sum_totalsII <- checks_summary(data = AvesRept_checked_WOE, groups = c("Appendix"), 
                             App1_only = FALSE, format = "long") %>% filter(Appendix != "I") %>% 
  group_by(Check) %>% tally(count) %>% 
  rbind(data.frame(Check = c("Check_10", "Check_11", "Check_12", "Check_13"), n = 0)) %>%
  mutate(Check = factor(Check, levels = c("Check_1", "Check_2", "Check_3", "Check_4", "Check_5",
                                          "Check_6", "Check_7", "Check_8", "Check_9", "Check_10",
                                          "Check_11", "Check_12", "Check_13")),
         total = 33937, prop = n/total)

check_tally_plotI <- ggplot(Sum_totalsI, aes(Check, count)) + 
  geom_rect(data = data.frame(Check = 1, count = 1), xmin = 0, xmax = 3.5, ymin = -Inf, ymax = Inf, fill = "lightskyblue1", alpha = .4) +
  geom_rect(data = data.frame(Check = 1, count = 1), xmin = 3.5, xmax = 6.5, ymin = -Inf, ymax = Inf, fill = "darkseagreen2", alpha = .4) +
  geom_rect(data = data.frame(Check = 1, count = 1), xmin = 6.5, xmax = 9.5, ymin = -Inf, ymax = Inf, fill = "rosybrown1", alpha = .4) +
  geom_rect(data = data.frame(Check = 1, count = 1), xmin = 9.5, xmax = 113.5, ymin = -Inf, ymax = Inf, fill = "lightgoldenrod", alpha = .4) +
  geom_col(fill = "grey", colour = "black", width = .75) + 
  coord_cartesian(ylim = c(1, 20000), expand = FALSE) +
  scale_x_discrete(labels = c("Check_1" = "1", "Check_2" = "2", "Check_3" = "3", "Check_4" = "4", 
                          "Check_5" = "5", "Check_6" = "6", "Check_7" = "7", "Check_8" = "8", 
                          "Check_9" = "9", "Check_10" = "10", "Check_11" = "11", "Check_12" = "12",
                          "Check_13" = "13")) +
  annotate("text", label = "Volume trends", x = 2, y = 10000, fontface = "bold", size = 3) +
  annotate("text", label = "Code\n switching", x = 5, y = 10000, fontface = "bold", size = 3) +
  annotate("text", label = "Legal acquisition", x = 8, y = 10000, fontface = "bold", size = 3) +
  annotate("text", label = "Reporting inconsistencies", x = 11.5, y = 10000, fontface = "bold", size = 3) +
  ylab("Tally of fails") +
  scale_y_log10() +
  theme_minimal(base_size = 14)

check_tally_plotII <- ggplot(Sum_totalsII, aes(Check, n)) + 
  geom_rect(data = data.frame(Check = 1, n = 1), xmin = 0, xmax = 3.5, ymin = -Inf, ymax = Inf, fill = "lightskyblue1", alpha = .4) +
  geom_rect(data = data.frame(Check = 1, n = 1), xmin = 3.5, xmax = 6.5, ymin = -Inf, ymax = Inf, fill = "darkseagreen2", alpha = .4) +
  geom_rect(data = data.frame(Check = 1, n = 1), xmin = 6.5, xmax = 9.5, ymin = -Inf, ymax = Inf, fill = "rosybrown1", alpha = .4) +
  geom_rect(data = data.frame(Check = 1, n = 1), xmin = 9.5, xmax = 113.5, ymin = -Inf, ymax = Inf, fill = "lightgoldenrod", alpha = .4) +
  geom_col(fill = "grey", colour = "black", width = .75) + 
  coord_cartesian(ylim = c(1, 75000), expand = FALSE) +
  scale_x_discrete(labels = c("Check_1" = "1", "Check_2" = "2", "Check_3" = "3", "Check_4" = "4", 
                              "Check_5" = "5", "Check_6" = "6", "Check_7" = "7", "Check_8" = "8", 
                              "Check_9" = "9", "Check_10" = "10", "Check_11" = "11", "Check_12" = "12",
                              "Check_13" = "13")) +
  annotate("text", label = "Volume trends", x = 2, y = 40000, fontface = "bold", drop = FALSE, size = 3) +
  annotate("text", label = "Code\n switching", x = 5, y = 40000, fontface = "bold", size = 3) +
  annotate("text", label = "Legal acquisition", x = 8, y = 40000, fontface = "bold", size = 3) +
  annotate("text", label = "Reporting inconsistencies", x = 11.5, y = 40000, fontface = "bold", size = 3) +
  ylab("Tally of fails") +
  scale_y_log10() +
  theme_minimal(base_size = 14)

AvesRept_checked_WOE <- AvesRept_checked_WOE %>% arrange(Score, App_sum) %>% mutate(sc_order = 1:n())
tally <- AvesRept_checked_WOE %>% group_by(Class, App_sum, Score) %>% tally()
AvesRept_checked_WOE %>% group_by(Class, App_sum) %>% summarise(mean = mean(Score))

score_tally_plt <- ggplot(AvesRept_checked_WOE, aes(Score, sc_order, colour = App_sum)) + 
  geom_point(shape = 15) +
  scale_color_manual(values = c("tomato", "black"), name = "Appendix") +
  xlab("Failed checks") +
  ylab("Cumulative records") +
  theme_minimal(base_size = 14) +
  theme(legend.position=c(.75,.15), legend.title = element_text(face = "bold", size = 8),
        legend.background = element_rect(fill = "grey90", colour = NA),
        legend.key.size = unit(.5, 'cm'))

check_tally_plot <- ggarrange(ggarrange(check_tally_plotI, check_tally_plotII, nrow = 2, 
                    labels = c("A.", "B.")),
          score_tally_plt, labels = c("", "C."), ncol = 2, widths = c(1, 0.5))

ggsave("Summaries/Figures/Checkcount_plt.png", check_tally_plot, device = "png", 
       width = 10, height = 6, bg = "white")            

#### species prioritisation - Aves ####
sp10_rankings <- AvesRept_checked_WOE %>% 
  ## remove the extinct sp
  #filter(Taxon != "Rhodonessa caryophyllacea") %>%
  select(Year, Taxon, Class, Order, Exporter, Vol, IUCN_code, ROW_ID, Appendix, 69:111) %>%
  unite("ID", c(Year, Taxon, Exporter), remove = FALSE) %>% group_by(Class) %>%
  slice_max(Ovr_score, n = 10) %>%
  mutate(IUCN_code = ifelse(IUCN_code == "Not assessed", "NE", IUCN_code))

sp10_checks <- AvesRept_checked_WOE %>%
  ## remove the extinct sp
  #filter(Taxon != "Rhodonessa caryophyllacea") %>%
  group_by(Class) %>%
  slice_max(Ovr_score, n = 10) %>%
  select(Year, Taxon, Exporter, Check_1, Check_2, Check_3, Check_4, 
         Check_5, Check_6, Check_7, Check_8,
         Check_9, Check_10, Check_11, Check_12, Check_13) %>%
  unite("ID", c(Year, Taxon, Exporter), remove = TRUE) %>%
  pivot_longer(!c(ID, Class), names_to = "Check", values_to = "Fail") %>%
  filter(Fail == TRUE) %>%
  mutate(Check = factor(Check, levels = c("Check_1", "Check_2", "Check_3", "Check_4", "Check_5",
                                          "Check_6", "Check_7", "Check_8", "Check_9", "Check_10",
                                          "Check_11", "Check_12", "Check_13"))) %>%
  left_join(select(sp10_rankings, ID, Ovr_score)) %>%
  group_by(Class) %>% arrange(Ovr_score) %>% group_by(ID, Class)%>%
  mutate(Num = cur_group_id())

Aves_plt_order <- sp10_rankings %>% filter(Class == "Aves") %>% arrange(Ovr_score)

sp10_plt_aves <- ggplot(filter(sp10_checks, Class == "Aves"), aes(Check, reorder(ID, Ovr_score))) +
  geom_point(colour = NA) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2000_Agapornis personatus_CN", Ovr_score = 1), xmin = 5.5, xmax = 8.5, 
            ymin = -Inf, ymax = Inf, fill = "lightskyblue1", alpha = .4) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2000_Agapornis personatus_CN", Ovr_score = 1),xmin = 8.5, xmax = 11.5, 
            ymin = -Inf, ymax = Inf, fill = "darkseagreen2", alpha = .4) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2000_Agapornis personatus_CN", Ovr_score = 1),xmin = 11.5, xmax = 14.5, 
            ymin = -Inf, ymax = Inf, fill = "rosybrown1", alpha = .4) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2000_Agapornis personatus_CN", Ovr_score = 1),xmin = 14.5, xmax = 18.5, 
            ymin = -Inf, ymax = Inf, fill = "lightgoldenrod", alpha = .4) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2000_Agapornis personatus_CN", Ovr_score = 1),xmin = 18.5, xmax = 19.5, 
            ymin = -Inf, ymax = 10.5, fill = "grey85", alpha = .4) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2000_Agapornis personatus_CN", Ovr_score = 1), xmin = -Inf, xmax = Inf, 
            ymin = 10.5, ymax = 13, fill = "white") +
  geom_point(shape = 4, colour = "darkred", size = 4) +
  scale_x_discrete(drop=FALSE, limits = c("Vol", "Exporter","Year",  "IUCN", "App", "Check_1", "Check_2", "Check_3", "Check_4", "Check_5",
                                          "Check_6", "Check_7", "Check_8", "Check_9", "Check_10",
                                          "Check_11", "Check_12","Check_13", "Score"),
                   labels = c("Vol", "Exporter", "Year",  "IUCN", "App", "1", "2", "3", "4", "5",
                              "6", "7", "8", "9", "10", "11", "12","13", "Score")) +
  scale_y_discrete(labels = Aves_plt_order$Taxon) + 
  geom_text(data = filter(sp10_rankings, Class == "Aves"), aes("Score", ID, label = round(Ovr_score, 2)), fontface = "bold") +
  geom_text(data = filter(sp10_rankings, Class == "Aves"), aes("Year", ID, label = round(Year, 2))) +
  geom_text(data = filter(sp10_rankings, Class == "Aves"), aes("Exporter", ID, label = Exporter)) +
  geom_text(data = filter(sp10_rankings, Class == "Aves"), aes("Vol", ID, label = round(Vol, 2))) +
  geom_label(data = filter(sp10_rankings, Class == "Aves"), aes("IUCN", ID, label = IUCN_code, fill = IUCN_code)) +
  geom_text(data = filter(sp10_rankings, Class == "Aves"), aes("App", ID, label = Appendix)) +
  scale_fill_manual(values = c("red", "darkorange", "dodgerblue", "skyblue", "goldenrod1")) +
  annotate("text", label = "Volume trends", x = 7, y = 11, fontface = "bold", size = 3.5) +
  annotate("text", label = "Code\n switching", x = 10, y = 11, fontface = "bold", size = 3.5) +
  annotate("text", label = "Legal acquisition", x = 13, y = 11, fontface = "bold", size = 3.5) +
  annotate("text", label = "Reporting\n inconsistencies", x = 16.5, y = 11, fontface = "bold", size = 3.5) +
  coord_cartesian(ylim = c(0.5, 11.5), expand = FALSE, xlim = c(0.5, 19.5)) +
  xlab("") + ylab("") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none",
        axis.text.x = element_text(face=c("bold","bold","bold", "bold","bold", "plain",
                                          "plain", "plain", "plain", "plain", "plain",
                                          "plain", "plain", "plain", "plain", "plain",
                                          "plain", "plain", "bold"), colour = "black"),
        axis.text.y = element_text(face = "bold.italic", colour = "black", size = 12))

## 2020
sp10_rankings_2020 <- AvesRept_checked_WOE %>% 
  select(ROW_ID, Year, Taxon, Class, Order, Exporter, Vol, IUCN_code, ROW_ID, Appendix, 69:111) %>%
  filter(Year == 2020) %>%
  unite("ID", c(Year, Taxon, Exporter), remove = FALSE) %>% group_by(Class) %>%
  slice_max(Ovr_score, n = 10) %>%
  mutate(IUCN_code = ifelse(IUCN_code == "Not assessed", "NE", IUCN_code))

sp10_checks_2020 <- AvesRept_checked_WOE  %>% filter(Year == 2020) %>% group_by(Class) %>%
  slice_max(Ovr_score, n = 10) %>%
  select(Year, Taxon, Exporter, Check_1, Check_2, Check_3, Check_4, 
         Check_5, Check_6, Check_7, Check_8,
         Check_9, Check_10, Check_11, Check_12, Check_13) %>%
  unite("ID", c(Year, Taxon, Exporter), remove = TRUE) %>%
  pivot_longer(!c(ID, Class), names_to = "Check", values_to = "Fail") %>%
  filter(Fail == TRUE) %>%
  mutate(Check = factor(Check, levels = c("Check_1", "Check_2", "Check_3", "Check_4", "Check_5",
                                          "Check_6", "Check_7", "Check_8", "Check_9", "Check_10",
                                          "Check_11", "Check_12", "Check_13"))) %>%
  left_join(select(sp10_rankings_2020, ID, Ovr_score))

Aves_plt_order_2020 <- sp10_rankings_2020 %>% filter(Class == "Aves") %>% arrange(Ovr_score)


sp10_plt_2020_aves <- ggplot(filter(sp10_checks_2020, Class == "Aves"), aes(Check, reorder(ID, Ovr_score))) + 
  geom_point(colour = NA) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2020_Lonchura oryzivora_CU", Ovr_score = 1), xmin = 5.5, xmax = 8.5, 
            ymin = -Inf, ymax = 10.5, fill = "lightskyblue1", alpha = .4) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2020_Lonchura oryzivora_CU", Ovr_score = 1),xmin = 8.5, xmax = 11.5, 
            ymin = -Inf, ymax = 10.5, fill = "darkseagreen2", alpha = .4) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2020_Lonchura oryzivora_CU", Ovr_score = 1),xmin = 11.5, xmax = 14.5, 
            ymin = -Inf, ymax = 10.5, fill = "rosybrown1", alpha = .4) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2020_Lonchura oryzivora_CU", Ovr_score = 1),xmin = 14.5, xmax = 18.5, 
            ymin = -Inf, ymax = 10.5, fill = "lightgoldenrod", alpha = .4) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2020_Lonchura oryzivora_CU", Ovr_score = 1),xmin = 18.5, xmax = 19.5, 
            ymin = -Inf, ymax = 10.5, fill = "grey85", alpha = .4) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2020_Lonchura oryzivora_CU", Ovr_score = 1), xmin = -Inf, xmax = Inf, 
            ymin = 10.5, ymax = 13, fill = "white") +
  geom_point(shape = 4, colour = "darkred", size = 4) +
  scale_x_discrete(drop=FALSE, limits = c("Vol", "Exporter","Year",  "IUCN", "App", "Check_1", "Check_2", "Check_3", "Check_4", "Check_5",
                                          "Check_6", "Check_7", "Check_8", "Check_9", "Check_10",
                                          "Check_11", "Check_12", "Check_13", "Score"),
                   labels = c("Vol",  "Exporter", "Year","IUCN", "App", "1", "2", "3", "4", "5",
                              "6", "7", "8", "9", "10", "11", "12", "13", "Score")) +
  scale_y_discrete(labels = Aves_plt_order_2020$Taxon) + 
  geom_text(data = filter(sp10_rankings_2020, Class == "Aves"), aes("Score", ID, label = round(Ovr_score, 2)), fontface = "bold") +
  geom_text(data = filter(sp10_rankings_2020, Class == "Aves"), aes("Year", ID, label = round(Year, 2))) +
  geom_text(data = filter(sp10_rankings_2020, Class == "Aves"), aes("Exporter", ID, label = Exporter)) +
  geom_text(data = filter(sp10_rankings_2020, Class == "Aves"), aes("Vol", ID, label = round(Vol, 2))) +
  geom_label(data = filter(sp10_rankings_2020, Class == "Aves"), aes("IUCN", ID, label = IUCN_code, fill = IUCN_code)) +
  geom_text(data = filter(sp10_rankings_2020, Class == "Aves"), aes("App", ID, label = Appendix)) +
  scale_fill_manual(values = c("red", "darkorange", "dodgerblue", "skyblue")) +
  annotate("text", label = "Volume trends", x = 7, y = 11, fontface = "bold", size = 3.5) +
  annotate("text", label = "Code\n switching", x = 10, y = 11, fontface = "bold", size = 3.5) +
  annotate("text", label = "Legal acquisition", x = 13, y = 11, fontface = "bold", size = 3.5) +
  annotate("text", label = "Reporting\n inconsistencies", x = 16.5, y = 11, fontface = "bold", size = 3.5) +
  coord_cartesian(ylim = c(0.5, 11.5), expand = FALSE, xlim = c(0.5, 19.5)) +
  xlab("") + ylab("") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none",
        axis.text.x = element_text(face=c("bold","bold","bold", "bold","bold", "plain",
                                          "plain", "plain", "plain", "plain", "plain",
                                          "plain", "plain", "plain", "plain", "plain",
                                          "plain", "plain", "bold"), colour = "black"),
        axis.text.y = element_text(face = "bold.italic", colour = "black", size = 12))

top10_plt <- ggarrange(sp10_plt_aves, sp10_plt_2020_aves, nrow = 2, labels = c("A. 2000 - 2020", "B. 2020     "))

#A_ora <- grid::rasterGrob(jpeg::readJPEG("Data/Images/A_Oratrix.jpg"), interpolate = TRUE)
#C_nov <- grid::rasterGrob(jpeg::readJPEG("Data/Images/C_novaezelandiae.jpg"), interpolate = TRUE)
#P_eri <- grid::rasterGrob(jpeg::readJPEG("Data/Images/P_erithacus.jpg"), interpolate = TRUE)
#A_sol <- grid::rasterGrob(jpeg::readJPEG("Data/Images/A_solstatis.jpg"), interpolate = TRUE)
C_malh <- grid::rasterGrob(jpeg::readJPEG("Data/Images/C_malh2.jpg"), interpolate = TRUE)
A_fisc <- grid::rasterGrob(jpeg::readJPEG("Data/Images/A_fisc2.jpg"), interpolate = TRUE)
L_oryz <- grid::rasterGrob(jpeg::readJPEG("Data/Images/L_oryz2.jpg"), interpolate = TRUE)
A_glau <- grid::rasterGrob(jpeg::readJPEG("Data/Images/A_glau2.jpg"), interpolate = TRUE)

images <- ggarrange(C_malh, A_fisc, L_oryz, A_glau, nrow = 1, labels = c("C.", "D.", "E.", "F."))

top10_plt_final <- ggarrange(top10_plt, images, nrow = 2, heights = c(1, .3))

ggsave("Summaries/Figures/top10_plt_aves.png", top10_plt_final, device = "png", 
       width = 10, height = 10, bg = "white")


#### species prioritisation - Reptiles ####

Rept_plt_order <- sp10_rankings %>% filter(Class == "Reptilia") %>% arrange(Ovr_score)

sp10_plt_rept <- ggplot(filter(sp10_checks, Class == "Reptilia"), aes(Check, reorder(ID, Ovr_score))) +
  geom_point(colour = NA) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2014_Chelonoidis carbonarius_SV", Ovr_score = 1), xmin = 5.5, xmax = 8.5, 
            ymin = -Inf, ymax = 10.5, fill = "lightskyblue1", alpha = .4) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2014_Chelonoidis carbonarius_SV", Ovr_score = 1),xmin = 8.5, xmax = 11.5, 
            ymin = -Inf, ymax = 10.5, fill = "darkseagreen2", alpha = .4) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2014_Chelonoidis carbonarius_SV", Ovr_score = 1),xmin = 11.5, xmax = 14.5, 
            ymin = -Inf, ymax = 10.5, fill = "rosybrown1", alpha = .4) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2014_Chelonoidis carbonarius_SV", Ovr_score = 1),xmin = 14.5, xmax = 18.5, 
            ymin = -Inf, ymax = 10.5, fill = "lightgoldenrod", alpha = .4) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2014_Chelonoidis carbonarius_SV", Ovr_score = 1),xmin = 18.5, xmax = 19.5, 
            ymin = -Inf, ymax = 10.5, fill = "grey85", alpha = .4) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2014_Chelonoidis carbonarius_SV", Ovr_score = 1), xmin = -Inf, xmax = Inf, 
            ymin = 10.5, ymax = 13, fill = "white") +
  geom_point(shape = 4, colour = "darkred", size = 4) +
  scale_x_discrete(drop=FALSE, limits = c("Vol", "Exporter","Year",  "IUCN", "App", "Check_1", "Check_2", "Check_3", "Check_4", "Check_5",
                                          "Check_6", "Check_7", "Check_8", "Check_9", "Check_10",
                                          "Check_11", "Check_12", "Check_13", "Score"),
                   labels = c("Vol", "Exporter", "Year",  "IUCN", "App", "1", "2", "3", "4", "5",
                              "6", "7", "8", "9", "10", "11", "12", "13", "Score")) +
  scale_y_discrete(labels = Rept_plt_order$Taxon) + 
  geom_text(data = filter(sp10_rankings, Class == "Reptilia"), aes("Score", ID, label = round(Ovr_score, 2)), fontface = "bold") +
  geom_text(data = filter(sp10_rankings, Class == "Reptilia"), aes("Year", ID, label = round(Year, 2))) +
  geom_text(data = filter(sp10_rankings, Class == "Reptilia"), aes("Exporter", ID, label = Exporter)) +
  geom_text(data = filter(sp10_rankings, Class == "Reptilia"), aes("Vol", ID, label = round(Vol, 2))) +
  geom_label(data = filter(sp10_rankings, Class == "Reptilia"), aes("IUCN", ID, label = IUCN_code, fill = IUCN_code)) +
  geom_text(data = filter(sp10_rankings, Class == "Reptilia"), aes("App", ID, label = Appendix)) +
  scale_fill_manual(values = c("red", "dodgerblue", "grey70", "goldenrod1")) +
  annotate("text", label = "Volume trends", x = 7, y = 11, fontface = "bold", size = 3.5) +
  annotate("text", label = "Code\n switching", x = 10, y = 11, fontface = "bold", size = 3.5) +
  annotate("text", label = "Legal acquisition", x = 13, y = 11, fontface = "bold", size = 3.5) +
  annotate("text", label = "Reporting\n inconsistencies", x = 16.5, y = 11, fontface = "bold", size = 3.5) +
  coord_cartesian(ylim = c(0.5, 11.5), expand = FALSE, xlim = c(0.5, 19.5)) +
  xlab("") + ylab("") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none",
        axis.text.x = element_text(face=c("bold","bold","bold", "bold","bold", "plain",
                                          "plain", "plain", "plain", "plain", "plain",
                                          "plain", "plain", "plain", "plain", "plain",
                                          "plain", "plain", "bold"), colour = "black"),
        axis.text.y = element_text(face = "bold.italic", colour = "black", size = 12))


Rept_plt_order_2020 <- sp10_rankings_2020 %>% filter(Class == "Reptilia") %>% arrange(Ovr_score)

sp10_plt_2020_rept <- ggplot(filter(sp10_checks_2020, Class == "Reptilia"), aes(Check, reorder(ID, Ovr_score))) + 
  geom_point(colour = NA) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2020_Chelonoidis carbonarius_SV", Ovr_score = 1), xmin = 5.5, xmax = 8.5, 
            ymin = -Inf, ymax = 10.5, fill = "lightskyblue1", alpha = .4) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2020_Chelonoidis carbonarius_SV", Ovr_score = 1),xmin = 8.5, xmax = 11.5, 
            ymin = -Inf, ymax = 10.5, fill = "darkseagreen2", alpha = .4) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2020_Chelonoidis carbonarius_SV", Ovr_score = 1),xmin = 11.5, xmax = 14.5, 
            ymin = -Inf, ymax = 10.5, fill = "rosybrown1", alpha = .4) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2020_Chelonoidis carbonarius_SV", Ovr_score = 1),xmin = 14.5, xmax = 18.5, 
            ymin = -Inf, ymax = 10.5, fill = "lightgoldenrod", alpha = .4) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2020_Chelonoidis carbonarius_SV", Ovr_score = 1),xmin = 18.5, xmax = 19.5, 
            ymin = -Inf, ymax = 10.5, fill = "grey85", alpha = .4) +
  geom_rect(data = data.frame(Check = "Check_1", ID = "2020_Chelonoidis carbonarius_SV", Ovr_score = 1), xmin = -Inf, xmax = Inf, 
            ymin = 10.5, ymax = 13, fill = "white") +
  geom_point(shape = 4, colour = "darkred", size = 4) +
  scale_x_discrete(drop=FALSE, limits = c("Vol", "Exporter","Year",  "IUCN", "App", "Check_1", "Check_2", "Check_3", "Check_4", "Check_5",
                                          "Check_6", "Check_7", "Check_8", "Check_9", "Check_10",
                                          "Check_11", "Check_12", "Check_13", "Score"),
                   labels = c("Vol",  "Exporter", "Year","IUCN", "App", "1", "2", "3", "4", "5",
                              "6", "7", "8", "9", "10", "11", "12", "13", "Score")) +
  scale_y_discrete(labels = Rept_plt_order_2020$Taxon) + 
  geom_text(data = filter(sp10_rankings_2020, Class == "Reptilia"), aes("Score", ID, label = round(Ovr_score, 2)), fontface = "bold") +
  geom_text(data = filter(sp10_rankings_2020, Class == "Reptilia"), aes("Year", ID, label = round(Year, 2))) +
  geom_text(data = filter(sp10_rankings_2020, Class == "Reptilia"), aes("Exporter", ID, label = Exporter)) +
  geom_text(data = filter(sp10_rankings_2020, Class == "Reptilia"), aes("Vol", ID, label = round(Vol, 2))) +
  geom_label(data = filter(sp10_rankings_2020, Class == "Reptilia"), aes("IUCN", ID, label = IUCN_code, fill = IUCN_code)) +
  geom_text(data = filter(sp10_rankings_2020, Class == "Reptilia"), aes("App", ID, label = Appendix)) +
  scale_fill_manual(values = c("red", "dodgerblue", "grey70", "goldenrod1")) +
  annotate("text", label = "Volume trends", x = 7, y = 11, fontface = "bold", size = 3.5) +
  annotate("text", label = "Code\n switching", x = 10, y = 11, fontface = "bold", size = 3.5) +
  annotate("text", label = "Legal acquisition", x = 13, y = 11, fontface = "bold", size = 3.5) +
  annotate("text", label = "Reporting\n inconsistencies", x = 16.5, y = 11, fontface = "bold", size = 3.5) +
  coord_cartesian(ylim = c(0.5, 11.5), expand = FALSE, xlim = c(0.5, 19.5)) +
  xlab("") + ylab("") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none",
        axis.text.x = element_text(face=c("bold","bold","bold", "bold","bold", "plain",
                                          "plain", "plain", "plain", "plain", "plain",
                                          "plain", "plain", "plain", "plain", "plain",
                                          "plain", "plain", "bold"), colour = "black"),
        axis.text.y = element_text(face = "bold.italic", colour = "black", size = 12))

C_sia <- grid::rasterGrob(jpeg::readJPEG("Data/Images/C_siamensis.jpg"), interpolate = TRUE)
G_pseu <- grid::rasterGrob(jpeg::readJPEG("Data/Images/G_pseu2.jpg"), interpolate = TRUE)
T_kle <- grid::rasterGrob(jpeg::readJPEG("Data/Images/T_kleinmanni.jpg"), interpolate = TRUE)
#M_tor <- grid::rasterGrob(jpeg::readJPEG("Data/Images/M_torneri.jpg"), interpolate = TRUE)
A_rad <- grid::rasterGrob(jpeg::readJPEG("Data/Images/A_radiata.jpg"), interpolate = TRUE)

top10_plt2 <- ggarrange(sp10_plt_rept, sp10_plt_2020_rept, nrow = 2, labels = c("A. 2000 - 2020", "B. 2020     "))
images2 <- ggarrange(C_sia, G_pseu, T_kle, A_rad, nrow = 1, labels = c("C.", "D.", "E.", "F."))

top10_plt_final2 <- ggarrange(top10_plt2, images2, nrow = 2, heights = c(1, .3))

ggsave("Summaries/Figures/top10_plt_rept.png", top10_plt_final2, device = "png", 
       width = 10, height = 10, bg = "white")

#### Species specific ####


dat <- ID_dataseries(data = Aves_checked_WOE, ID_list = sp10_rankings$ROW_ID) %>%
  unite("ID", c(ROW_ID, Taxon, Exporter), remove = FALSE)
  
dat20 <- ID_dataseries(data = Aves_checked_WOE, ID_list = sp10_rankings_2020$ROW_ID) %>%
  unite("ID", c(ROW_ID, Taxon, Exporter), remove = FALSE)

ggplot(dat20, aes(Year, Vol, colour = Type)) + 
  geom_line() + geom_point() +
  facet_wrap(~ID, scales = "free")

ggplot(dat, aes(Year, Vol, colour = Type)) + 
  geom_line() + geom_point() +
  facet_wrap(~ID, scales = "free")

ggplot(Aves_checked_WOE, aes(Year, Ovr_score)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~region)



#### Mean species ####
mean_species_sum <- AvesRept_checked_WOE %>% 
  group_by(Class, Taxon) %>% 
  summarise(mean = round(mean(Ovr_score), digits = 2), 
            min = round(min(Ovr_score), digits = 2),
            max = round(max(Ovr_score), digits = 2),
            vol = sum(Vol),
            Exporters = n_distinct(Exporter),
            Year = n_distinct(Year), events = n()) %>% slice_max(mean, n=10) %>%
  unite("Range", min:max, sep = " - ")

write.csv(mean_species_sum, "Summaries/Figures/Mean_species.csv")

f <- Aves_checked_WOE %>% filter(Taxon == "Leucopsar rothschildi") %>% group_by(Year, Exporter) %>%
  tally(Vol)
f <- Aves_checked_WOE %>% filter(Taxon == "Leucopsar rothschildi") %>% 
  select(Exporter, Year, Vol, Ovr_score)
  tally(Vol)
f <- AvesRept_checked_WOE %>% filter(Taxon == "Agapornis personatus", Exporter == "CN", Year == "2000") %>%   
  select(Year, Taxon, Exporter, Source_traded, Year_F1,  Check_1, Check_2, Check_3, Check_4, 
         Check_5, Check_6, Check_7, Check_8,
         Check_9, Check_10, Check_11, Check_12, Ovr_score)

Full %>% filter(Taxon == "Pelodiscus sinensis", Exporter == "TH", Year == "2005")

#### Extinct species ####

Ex_species <- AvesRept_checked_WOE %>% filter(IUCN_code %in% c("EW", "EX")) %>%
  select(Taxon, Year, Exporter, IUCN_code, Appendix, Vol, Ovr_score)
write.csv(Ex_species, "Summaries/Figures/Extinct_species.csv")


AvesRept_checked_WOE %>% filter(Taxon == "Rhodonessa caryophyllacea") %>%
  select(Taxon, Year, Exporter, IUCN_code, Appendix, Vol, Ovr_score)

CITES_MASTER %>% filter(Taxon == "Anas oustaleti")

#### Mean exporters ####

mean_Exporter_sum <- AvesRept_checked_WOE %>%
  group_by(Class, region, Exporter) %>% 
  summarise(mean = round(mean(Ovr_score), digits = 2), min = round(min(Ovr_score), digits = 2),
            max = round(max(Ovr_score), digits = 2),
            vol = sum(Vol),
            Species = n_distinct(Taxon),
            Year = n_distinct(Year), events = n()) %>% 
  group_by(Class) %>%
  slice_max(mean, n=10)

write.csv(mean_Exporter_sum, "Summaries/Figures/Mean_exporter.csv") 

## Aves
Aves_order1 <- AvesRept_checked_WOE %>%
  filter( Class == "Aves",
         region %in% c("Africa", "Americas")) %>%
  group_by(region, Exporter) %>% reframe(mean = median(Ovr_score)) %>%
  arrange(region, mean)

Aves_order2 <- AvesRept_checked_WOE %>%
  filter( Class == "Aves",
         region %in% c("Asia", "Europe", "Oceania")) %>%
  group_by(region, Exporter) %>% reframe(mean = median(Ovr_score)) %>%
  arrange(region, mean)

Aves_exp_plt1 <- 
  ggplot(AvesRept_checked_WOE %>%
         filter(Class == "Aves",
                region %in% c("Africa", "Americas")), 
       aes(Ovr_score, Exporter, fill = region)) +
  scale_fill_manual(values = c("tomato", "dodgerblue")) +
  geom_boxplot(outlier.shape = NA, alpha = .5) +
  scale_y_discrete(limits = Aves_order1$Exporter) +
  xlab("Overall priority score") +
  theme_minimal() +
  theme(legend.position = "none")

Aves_exp_plt2 <- 
  ggplot(AvesRept_checked_WOE %>%
           filter( Class == "Aves",
                  region %in% c("Asia", "Europe", "Oceania")), 
         aes(Ovr_score, Exporter, fill = region)) +
  scale_fill_manual(values = c("chartreuse4", "goldenrod4", "purple4")) +
  geom_boxplot(outlier.shape = NA, alpha = .5) +
  scale_y_discrete(limits = Aves_order2$Exporter) +
  xlab("Overall priority score") +
  theme_minimal() +
  theme(legend.position = "none")

Aves_reg_plt <- 
  ggplot(AvesRept_checked_WOE %>%
           filter( Class == "Aves"), 
         aes(region, Ovr_score, fill = region)) +
  geom_point(alpha = .05, position = position_jitter()) +
  geom_boxplot(outlier.shape = NA, alpha = .5) +
  scale_fill_manual(values = c("tomato", "dodgerblue", "chartreuse4",
                               "goldenrod4", "purple4")) +
  xlab("Region") +
  ylab("Overall priority score") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

Countryboxes_aves <- ggarrange(
  ggarrange(Aves_exp_plt1, Aves_exp_plt2, ncol = 2, labels = c("A.", "B.")),
  Aves_reg_plt, labels = c("", "C."), nrow = 2, heights = c(1, 0.5))

ggsave("Summaries/Figures/Country_regions_aves.png", Countryboxes_aves, device = "png", 
       width = 10, height = 13, bg = "white")

## Rept
Rept_order1 <- AvesRept_checked_WOE %>%
  filter( Class == "Reptilia",
         region %in% c("Africa", "Americas")) %>%
  group_by(region, Exporter) %>% reframe(mean = median(Ovr_score)) %>%
  arrange(region, mean)

Rept_order2 <- AvesRept_checked_WOE %>%
  filter( Class == "Reptilia",
         region %in% c("Asia", "Europe", "Oceania")) %>%
  group_by(region, Exporter) %>% reframe(mean = median(Ovr_score)) %>%
  arrange(region, mean)

Rept_exp_plt1 <- 
  ggplot(AvesRept_checked_WOE %>%
           filter( Class == "Reptilia",
                  region %in% c("Africa", "Americas")), 
         aes(Ovr_score, Exporter, fill = region)) +
  scale_fill_manual(values = c("tomato", "dodgerblue")) +
  geom_boxplot(outlier.shape = NA, alpha = .5) +
  scale_y_discrete(limits = Rept_order1$Exporter) +
  xlab("Overall priority score") +
  theme_minimal() +
  theme(legend.position = "none")

Rept_exp_plt2 <- 
  ggplot(AvesRept_checked_WOE %>%
           filter( Class == "Reptilia",
                  region %in% c("Asia", "Europe", "Oceania")), 
         aes(Ovr_score, Exporter, fill = region)) +
  scale_fill_manual(values = c("chartreuse4", "goldenrod4", "purple4")) +
  geom_boxplot(outlier.shape = NA, alpha = .5) +
  scale_y_discrete(limits = Rept_order2$Exporter) +
  xlab("Overall priority score") +
  theme_minimal() +
  theme(legend.position = "none")

Rept_reg_plt <- 
  ggplot(AvesRept_checked_WOE %>%
           filter(Class == "Reptilia"), 
         aes(region, Ovr_score, fill = region)) +
  geom_point(alpha = .05, position = position_jitter()) +
  geom_boxplot(outlier.shape = NA, alpha = .5) +
  scale_fill_manual(values = c("tomato", "dodgerblue", "chartreuse4",
                               "goldenrod4", "purple4")) +
  xlab("Region") +
  ylab("Overall priority score") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

Countryboxes_rept <- ggarrange(
  ggarrange(Rept_exp_plt1, Rept_exp_plt2, ncol = 2, labels = c("A.", "B.")),
  Rept_reg_plt, labels = c("", "C."), nrow = 2, heights = c(1, 0.5))

ggsave("Summaries/Figures/Country_regions_rept.png", Countryboxes_rept, device = "png", 
       width = 10, height = 13, bg = "white")

#### Year region temporal trends ####

Year_plt <- ggplot(AvesRept_checked_WOE, aes(as.factor(Year), Ovr_score)) +
  geom_point(alpha = .1, colour = "grey25", shape = 16) +
  geom_boxplot(outlier.shape = NA) +
  facet_grid(region~Class) +
  scale_x_discrete(labels = c("2000", "", "" ,"", "", "", "", "" ,"", "", 
                              "2010", "", "" ,"", "", "", "", "" ,"", "", 
                              "2020")) +
  xlab("Year") +
  ylab("Overall priority score") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))

Year_plt2 <- egg::tag_facet(Year_plt, open = "", close = ".", tag_pool = LETTERS) +
  theme(strip.text = element_text(face = "bold"))

ggsave("Summaries/Figures/Year_regions.png", Year_plt2, device = "png", 
       width = 10, height = 7, bg = "white")
