# .libPaths("C:/Packages") ## Set up for working from home.

library(tidyverse)
library(ggridges)
library(viridis)
library(ggpubr)
library(rnaturalearth)
library(ggrepel)
library(cowplot)
library(grid)
library(png)

source("2_Assessment_Pipeline/Functions.R")

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
  ## cumulative tally of exporter
  group_by(Taxon) %>%
  mutate(Exp_tally = cumsum(!duplicated(Exporter))) %>%
  group_by(Taxon, Year) %>%
  mutate(Exp_tally = max(Exp_tally)) %>%
  ## total exporters
  group_by(Taxon) %>%
  mutate(Exp_total = n_distinct(Exporter)) %>%
  ## First exported
  group_by(Exporter, Taxon) %>%
  mutate(First_exp = min(Year)) %>%
  ungroup() %>%
  mutate(Volume_trends_grp = case_when((Check_1 + Check_2 + Check_3) == 0 ~ 0,
                                       (Check_1 + Check_2 + Check_3) == 1 ~ 1,
                                       (Check_1 + Check_2 + Check_3) == 2 ~ 1.5,
                                       (Check_1 + Check_2 + Check_3) == 3 ~ 1.5),
         Code_switch_grp = case_when((Check_4 + Check_5 + Check_6) == 0 ~ 0,
                                       (Check_4 + Check_5 + Check_6) == 1 ~ 1,
                                       (Check_4 + Check_5 + Check_6) == 2 ~ 1.5,
                                       (Check_4 + Check_5 + Check_6) == 3 ~ 1.5),
         Legal_acq_grp = case_when((Check_7 + Check_8 + Check_9) == 0 ~ 0,
                                   (Check_7 + Check_8 + Check_9) == 1 ~ 0,
                                   (Check_7 + Check_8 + Check_9) == 2 ~ 0,
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
                                      ((Check_10 + Check_11 + Check_12 + Check_13) == 4 & Appendix != "I") ~ 0),
         Additional_penalty = Check_4 + Check_5 + Check_10 + Check_11 + Check_12 + Check_13) %>%
  mutate(Fail_score = Volume_trends_grp + Code_switch_grp + Legal_acq_grp + Reporter_inc_grp,
         #Thr_score = case_when(IUCN_code == "LC" ~ 0,
          #                     IUCN_code == "NT" ~ 0.5, 
           #                    IUCN_code == "VU" ~ 1,
            #                   IUCN_code == "EN" ~ 1.5,
             #                  IUCN_code == "CR" ~ 2,
              #                 IUCN_code == "EW" ~ 2,
               #                IUCN_code == "EX" ~ 2,
                #               IUCN_code == "Not assessed" ~ 2),
         Thr_score1 = case_when(IUCN_code == "LC" ~ 1,
                                IUCN_code == "NT" ~ 10, 
                                IUCN_code == "VU" ~ 10,
                                IUCN_code == "EN" ~ 10,
                                IUCN_code == "CR" ~ 10,
                                IUCN_code == "EW" ~ 10,
                                IUCN_code == "EX" ~ 10,
                                IUCN_code == "Not assessed" ~ 10,
                                IUCN_code == "DD" ~ 10),
         Thr_score2 = case_when(IUCN_code == "LC" ~ 0,
                               IUCN_code == "NT" ~ 0.25, 
                               IUCN_code == "VU" ~ 0.5,
                               IUCN_code == "EN" ~ 1,
                               IUCN_code == "CR" ~ 2,
                               IUCN_code == "EW" ~ 2,
                               IUCN_code == "EX" ~ 2,
                               IUCN_code == "Not assessed" ~ 0.5,
                               IUCN_code == "DD" ~ 0.5),
         Thr_score3 = case_when(IUCN_code == "LC" ~ 2,
                                IUCN_code == "NT" ~ 4, 
                                IUCN_code == "VU" ~ 8,
                                IUCN_code == "EN" ~ 16,
                                IUCN_code == "CR" ~ 32,
                                IUCN_code == "EW" ~ 32,
                                IUCN_code == "EX" ~ 32,
                                IUCN_code == "Not assessed" ~ 2,
                                IUCN_code == "DD" ~ 2),
         #Fail_score = ifelse(Exp_total > 5, 0, Fail_score),
         #Ovr_score = (log10(Vol) + Thr_score1) * (Fail_score + Additional_penalty),
         Ovr_score1 = ((Vol*Thr_score1)/1)*(Check_4 + Check_5 +
                                                      Check_10 + Check_11 + Check_12 + Check_13),
         Ovr_score2 = ((log10(Vol) + Thr_score2)/1)*(Check_4 + Check_5 +
                                                      Check_10 + Check_11 + Check_12 + Check_13),
         Ovr_score3 = ((Vol*Thr_score3)/1)*(Check_4 + Check_5 +
                                                      Check_10 + Check_11 + Check_12 + Check_13),
         Ovr_base1 = ((1000*10)/1)*2,
         Ovr_base2 = ((log10(1000) + 2)/1)*2,
         Ovr_base3 = ((1000*32)/1)*2,
         st_score1 = Ovr_score1/Ovr_base1, 
         st_score2 = Ovr_score2/Ovr_base1, 
         st_score3 = Ovr_score3/Ovr_base1, 
         Launder_SwWC = ifelse(Check_4 == TRUE & Check_7 == FALSE, 1, 0),
         Launder_SwRC = ifelse(Check_5 == TRUE & Check_7 == FALSE, 1, 0),
         Launder_ReWC = ifelse(Check_10 == TRUE & Check_7 == FALSE, 1, 0),
         Launder_ReRC = ifelse(Check_11 == TRUE & Check_7 == FALSE, 1, 0),
         Launder_ReCom = ifelse(Check_12 == TRUE & Check_7 == FALSE, 1, 0),
         Providence_SwWC = ifelse(Check_4 == TRUE & Legal_acq_grp == 1.5, 1, 0),
         Providence_SwRC = ifelse(Check_5 == TRUE & Legal_acq_grp == 1.5, 1, 0),
         Providence_ReWC = ifelse(Check_10 == TRUE & Legal_acq_grp == 1.5, 1, 0),
         Providence_ReRC = ifelse(Check_11 == TRUE & Legal_acq_grp == 1.5, 1, 0),
         Providence_ReCom = ifelse(Check_12 == TRUE & Legal_acq_grp == 1.5, 1, 0),
         Novel_exp = ifelse(Legal_acq_grp == 1.5 & Exp_tally == 1 &
                              Exp_total == 1, 1, 0),
         Rare_exp1 = ifelse(Legal_acq_grp == 1.5 & Exp_total < 3 & Check_1 == TRUE, 1, 0),
         Rare_exp2 = ifelse(Legal_acq_grp == 1.5 & Exp_total < 3 & Check_2 == TRUE, 1, 0),
         
         Row = "A")

sum(AvesRept_checked_WOE$Vol) # 53674762
sum(filter(AvesRept_checked_WOE, Class == "Aves")$Vol) # 7429562
sum(filter(AvesRept_checked_WOE, Class == "Reptilia")$Vol) # 46245200

world <- ne_countries(scale = "medium", returnclass = "sf", type = "countries") 
SOM_map <- left_join(world, AvesRept_checked_WOE %>% group_by(Exporter) %>% tally(), 
                     by = c("iso_a2" = "Exporter")) %>% filter(!is.na(n)) %>% as.data.frame() %>%
  select(name, iso_a2)
write.csv(AvesRept_checked_WOE, "Outputs/Full_Check/BirdsRept/AvesRept_checked_WOE_score.csv", na = "")
write.csv(SOM_map, "Outputs/Full_Check/BirdsRept/SOM_Country_codes.csv", na = "")


AvesRept_checked_WOE %>% filter(Launder_SwWC == 1)
AvesRept_checked_WOE %>% filter(Launder_SwRC == 1)
AvesRept_checked_WOE %>% filter(Launder_ReCom == 1)
AvesRept_checked_WOE %>% filter(Launder_ReWC == 1)
AvesRept_checked_WOE %>% filter(Launder_ReRC == 1)
AvesRept_checked_WOE %>% filter(Rare_exp1 == 1)
AvesRept_checked_WOE %>% filter(Rare_exp2 == 1)
AvesRept_checked_WOE %>% filter(Check_13 == TRUE)

Imp_greater_than_Exp <- AvesRept_checked_WOE %>% 
  filter(Vol*10 < Capt_Vol_Contrast + Ranch_Vol_Contrast, Vol > 10) %>% select(1:16, Appendix)
Imp_greater_than_Exp %>% group_by(Class) %>% summarise(n_distinct(Taxon))

#### Misuse of code D ####

## 531 + 142 instances of code D
inci <- AvesRept_checked_WOE %>% filter(grepl("D", Source_traded)) %>%
  group_by(Check_13) %>% reframe(inci = n(), vol = sum(Code_D_vol)) %>%
  ungroup() %>% mutate(tot_inc = sum(inci), tot_vol = sum(vol),
                       prop_inc = inci/tot_inc *100,
                       prop_vol = vol/tot_vol *100)

## 142 possible misuses
Code_d <- AvesRept_checked_WOE %>% filter(Check_13 == TRUE) %>% select(1:20, Family, Code_D_vol, Appendix, IUCN_code)
Code_d %>% group_by(IUCN_code) %>% summarise(n = n(), vol = sum(Code_D_vol),
                                             sp = n_distinct(Taxon), Exp = n_distinct(Exporter))

Code_d_exp <- Code_d %>% group_by(Exporter) %>% tally()
Code_d_time <- Code_d %>% group_by(Year) %>% reframe(Vol = sum(Code_D_vol), Incidents = n())
Code_d_fam <- Code_d %>% group_by(Class, Family) %>% reframe(Vol = sum(Code_D_vol), Incidents = n()) %>% 
  arrange(Class, Family) %>% mutate(ord = seq(1:n()))

D_yr_plt <- ggplot(Code_d_time, aes(x = Year)) +
  geom_col(aes(y = Incidents), colour = "black", fill = "white", linewidth = .7) +
  geom_line(aes(y = Vol/500), colour = "darkorange", linewidth = .7) +
  geom_point(aes(y = Vol/500), colour = "darkorange", size = 2) +
  scale_y_continuous(name = "Incidents", 
                     sec.axis = sec_axis(~.*500, name="Volume (WOEs)")) +
  theme_minimal(base_size = 12) +
  theme(axis.title.y.right = element_text(colour = "darkorange"))

D_fam_plt <- ggplot(Code_d_fam, aes(x = reorder(Family, ord))) +
  geom_col(aes(y = Incidents), colour = "black", fill = "white", linewidth = .7) +
  geom_line(aes(y = Vol/500), colour = "darkorange", linewidth = .7) +
  geom_point(aes(y = Vol/500), colour = "darkorange", size = 2) +
  geom_segment(aes(x = 1, xend = 8, y = 60, yend = 60)) +
  geom_segment(aes(x = 1, xend = 1, y = 60, yend = 57)) +
  geom_segment(aes(x = 8, xend = 8, y = 60, yend = 57)) +
  geom_segment(aes(x = 9, xend = 12, y = 60, yend = 60)) +
  geom_segment(aes(x = 9, xend = 9, y = 60, yend = 57)) +
  geom_segment(aes(x = 12, xend = 12, y = 60, yend = 57)) +
  annotate("text", x = 4.5, y = 65, label = "Aves", fontface = "bold") +
  annotate("text", x = 10.5, y = 65, label = "Reptlia", fontface = "bold") +
  scale_y_continuous(name = "Incidents", 
                     sec.axis = sec_axis(~.*500, name="Volume (WOEs)")) +
  xlab("Family") +
  theme_minimal(base_size = 12) +
  theme(axis.title.y.right = element_text(colour = "darkorange"),
        axis.text.x = element_text(angle = 45, hjust = 1))

world <- ne_countries(scale = "medium", returnclass = "sf", type = "countries") 
D_map <- left_join(world, Code_d_exp, by = c("iso_a2" = "Exporter"))


D_map_plt <- ggplot() + geom_sf(data = D_map, aes(fill = n), colour = "grey25") +
  scale_fill_gradientn(name = "Records", na.value="grey95",
                       colours = c( "palegoldenrod",  "orange","darkred")) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.5)) +
  theme_classic(base_size = 12) +
  theme(legend.position = "right")

## pies
pie_d <- inci %>% 
  mutate(csum = rev(cumsum(rev(inci))), 
         pos = inci/2 + lead(csum, 1),
         pos = if_else(is.na(pos), inci/2, pos),
         csumv = rev(cumsum(rev(vol))), 
         posv = vol/2 + lead(csumv, 1),
         posv = if_else(is.na(posv), vol/2, posv),
         Vol_c = format(round(as.numeric(vol), 0), nsmall=0, big.mark=","))


pie_rec_plt <- ggplot(pie_d, aes(x="", y=inci, fill=Check_13)) +
  geom_bar(stat="identity", width=1, color="grey50") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("white", "black")) +
  geom_text_repel(aes(y = pos, label = paste0(inci, " records")),
                  size = 3, nudge_x = 3, show.legend = FALSE) +
  theme_void() +
  theme(legend.position = "none")

pie_vol_plt <- ggplot(pie_d, aes(x="", y=vol, fill=Check_13)) +
  geom_bar(stat="identity", width=1, color="grey50") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("white", "darkred")) +
  geom_text_repel(aes(y = posv, label = paste0(Vol_c, " WOEs")),
                  size = 3, nudge_x = 3, show.legend = FALSE) +
  theme_void()+
  theme(legend.position = "none")

#Code_D <- ggarrange(
 # ggarrange(ggarrange(pie_rec_plt,pie_vol_plt, nrow = 2, labels = c("A.", "B.")), 
  #          D_map_plt, nrow = 1, widths = c(1, 4), labels = c("", "C.")),
  #ggarrange(D_yr_plt, D_fam_plt, align = "hv", nrow = 1, labels = c("D.", "E.")),nrow =2)

Code_D <- ggarrange(D_map_plt, labels = c("A.", ""),
  ggarrange(D_yr_plt, D_fam_plt, align = "hv", nrow = 1, labels = c("B.", "C.")),
  nrow =2, heights = c(1, .65))

ggsave("Summaries/Figures/Code_D.png", Code_D, device = "png", 
       width = 10, height = 7, bg = "white")

#### Laundering - mismatches ####

WRC_tally <- AvesRept_checked_WOE %>% filter(Check_10_11_12_equivalent_reporting == TRUE, Appendix == "I") %>%
  mutate(Launder = case_when(Launder_ReWC == 1 ~ "WC",
                             Launder_ReRC == 1 ~ "RC",
                             Launder_ReWC == 1 & Launder_ReRC == 1 ~ "ERROR",
                             Launder_ReWC == 0 & Launder_ReRC == 0  & Check_7 == TRUE ~ "Pass-not range",
                             Launder_ReWC == 0 & Launder_ReRC == 0  & Check_7 == FALSE ~ "Pass-range")) %>%
  group_by(Launder) %>% 
  summarise(Records = n(), Vol = sum(Vol))%>% 
  filter(Launder != "Pass-not range") %>%
  ungroup() %>% 
  mutate(tot_rec = sum(Records), tot_vol = sum(Vol),
         prop_rec = Records/tot_rec *100,
         prop_vol = Vol/tot_vol *100) %>%
  mutate(csum = rev(cumsum(rev(Records))), 
         pos = Records/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Records/2, pos),
         csumv = rev(cumsum(rev(Vol))), 
         posv = Vol/2 + lead(csumv, 1),
         posv = if_else(is.na(posv), Vol/2, posv),
         Launder = as.character(Launder),
         Vol_c = format(round(as.numeric(Vol), 0), nsmall=0, big.mark=","))


Com_tally <- AvesRept_checked_WOE %>% 
  filter(Check_10_11_12_equivalent_reporting == TRUE, Appendix == "I", Check_7 == FALSE) %>%
  group_by(Launder_ReCom) %>% summarise(Records = n(), Vol = sum(Vol))%>% 
  ungroup() %>% 
  mutate(tot_rec = sum(Records), tot_vol = sum(Vol),
         prop_rec = Records/tot_rec *100,
         prop_vol = Vol/tot_vol *100) %>% 
  mutate(csum = rev(cumsum(rev(Records))), 
         pos = Records/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Records/2, pos),
         csumv = rev(cumsum(rev(Vol))), 
         posv = Vol/2 + lead(csumv, 1),
         posv = if_else(is.na(posv), Vol/2, posv),
         Launder_ReCom = as.character(Launder_ReCom),
         Vol_c = format(round(as.numeric(Vol), 0), nsmall=0, big.mark=",")) %>%
  rename("Launder" = "Launder_ReCom")


LaunderWC <- AvesRept_checked_WOE %>% 
  filter(Launder_ReWC == 1, Vol > 10) %>%
  mutate(diffC = Vol - Capt_Vol_Contrast - Ranch_Vol_Contrast,
         diffW = (Year_0_vol_Wild - Wild_Vol_Contrast)*-1) %>%
  mutate(dir = ifelse(diffC > 0|diffW>0, "C reported as W", "W reported as C"),
         diffC = abs(diffC),
         diffW = abs(diffW)) %>%
  mutate(equal_diff = ifelse(diffC == diffW, diffC, NA)) %>%
  select(Year, Taxon, Class, Exporter, Vol, Capt_Vol_Contrast,  Ranch_Vol_Contrast, 
         Year_0_vol_Wild, Wild_Vol_Contrast, diffC, diffW, dir, IUCN_code, ROW_ID,
         equal_diff) %>%
  unite( "ID", Year:Exporter,remove = FALSE) %>% arrange(Class, diffW) %>%
  mutate(pos = seq(1:n()),
         sp = str_split_fixed(Taxon, " ", 2),
         gen = substr(Taxon, 1, 1),
         Taxon_short = paste(gen, ". ", sp[,2] )) 

LaunderRC <- AvesRept_checked_WOE %>% 
  filter(Launder_ReRC == 1, Vol > 10) %>%
  mutate(diffC = Year_0_vol_Capt - Capt_Vol_Contrast,
         diffR = (Year_0_vol_Ranch - Ranch_Vol_Contrast)*-1) %>%
  mutate(dir = ifelse(diffC >0|diffR>0, "C reported as R", "R reported as C"),
         diffC = abs(diffC),
         diffR = abs(diffR)) %>%
  mutate(equal_diff = ifelse(diffC == diffR, diffC, NA)) %>%
  select(Year, Taxon, Class, Exporter, Vol, Year_0_vol_Capt,  Capt_Vol_Contrast, 
         Year_0_vol_Ranch, Ranch_Vol_Contrast, diffC, diffR, dir, IUCN_code, ROW_ID,
         equal_diff) %>%
  unite( "ID", Year:Exporter,remove = FALSE) %>% arrange(Class, diffR) %>%
  mutate(pos = seq(1:n()),
         sp = str_split_fixed(Taxon, " ", 2),
         gen = substr(Taxon, 1, 1),
         Taxon_short = paste(gen, ". ", sp[,2] )) 

LaunderCom <- AvesRept_checked_WOE %>% 
  filter(Launder_ReCom == 1, Vol > 10) %>%
  mutate(diffC = Comm_vol - Comm_Vol_Contrast,
         diffNC = (NonComm_vol + NA_vol - NonComm_Vol_Contrast - NA_Vol_Contrast)*-1) %>%
  mutate(dir = ifelse(diffC >0|diffNC>0, "C reported as NC", "NC reported as C"),
         diffC = abs(diffC),
         diffNC = abs(diffNC)) %>%
  # filter(dir == "NC reported as C") %>%
  mutate(equal_diff = ifelse(diffC == diffNC, diffC, NA)) %>%
  select(Year, Taxon, Class, Family, Exporter, region, Vol, Comm_vol,  Comm_Vol_Contrast, 
         NonComm_vol, NA_vol, NonComm_Vol_Contrast, NA_Vol_Contrast, diffC, diffNC, dir, IUCN_code, ROW_ID,
         equal_diff) %>%
  unite( "ID", Year:Exporter,remove = FALSE) %>% arrange(Class, diffNC) %>%
  mutate(pos = seq(1:n()),
         sp = str_split_fixed(Taxon, " ", 2),
         gen = substr(Taxon, 1, 1),
         Taxon_short = paste(gen, ". ", sp[,2] ))

LaunderCom %>% group_by(IUCN_code) %>% summarise(n = n(), sp = n_distinct(Taxon))
LaunderRC %>% group_by(IUCN_code) %>% summarise(n = n(), sp = n_distinct(Taxon))
LaunderWC %>% group_by(IUCN_code) %>% summarise(n = n(), sp = n_distinct(Taxon))

dat <- data.frame(R = c("E", "E", "I", "I"), S = c("C", "W", "C", "W"), Vol = c(105,25, 40, 100))

Launder_concept_plt <- ggplot(dat, aes(R, Vol, fill = S)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_segment(aes(x = 3, xend = 3, y = 105, yend = 40),
               arrow = arrow(), colour = "black", size = 1) +
  geom_segment(aes(x = 3.5, xend = 3.5, y = 25, yend = 100),
               arrow = arrow(), colour = "grey", size = 1) +
  coord_cartesian(xlim = c(1, 3.5), ylim = c(0, 110)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_point(x = 3.5, y = 110, colour = "grey", size = 2) +
  geom_point(x = 3, y = 30, shape = 21, colour = "black", fill = "white", size = 2) +
  annotate(geom = "text", label = expression(Delta*"Captive"), x = 2.75, y = 75, angle = 90,
           colour = "black", fontface = "bold") +
  annotate(geom = "text", label = expression(Delta*"Wild"), x = 3.25, y = 65, angle = 90,
           colour = "grey", fontface = "bold") +
  ylab("Volume") +
  xlab("Reporter") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none")



LaunderWC_plt <- ggplot(LaunderWC, aes(x = reorder(ID, pos), y = diffC, group = ROW_ID, colour = dir)) +
  geom_segment(aes(xend = ID, yend = diffW), linewidth = .5, alpha = .75) +
  geom_point(size = 2, shape = 21, fill = "white") +
  geom_point(aes(y = diffW), size = 2)  +
  geom_point(aes(y = equal_diff), size = 2) +
  scale_colour_manual(values = c("darkorange", "black"), labels = c("E[CDFR] > I[CDFR]", "E[WUXR] > I[WUXR]")) +
  scale_y_log10(limits = c(1, 1300000)) +
  scale_x_discrete(labels = LaunderWC$Taxon_short) +
  geom_text(aes(x = ID, label = paste0(Exporter, ", ", Year)),
            size = 3, nudge_y =1.2, show.legend = FALSE, angle = 90) +
  geom_segment(aes(x = 1, xend = 4, y = 700000, yend = 700000), colour = "black") +
  geom_segment(aes(x = 1, xend = 1, y = 700000, yend = 600000), colour = "black") +
  geom_segment(aes(x = 4, xend = 4, y = 700000, yend = 600000), colour = "black") +
  geom_segment(aes(x = 5, xend = 6, y = 700000, yend = 700000), colour = "black") +
  geom_segment(aes(x = 5, xend = 5, y = 700000, yend = 600000), colour = "black") +
  geom_segment(aes(x = 6, xend = 6, y = 700000, yend = 600000), colour = "black") +
  annotate("text", x = 2.5, y = 1300000, label = "Aves", fontface = "bold") +
  annotate("text", x = 5.5, y = 1300000, label = "Reptlia", fontface = "bold") +
  ylab("Reporter difference (CDFR:WUX)") +
  xlab("Taxon") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),
        legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank())

LaunderRC_plt <- ggplot(LaunderRC, aes(x = reorder(ID, pos), y = diffC, group = ROW_ID, colour = dir)) +
  geom_segment(aes(xend = ID, yend = diffR), linewidth = .5, alpha = .75) +
  geom_point(size = 2, shape = 21, fill = "white") +
  geom_point(aes(y = diffR), size = 2)  +
  geom_point(aes(y = equal_diff), size = 2) +
  scale_colour_manual(values = c("darkorange", "black"),  labels = c("E[CDF] > I[CDF]", "E[R] > I[R]")) +
  scale_y_log10(limits = c(10, 250000)) +
  scale_x_discrete(labels = LaunderRC$Taxon_short) +
  geom_text(aes(x = ID, label = paste0(Exporter, ", ", Year)),
            size = 3, nudge_y =1, show.legend = FALSE, angle = 90) +
  geom_segment(aes(x = 1, xend = 2, y = 140000, yend = 140000), colour = "black") +
  geom_segment(aes(x = 1, xend = 1, y = 140000, yend = 120000), colour = "black") +
  geom_segment(aes(x = 2, xend = 2, y = 140000, yend = 120000), colour = "black") +
  geom_segment(aes(x = 3, xend = 4, y = 140000, yend = 140000), colour = "black") +
  geom_segment(aes(x = 4, xend = 4, y = 140000, yend = 120000), colour = "black") +
  geom_segment(aes(x = 3, xend = 3, y = 140000, yend = 120000), colour = "black") +
  annotate("text", x = 1.5, y = 250000, label = "Aves", fontface = "bold") +
  annotate("text", x = 3.5, y = 250000, label = "Reptlia", fontface = "bold") +
  ylab("Reporter difference (CDF:R)") +
  xlab("Taxon") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),
        legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank())

LaunderCom_plt <- ggplot(LaunderCom, aes(x = pos, y = diffC, group = ROW_ID, colour = dir)) +
  scale_x_continuous(breaks = c(25, 50, 75, 100, 125, 150, 175, 200, 225)) +
  geom_segment(aes(xend = pos, yend = diffNC), linewidth = .5, alpha = .75) +
  geom_point(size = 2, shape = 21, fill = "white") +
  geom_point(aes(y = diffNC), size = 2, alpha = .5)  +
  geom_point(aes(y = equal_diff), size = 2, alpha = .5) +
  scale_colour_manual(values = c("black", "darkorange"), 
                      labels = c("E[Commercial] > I[Commerical]", "E[Non-comm] > I[Non-comm]")) +
  scale_y_log10(limits = c(1, 200000), breaks = c(1, 100, 10000)) +
  coord_cartesian(xlim = c(-3, 150), expand = FALSE) +
  #geom_text(aes(x = ID, label = paste0(Exporter, ", ", Year)),size = 3, nudge_y =.5, show.legend = FALSE, angle = 90) +
  geom_segment(aes(x = 1, xend = 134, y = 80000, yend = 80000), colour = "black") +
  geom_segment(aes(x = 1, xend = 1, y = 80000, yend = 60000), colour = "black") +
  geom_segment(aes(x = 134, xend = 134, y = 80000, yend = 60000), colour = "black") +
  geom_segment(aes(x = 135, xend = 148, y = 80000, yend = 80000), colour = "black") +
  geom_segment(aes(x = 135, xend = 135, y = 80000, yend = 60000), colour = "black") +
  geom_segment(aes(x = 148, xend = 148, y = 80000, yend = 60000), colour = "black") +
  #geom_segment(aes(x = 1, xend = 6, y = 40000, yend = 40000), colour = "grey50", linewidth = 2) +
  #geom_segment(aes(x = 7, xend = 166, y = 40000, yend = 40000), colour = "grey50", linewidth = 2) +
  #geom_segment(aes(x = 167, xend = 193, y = 40000, yend = 40000), colour = "grey50", linewidth = 2) +
  #geom_segment(aes(x = 194, xend = 206, y = 40000, yend = 40000), colour = "grey50", linewidth = 2) +
  #geom_segment(aes(x = 207, xend = 221, y = 40000, yend = 40000), colour = "grey75", linewidth = 2) +
  #geom_segment(aes(x = 222, xend = 223, y = 40000, yend = 40000), colour = "grey75", linewidth = 2) +
  #annotate("text", label = "Asia", x = 3.5, y = 150000, angle = 90, colour = "grey50", size = 3.5) +
  #annotate("text", label = "Europe", x = 86.5, y = 150000, angle = 90, colour = "grey50", size = 3.5) +
  #annotate("text", label = "N Am", x = 180, y = 150000, angle = 90, colour = "grey50", size = 3.5) +
  #annotate("text", label = "S & C Am", x = 200, y = 150000, angle = 90, colour = "grey50", size = 3.5) +
  #annotate("text", label = "Asia", x = 214, y = 150000, angle = 90, colour = "grey75", size = 3.5) +
#annotate("text", label = "S & C Am", x = 222.5, y = 150000, angle = 90, colour = "grey75", size = 3.5) +
annotate("text", x = 67.5, y = 150000, label = "Aves", fontface = "bold") +
  annotate("text", x = 141.5, y = 150000, label = "Reptlia", fontface = "bold") +
  ylab("Reporter difference (Com:NonCom)") +
  xlab("Taxon") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),
        legend.position = "bottom", legend.title = element_blank())



Launder_plt<- ggarrange(
  ggarrange(Launder_concept_plt, ggplot() + theme_void(), LaunderWC_plt, LaunderRC_plt, nrow = 1, 
            widths = c(1,0.1, 1, 0.75), align = "hv", labels = c("A.","", "B.", "C.")),
  LaunderCom_plt, nrow = 2, labels = c("", "D."))

ggsave("Summaries/Figures/Launder.png", Launder_plt, device = "png", 
       width = 10, height = 8, bg = "white")


#### Launder switch ####

AvesRept_checked_WOE %>% 
  filter(Check_7 == FALSE) %>% group_by(Launder_SwWC) %>%
  reframe(inci = n(), vol = sum(Vol)) %>%
  ungroup() %>% 
  mutate(tot_inc = sum(inci), tot_vol = sum(vol),
         prop_inc = inci/tot_inc *100, prop_vol = vol/tot_vol *100)


Launder_swWC <- AvesRept_checked_WOE %>% 
  filter(Launder_SwWC == 1, Vol > 10) %>%
  group_by(ROW_ID) %>%
  mutate(meanC = mean(c(Year_5_vol_Capt + Year_5_vol_Ranch, 
                        Year_4_vol_Capt + Year_4_vol_Ranch,
                        Year_3_vol_Capt + Year_3_vol_Ranch,
                        Year_2_vol_Capt + Year_2_vol_Ranch,
                        Year_1_vol_Capt + Year_1_vol_Ranch)),
         meanW = mean(c(Year_5_vol_Wild,
                        Year_4_vol_Wild,
                        Year_3_vol_Wild,
                        Year_2_vol_Wild,
                        Year_1_vol_Wild)),
         diffC = (Vol - mean(c(Year_5_vol_Capt + Year_5_vol_Ranch, 
                               Year_4_vol_Capt + Year_4_vol_Ranch,
                               Year_3_vol_Capt + Year_3_vol_Ranch,
                               Year_2_vol_Capt + Year_2_vol_Ranch,
                               Year_1_vol_Capt + Year_1_vol_Ranch))),
         diffW = (Year_0_vol_Wild - mean(c(Year_5_vol_Wild,
                                           Year_4_vol_Wild,
                                           Year_3_vol_Wild,
                                           Year_2_vol_Wild,
                                           Year_1_vol_Wild))),
         Year_0_vol_CDFR = Year_0_vol_Capt + Year_0_vol_Ranch, 
         Year_1_vol_CDFR = Year_1_vol_Capt + Year_1_vol_Ranch,
         Year_2_vol_CDFR = Year_2_vol_Capt + Year_2_vol_Ranch,
         Year_3_vol_CDFR = Year_3_vol_Capt + Year_3_vol_Ranch,
         Year_4_vol_CDFR = Year_4_vol_Capt + Year_4_vol_Ranch, 
         Year_5_vol_CDFR = Year_5_vol_Capt + Year_5_vol_Ranch) %>%
  ungroup() %>%
  mutate(dir = ifelse(diffC < 0, "C switched to W", "W switched to C"),
         diffC = abs(diffC),
         diffW = abs(diffW)) %>%
  mutate(equal_diff = ifelse(diffC == diffW, diffC, NA)) %>%
  unite("Wild_seq", Year_0_vol_Wild:Year_5_vol_Wild, remove = FALSE) %>%
  unite("CDFR_seq", Year_0_vol_CDFR:Year_5_vol_CDFR) %>%
  select(Year, Taxon, Class, Family, Exporter, region, Vol,Year_0_vol_Wild, Wild_seq, CDFR_seq, 
         meanC, meanW, 
         diffC, diffW, dir, IUCN_code, ROW_ID,
         equal_diff) %>%
  unite( "ID", Year:Exporter,remove = FALSE)%>%
  ungroup() %>%
  arrange(Class, diffC) %>%
  mutate(pos = seq(1:n()),
         sp = str_split_fixed(Taxon, " ", 2),
         gen = substr(Taxon, 1, 1),
         Taxon_short = paste(gen, ". ", sp[,2] ))  %>% 
  mutate(label1 = paste0(Exporter, ", ", Year),
         label2 = paste0(Exporter, ", ", Year),
         label1 = ifelse(pos %% 2 == 0, label1, ""),
         label2 = ifelse(pos %% 2 == 1, label2, "")) 

Launder_swWC %>% group_by(IUCN_code) %>% summarise(n = n(), sp = n_distinct(Taxon))


SwWC_plt <- ggplot(Launder_swWC, aes(x = reorder(ID, pos), y = diffC, group = ROW_ID)) +
  geom_segment(aes(xend = ID, yend = diffW), linewidth = .5, alpha = .75) +
  geom_point(size = 2, shape = 21, fill = "white") +
  geom_point(aes(y = diffW), size = 2)  +
  geom_point(aes(y = equal_diff), size = 2) +
  #scale_colour_manual(values = c("tomato","black","chartreuse4", "dodgerblue", "goldenrod")) +
  scale_y_log10(limits = c(0.5, 550000)) +
  scale_x_discrete(labels = Launder_swWC$Taxon_short) +
  geom_text(aes(x = ID, label = label1),
            size = 3, nudge_y =1, show.legend = FALSE, angle = 90) +
  geom_text(aes(x = ID, label = label2),
            size = 3, nudge_y =-1, show.legend = FALSE, angle = 90) +
  geom_segment(aes(x = 1, xend = 21, y = 200000, yend = 200000), colour = "black") +
  geom_segment(aes(x = 1, xend = 1, y = 200000, yend = 160000), colour = "black") +
  geom_segment(aes(x = 21, xend = 21, y = 200000, yend = 160000), colour = "black") +
  geom_segment(aes(x = 22, xend = 89, y = 200000, yend = 200000), colour = "black") +
  geom_segment(aes(x = 22, xend = 22, y = 200000, yend = 160000), colour = "black") +
  geom_segment(aes(x = 89, xend = 89, y = 200000, yend = 160000), colour = "black") +
  annotate("text", x = 11, y = 300000, label = "Aves", fontface = "bold") +
  annotate("text", x = 55.5, y = 300000, label = "Reptlia", fontface = "bold") +
  ylab("Source switch (CDFR:WUX)") +
  xlab("Taxon") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic", size = 7),
        legend.position = "none")

## Map of incidences plus histo of frequency of species to show different species
SwWC_exp <- Launder_swWC %>% group_by(Exporter, region) %>% tally()
SwWC_sp<- Launder_swWC %>% group_by(Taxon) %>% tally() %>%
  group_by(n) %>% summarise(freq = n())

world <- ne_countries(scale = "medium", returnclass = "sf", type = "countries") 
SwWC_map <- left_join(world, SwWC_exp, by = c("iso_a2" = "Exporter"))


SwWC_map_plt <- ggplot() + geom_sf(data = SwWC_map, aes(fill = n), colour = "grey25") +
  scale_fill_gradientn(name = "Records", na.value="grey95",
                       colours = c( "palegoldenrod",  "orange","darkred")) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.5)) +
  theme_classic(base_size = 10) +
  theme(legend.position = "right")

SwWC_tally_plt <- ggplot(SwWC_sp, aes(n, freq)) + 
  geom_col(aes(fill = n), colour = "black", width = 1) +
  scale_fill_gradientn(colours = c( "palegoldenrod",  "orange","darkred")) +
  xlab("Species occurences") +
  ylab("Frequency") +
  theme_minimal(base_size = 8) +
  theme(legend.position = "none")

library(cowplot)
SwWC_map_plt2 <- ggdraw() +
  draw_plot(SwWC_map_plt) +
  draw_plot(SwWC_tally_plt, x = 0., y = 0.05, width = .2, height = .4)

Concept_dat <- data.frame(Source = c(rep("C", 6), rep("W", 6)), Vol = c(1, 3, 2, 2, 1, 11, 9, 10, 8, 9, 8, 0),
                          Year = c(1:6, 1:6)) %>%
  group_by(Source) %>%
  mutate(mean = mean(Vol[1:5]))

Sw_concept <- ggplot(Concept_dat, aes(Year, Vol, colour = Source)) +
  geom_point(size = 1.5) +
  geom_line(alpha = .5) +
  scale_color_manual(values = c("black", "grey")) +
  geom_segment(aes(x = 1, xend = 5, y = mean, yend = mean),
               linetype = "longdash", linewidth = .7) +
  coord_cartesian(xlim = c(1, 8)) +
  geom_segment(aes(x = 7, xend = 7, y = 1.8, yend = 11),
               arrow = arrow(), colour = "black", size = 1) +
  geom_segment(aes(x = 8, xend = 8, y = 8.8, yend = 0),
               arrow = arrow(), colour = "grey", size = 1) +
  geom_point(x = 8, y = 10, shape = 16, colour = "grey", size = 2) +
  geom_point(x = 7, y = .5, shape = 21, colour = "black", size = 2) +
  annotate(geom = "text", label = expression(Delta*"Captive"), x = 6.5, y = 6, angle = 90,
           colour = "black", fontface = "bold") +
  annotate(geom = "text", label = expression(Delta*"Wild"), x = 7.5, y = 5, angle = 90,
           colour = "grey", fontface = "bold") +
  ylab("Volume") +
  theme_minimal() +
  theme(axis.text = element_blank(), legend.position = "none")

Switch_plt <- ggarrange(
  ggarrange(Sw_concept, ggplot() + theme_void(), SwWC_map_plt2, widths = c(2, .5, 5), 
            nrow =1, labels = c("A.", "", "B.")),
  SwWC_plt, nrow = 2, heights = c(1, 1.5), labels = c("", "C."))

ggsave("Summaries/Figures/Switch.png", Switch_plt, device = "png", 
       width = 10, height = 7, bg = "white")

#### Volume and range ####

AvesRept_checked_WOE %>% group_by(Check_7, Class) %>% tally(Vol) %>%
  group_by(Class) %>% mutate(Tot = sum(n), Prop = n/Tot) 

AvesRept_checked_WOE %>% group_by(Taxon, Exp_total) %>% tally %>% ungroup() %>% reframe(mean(Exp_total))
AvesRept_checked_WOE %>% group_by(Class, Taxon, Exp_total) %>% tally %>% group_by(Class) %>% reframe(mean(Exp_total))

Source_dat <- AvesRept_checked_WOE %>% group_by(Check_7, Check_8, Check_9, Class) %>% summarise(tot = sum(Vol)) %>%
  mutate(cat = case_when((Check_7 + Check_8 + Check_9) == 3  ~ "Imrobable occurence",
                         (Check_7 + Check_8 + Check_9) < 3 ~ "Probable occurence"),
         cat = ifelse(Check_7 == FALSE, "Within range", cat)) %>% 
  group_by(Class, cat) %>%
  summarise(tot = sum(tot)) %>%
  group_by(Class) %>% mutate(ovr_tot = sum(tot),
                             prop = tot/ovr_tot) 

Range_raw <- AvesRept_checked_WOE %>% filter(Legal_acq_grp == 1.5) %>% group_by(Class, Exporter) %>% summarise(tot = sum(Vol))
Exporters_sp <- AvesRept_checked_WOE %>% filter(Legal_acq_grp == 1.5)  %>% 
  group_by(Class, Taxon) %>% summarise(Exporters = n_distinct(Exporter))
Exporters_tally <- Exporters_sp %>% group_by(Exporters) %>% tally

Range_first_all <- AvesRept_checked_WOE %>% filter(Novel_exp == 1) %>% 
  group_by(Taxon) %>% mutate(Years_traded = n()) %>%
  arrange(Year) %>% slice_head(n=1) %>% 
  ungroup() %>%
  arrange(Class, Vol) %>% mutate(pos = 1:n(),
                                 sp = str_split_fixed(Taxon, " ", 2),
                                 gen = substr(Taxon, 1, 1),
                                 Taxon_short = paste(gen, ". ", sp[,2] )) 

Range_first <- Range_first_all %>% filter(Vol >10) %>% 
  mutate(label1 = paste0(Exporter, ", ", Year),
         label2 = paste0(Exporter, ", ", Year),
         label1 = ifelse(pos %% 2 == 0, label1, ""),
         label2 = ifelse(pos %% 2 == 1, label2, ""))

OOR_records <- AvesRept_checked_WOE %>% filter(Legal_acq_grp == 1.5)%>%
  arrange(Class, Vol) %>% mutate(pos = 1:n())

Concept_dat <- AvesRept_checked_WOE %>% group_by(Check_7, Check_8, Check_9, Taxon, Class, Exporter) %>% 
  summarise(tot = sum(Vol)) %>%
  mutate(cat = case_when((Check_7 + Check_8 + Check_9) == 3  ~ "Improbable occurence",
                         (Check_7 + Check_8 + Check_9) < 3 ~ "Probable occurence"),
         cat = ifelse(Check_7 == FALSE, "Within range", cat)) %>%
  group_by(Taxon) %>% filter(n_distinct(cat) > 2) %>%
  mutate(n = n()) %>%
  filter(Taxon == "Psittacus erithacus")

world <- ne_countries(scale = "medium", returnclass = "sf", type = "countries") 
RAves_map <- left_join(world, filter(Range_raw, Class == "Aves"), by = c("iso_a2" = "Exporter"))
RRept_map <- left_join(world, filter(Range_raw, Class == "Reptilia"), by = c("iso_a2" = "Exporter"))
Concept_map <- left_join(world, Concept_dat, by = c("iso_a2" = "Exporter"))

Concept_map_plt <- ggplot() + 
  geom_sf(data = Concept_map, aes(fill = cat), colour = "black") +
  scale_fill_manual(na.value="grey85", breaks = c("Improbable occurence", "Probable occurence", "Within range"), 
                    values = c("darkorange", "grey25", "dodgerblue")) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  theme_classic(base_size = 10) +
  theme(legend.position = "right", legend.justification = "bottom", legend.title = element_blank())

Exp_tally <- AvesRept_checked_WOE %>% group_by(Taxon, Exp_total) %>% tally
Exp_tally_plt <- ggplot(Exp_tally, aes(Exp_total)) + 
  geom_histogram(bins = 40, fill = "grey", colour = "black") +
  xlab("Exporters per species") +
  ylab("Frequency") +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(), axis.line.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_blank())

RAves_map_plt <- ggplot() + geom_sf(data = RAves_map, aes(fill = tot), colour = "grey25") +
  scale_fill_gradientn(name = "Volume of \n improbable occurences", na.value="grey95", trans = "log10",
                       colours = c( "palegoldenrod",  "orange","darkred"),
                       limits = c(1, 65000)) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.5)) +
  theme_classic(base_size = 8) +
  theme(legend.position = "bottom", legend.key.height= unit(.2, 'cm'),
        legend.key.width= unit(.5, 'cm'))

RRept_map_plt <- ggplot() + geom_sf(data = RRept_map, aes(fill = tot), colour = "grey25") +
  scale_fill_gradientn(name = "Volume of \n improbable occurences", na.value="grey95", trans = "log10",
                       colours = c( "palegoldenrod",  "orange","darkred"),
                       limits = c(1, 40000)) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.5)) +
  theme_classic(base_size = 8) +
  theme(legend.position = "bottom", legend.key.height= unit(.2, 'cm'),
        legend.key.width= unit(.5, 'cm'))

Aves_range_bars <- ggplot(filter(Source_dat, Class == "Aves"), aes(cat, tot, fill = cat)) +
  geom_col(width = 1, colour = "black") +
  scale_fill_manual(values = c("darkorange", "grey25", "dodgerblue")) +
  scale_x_discrete(labels = c("Impr Occ", "Pr Occ", "w/range")) +
  # scale_y_log10(breaks = c(10^1, 10^3, 10^5, 10^7),
  #   labels = expression(10^1, 10^3, 10^5, 10^7)) +
  xlab("") +
  ylab("Volumes (WOEs)") +
  coord_flip() +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none", axis.text.y = element_text(colour = "black"))

Rept_range_bars <- ggplot(filter(Source_dat, Class == "Reptilia"), aes(cat, tot, fill = cat)) +
  geom_col(width = 1, colour = "black") +
  scale_fill_manual(values = c("darkorange", "grey25", "dodgerblue")) +
  scale_x_discrete(labels = c("Impr Occ", "Pr Occ", "w/range")) +
  #scale_y_log10(breaks = c(10^1, 10^3, 10^5, 10^7),
  #            labels = expression(10^1, 10^3, 10^5, 10^7)) +
  xlab("") +
  ylab("Volumes (WOEs)") +
  coord_flip() +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none", axis.text.y = element_text(colour = "black"))



RAves_map_plt2<- ggdraw() +
  draw_plot(RAves_map_plt) +
  draw_plot(Aves_range_bars, x = 0., y = 0.0, width = .25, height = .5)

RRept_map_plt2<- ggdraw() +
  draw_plot(RRept_map_plt) +
  draw_plot(Rept_range_bars, x = 0., y = 0.0, width = .25, height = .5)

All_OOR_Records <- ggplot(OOR_records, aes(pos, Vol, colour = Exp_total)) +
  scale_color_viridis_c(trans = "log10") +
  geom_point(alpha = .5, size = 2) +
  geom_segment(aes(x = 1, xend = 2233, y = 20000, yend = 20000), colour = "black") +
  geom_segment(aes(x = 1, xend = 1, y = 20000, yend = 18000), colour = "black") +
  geom_segment(aes(x = 2233, xend = 2233, y = 20000, yend = 15000), colour = "black") +
  geom_segment(aes(x = 2234, xend = 3179, y = 20000, yend = 20000), colour = "black") +
  geom_segment(aes(x = 2234, xend = 2234, y = 20000, yend = 15000), colour = "black") +
  geom_segment(aes(x = 3179, xend = 3179, y = 20000, yend = 15000), colour = "black") +
  annotate("text", x = 1116, y = 40000, label = "Aves", fontface = "bold") +
  annotate("text", x = 2706.5, y = 40000, label = "Reptlia", fontface = "bold") +
  xlab("Records where species is an improbable occurence") +
  ylab("Volume (WOEs)") +
  guides(colour = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.5,
                                 title = "Exporters trading \n the species")) +
  scale_y_log10() +
  theme_minimal() +
  theme(legend.position=c(.2,.75), legend.direction = "horizontal", legend.key.height= unit(.2, 'cm'),
        legend.key.width= unit(.5, 'cm'))



First1_OOR_Records <- ggplot(Range_first, aes(reorder(Taxon, pos), Vol)) + 
  scale_y_log10() +
  geom_point() +
  coord_cartesian(ylim = c(0.4, 70000)) +
  geom_text(aes(x = Taxon, label = label1),
            size = 3, nudge_y =1, show.legend = FALSE, angle = 90) +
  geom_text(aes(x = Taxon, label = label2),
            size = 3, nudge_y =-1, show.legend = FALSE, angle = 90) +
  geom_segment(aes(x = 1, xend = 11, y = 10000, yend = 10000), colour = "black") +
  geom_segment(aes(x = 1, xend = 1, y = 10000, yend = 8000), colour = "black") +
  geom_segment(aes(x = 11, xend = 11, y = 10000, yend = 8000), colour = "black") +
  geom_segment(aes(x = 12, xend = 21, y = 10000, yend = 10000), colour = "black") +
  geom_segment(aes(x = 12, xend = 12, y = 10000, yend = 8000), colour = "black") +
  geom_segment(aes(x = 21, xend = 21, y = 10000, yend = 8000), colour = "black") +
  annotate("text", x = 6.5, y = 25000, label = "Aves", fontface = "bold") +
  annotate("text", x = 16.5, y = 25000, label = "Reptlia", fontface = "bold") +
  xlab("Taxa only traded from one exporter (where species is improbable occurence)") +
  ylab("Volume (WOEs)") +
  #scale_colour_manual(values = c("tomato","black","chartreuse4","purple4", "dodgerblue", "goldenrod")) +
  
  scale_x_discrete(labels = Range_first$Taxon_short) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic", size = 9),
        legend.position = "none")


OOR_range_master <- ggarrange(
  ggarrange(Exp_tally_plt, Concept_map_plt, widths = c(1, 3), labels = c("A.", "B.")),
  ggarrange(RAves_map_plt2, RRept_map_plt2, nrow = 1, labels = c("C.", "D.")),
  First1_OOR_Records, ncol = 1, heights = c(1, 1, 1.5), labels = c("", "", "D."))

OOR_range_master <- ggarrange(
  ggarrange(Aves_range_bars, Rept_range_bars, nrow = 1, labels = c("A.", "B.")),
  ggarrange(Exp_tally_plt, Concept_map_plt, widths = c(1, 3), labels = c("C.", "D.")),
  First1_OOR_Records, ncol = 1, heights = c(.75, 1, 1.5), labels = c("", "", "E."))

P_eri <- grid::rasterGrob(jpeg::readJPEG("Data/Images/P_erithacus.jpg"), interpolate = TRUE)
OOR_range_master2 <- OOR_range_master + annotation_custom(P_eri, xmin = .85, xmax = 1, ymin = .6, ymax = .75)

ggsave("Summaries/Figures/Range.png", OOR_range_master2, device = "png", 
       width = 10, height = 10, bg = "white")


#### SOM Checks ####
Sum_totalsI <- checks_summary(data = AvesRept_checked_WOE, groups = c("Appendix"), 
                              App1_only = TRUE, format = "long")  %>%
  mutate(Check = factor(Check, levels = c("Check_1", "Check_2", "Check_3", "Check_4", "Check_5",
                                          "Check_6", "Check_7", "Check_8", "Check_9", "Check_10",
                                          "Check_11", "Check_12", "Check_13")),
         prop = count/Total_count)
## 33937 total records
nrow(AvesRept_checked_WOE %>% filter(Appendix != "I"))
nrow(AvesRept_checked_WOE %>% filter(Appendix == "I"))

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
  coord_cartesian(ylim = c(1, 5000), expand = FALSE) +
  scale_x_discrete(labels = c("Check_1" = "1", "Check_2" = "2", "Check_3" = "3", "Check_4" = "4", 
                              "Check_5" = "5", "Check_6" = "6", "Check_7" = "7", "Check_8" = "8", 
                              "Check_9" = "9", "Check_10" = "10", "Check_11" = "11", "Check_12" = "12",
                              "Check_13" = "13")) +
  annotate("text", label = "Volume trends", x = 2, y = 4500, fontface = "bold", size = 3) +
  annotate("text", label = "Code\n switching", x = 5, y = 4500, fontface = "bold", size = 3) +
  annotate("text", label = "Legal acquisition", x = 8, y = 4500, fontface = "bold", size = 3) +
  annotate("text", label = "Reporting inconsistencies", x = 11.5, y = 4500, fontface = "bold", size = 3) +
  ylab("Tally of fails") +
  xlab("Criteria") +
  #scale_y_log10() +
  theme_minimal(base_size = 14)

check_tally_plotII <- ggplot(Sum_totalsII, aes(Check, n)) + 
  geom_rect(data = data.frame(Check = 1, n = 1), xmin = 0, xmax = 3.5, ymin = -Inf, ymax = Inf, fill = "lightskyblue1", alpha = .4) +
  geom_rect(data = data.frame(Check = 1, n = 1), xmin = 3.5, xmax = 6.5, ymin = -Inf, ymax = Inf, fill = "darkseagreen2", alpha = .4) +
  geom_rect(data = data.frame(Check = 1, n = 1), xmin = 6.5, xmax = 9.5, ymin = -Inf, ymax = Inf, fill = "rosybrown1", alpha = .4) +
  geom_rect(data = data.frame(Check = 1, n = 1), xmin = 9.5, xmax = 113.5, ymin = -Inf, ymax = Inf, fill = "lightgoldenrod", alpha = .4) +
  geom_col(fill = "grey", colour = "black", width = .75) + 
  coord_cartesian(ylim = c(1, 30000), expand = FALSE) +
  scale_x_discrete(labels = c("Check_1" = "1", "Check_2" = "2", "Check_3" = "3", "Check_4" = "4", 
                              "Check_5" = "5", "Check_6" = "6", "Check_7" = "7", "Check_8" = "8", 
                              "Check_9" = "9", "Check_10" = "10", "Check_11" = "11", "Check_12" = "12",
                              "Check_13" = "13")) +
  annotate("text", label = "Volume trends", x = 2, y = 27000, fontface = "bold", drop = FALSE, size = 3) +
  annotate("text", label = "Code\n switching", x = 5, y = 27000, fontface = "bold", size = 3) +
  annotate("text", label = "Legal acquisition", x = 8, y = 27000, fontface = "bold", size = 3) +
  annotate("text", label = "Reporting inconsistencies", x = 11.5, y = 27000, fontface = "bold", size = 3) +
  ylab("Tally of fails") +
  xlab("Criteria") +
  #scale_y_log10() +
  theme_minimal(base_size = 14)

AvesRept_checked_WOE <- AvesRept_checked_WOE %>% arrange(Score, App_sum) %>% mutate(sc_order = 1:n())
tally <- AvesRept_checked_WOE %>% group_by(Class, App_sum, Score) %>% tally()
AvesRept_checked_WOE %>% group_by(Class, App_sum) %>% summarise(mean = mean(Score))
AvesRept_checked_WOE %>% group_by(Class, Score) %>% tally() %>% filter(Score == 0)
AvesRept_checked_WOE %>% group_by(Class, App_sum, Score) %>% tally() %>% filter(Score == 0)

score_tally_plt <- ggplot(AvesRept_checked_WOE, aes(Score, sc_order, colour = App_sum)) + 
  geom_point(shape = 15) +
  scale_color_manual(values = c("dodgerblue", "black"), name = "Appendix") +
  xlab("Failed criteria") +
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

#### Extinct species ####

Ex_species <- AvesRept_checked_WOE %>% filter(IUCN_code %in% c("EW", "EX")) %>%
  select(Taxon, Year, Exporter, IUCN_code, Appendix, Vol, Ovr_score)
write.csv(Ex_species, "Summaries/Figures/Extinct_species.csv")


AvesRept_checked_WOE %>% filter(Taxon == "Rhodonessa caryophyllacea") %>%
  select(Taxon, Year, Exporter, IUCN_code, Appendix, Vol, Ovr_score)

CITES_MASTER %>% filter(Taxon == "Anas oustaleti")

