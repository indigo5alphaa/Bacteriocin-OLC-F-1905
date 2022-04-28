library(readxl)
Shg_BPEC_Ratio_20h <- read_excel("Dropbox/OLC-F-1915 Bacteriocin/Colicin Producer Genotypes Paper/Devon & Ryan's thesis copy/Shigella BPEC competition - Devon/Shg_BPEC_Ratio_20h.xlsx", 
                                 col_types = c("text", "text", "numeric", 
                                               "numeric", "numeric", "numeric"))

library(tidyverse)

Shg_BPEC_Ratio_20h <- Shg_BPEC_Ratio_20h %>%
  mutate(BPEC = as.factor(BPEC),
         Fraction_Intial = as.factor(Fraction_Intial),
         BPEC_Fraction_Overnight = as.numeric(BPEC_Fraction_Overnight),
         Shg_Fraction_Overnight = as.numeric(Shg_Fraction_Overnight),
         Ratio_Overnight_AVG = as.numeric(Ratio_Overnight_AVG),
         Ratio_Overnight_SD = as.numeric(Ratio_Overnight_SD))

library(ggplot2)
library(scales)

ggplot(Shg_BPEC_Ratio_20h,
       aes(x = Fraction_Intial, y = Ratio_Overnight_AVG)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin = Ratio_Overnight_AVG - Ratio_Overnight_SD,
                    ymax = Ratio_Overnight_AVG + Ratio_Overnight_SD),
                position = position_dodge(0.9), width = 0.2) +
  facet_wrap(~ BPEC) +
  labs(x = "Initial BPEC fraction (%)", y = "Bacterial density Ratio of BPEC:Shigella after Overnight")  +
  guides(color = guide_legend(title = 'Enrichment Media')) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 13, face = 'bold', color = 'black'),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 12, face = 'bold'),
        strip.text = element_text(size = 13, face = 'bold')) +
  scale_x_discrete(
    breaks = c('0.01', '0.1', '0.25', '0.5'),
    labels = c('1%', '10%', '25%', '50%')) 
 
  # scale_y_log10(labels = label_number(), breaks = c(0.005, 0.05, 0.5, 5, 50, 500, 5000)) +
  # annotation_logticks(sides = "lr") 

