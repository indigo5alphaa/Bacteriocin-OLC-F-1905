library(readxl)
Shg_BPEC_Ratio_20h_stacked <- read_excel("Dropbox/OLC-F-1915 Bacteriocin/Colicin Producer Genotypes Paper/Devon & Ryan's thesis copy/Shigella BPEC competition - Devon/Shg_BPEC_Ratio_20h_stacked.xlsx", 
                                         col_types = c("text", "text", "text", 
                                                       "numeric"))

library(tidyverse)

Shg_BPEC_Ratio_20h_stacked <- Shg_BPEC_Ratio_20h_stacked %>%
  mutate(BPEC_ID = as.factor(BPEC_ID),
         Species = as.factor(Species),
         Fraction_Intial = as.factor(Fraction_Intial),
         Fraction_Overnight = as.numeric(Fraction_Overnight))

library(ggplot2)
library(scales)


# plots -------------------------------------------------------------------


ggplot(Shg_BPEC_Ratio_20h_stacked,
       aes(x = Fraction_Intial, y = Fraction_Overnight, fill = Species)) +
  geom_col(position = 'fill') +
  facet_wrap(~ BPEC_ID) +
  labs(x = "Initial BPEC fraction", y = "Fraction after Overnight Incubation")  +
  guides(color = guide_legend(title = 'Enrichment Media')) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 13, face = 'bold', color = 'black'),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title = element_text(size = 12, face = 'bold'),
        strip.text = element_text(size = 13, face = 'bold')) +
  scale_x_discrete(
    breaks = c('0.01', '0.1', '0.25', '0.5'),
    labels = c('1%', '10%', '25%', '50%')) +
  scale_y_continuous(labels = scales::percent)
