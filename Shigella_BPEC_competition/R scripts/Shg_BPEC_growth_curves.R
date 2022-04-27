library(readxl)
Growth_Curve_0_20h_Clean <- read_excel("Dropbox/Github/Bacteriocin-OLC-F-1905/Shigella_BPEC_competition/long_format_data/Growth_Curve_0-20h_Clean.xlsx", 
                                       col_types = c("text", "text", "text", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric"))

library(tidyverse)

logCFU_Growth_Curve <- Growth_Curve_0_20h_Clean %>%
  mutate(EC_ID = as.factor(EC_ID),
         EC_Description = as.factor(EC_Description),
         Count_Target = as.factor(Count_Target),
         Incubation_Time = as.numeric(Incubation_Time),
         logCFU_Rep1 = as.numeric(logCFU_Rep1),
         logCFU_Rep2 = as.numeric(logCFU_Rep2),
         logCFU_Rep3 = as.numeric(logCFU_Rep3),
         logCFU_AVG = as.numeric(logCFU_AVG),
         logCFU_SD = as.numeric(logCFU_SD))

library(dplyr)

logCFU_0637 <- filter(logCFU_Growth_Curve, EC_ID %in% c('OLC1614', 'OLC0637'))
logCFU_1512 <- filter(logCFU_Growth_Curve, EC_ID %in% c('OLC1614', 'OLC1512'))
logCFU_3094 <- filter(logCFU_Growth_Curve, EC_ID %in% c('OLC1614', 'OLC3094'))
logCFU_3095 <- filter(logCFU_Growth_Curve, EC_ID %in% c('OLC1614', 'OLC3095'))

library(ggplot2)

ggplot(logCFU_0637,aes(x = Incubation_Time, y = logCFU_AVG,
                       shape = EC_Description,
                       color = Count_Target)) +
  geom_point(size = 2) +
  geom_line(aes(linetype = EC_Description)) +
  geom_errorbar(aes(ymin = logCFU_AVG - logCFU_SD,
                    ymax = logCFU_AVG + logCFU_SD),
                position = position_dodge(0),
                width = 0.25) +
  labs(x = "Incubation Time (h)", y = "log CFU/mL") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,20)) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 13, face = 'bold', color = 'black'),
        axis.text = element_text(size = 10.5),
        # hjust 0.5 = in middle
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title = element_text(size = 13, face = 'bold'),
        strip.text = element_text(size = 13, face = 'bold')) 