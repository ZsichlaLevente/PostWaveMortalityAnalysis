# Libraries----
library(tidyverse)

# Data----
fatality.matrices <- read.csv("dataResultsGender/fatality.matrices.FR_UK_US.csv", header = T, sep = " ", dec = ".")
frailty.matrices <- read.csv("dataResultsGender/frailty.matrices.FR_UK_US.csv", header = T, sep = " ", dec = ".")
deathrate.matrices <- read.csv("dataResultsGender/deathrate.matrices.FR_UK_US.csv", header = T, sep = " ", dec = ".")
IFR.diff.vectors <- read.csv("dataResultsGender/IFR.diff.vectors.FR_UK_US.csv", header = T, sep = " ", dec = ".")

# Main code----
frailty.matrices.grouped <- group_split(frailty.matrices, gender)
frail_diff <- ggplot(
  data = frailty.matrices.grouped[[2]],
  aes(x = Var1 + 10 - 1, y = Var2 - 1)
) +
  geom_raster(aes(fill = value - frailty.matrices.grouped[[1]]$value)) +
  theme_bw() +
  labs(x = "Életkor", y = "YPL", title = "A frailty mátrixok különbsége a két nemre (férfi - nõi)", fill = "Fõ") +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 18, face = "bold"),
    title = element_text(size = 20),
    legend.text = element_text(size = 12)
  ) +
  scale_fill_gradient2(low = "black", high = "red", mid = "purple") +
  facet_grid(cols = vars(c_code))

deathrate.matrices.grouped <- group_split(summarize(group_by(deathrate.matrices, age, YPL, c_code, gender),
  mort_mean = mean(mort),
  .groups = "drop"
), gender)
mort_diff <- ggplot(
  data = deathrate.matrices.grouped[[2]],
  aes(x = age + 10 - 1, y = YPL - 1)
) +
  geom_raster(aes(fill = mort_mean - deathrate.matrices.grouped[[1]]$mort_mean)) +
  theme_bw() +
  labs(x = "Életkor", y = "YPL", title = "Az IFR-mátrixok átlagos különbsége (férfi - nõi)", fill = "IFR_{m}-IFR_{f}") +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 18, face = "bold"),
    title = element_text(size = 20),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  ) +
  scale_fill_gradient2(low = "black", high = "red", mid = "purple") +
  facet_grid(cols = vars(c_code))

fat_diff <- ggplot(
  data = summarize(group_by(fatality.matrices, age, YPL, c_code),
    deaths_m_orig_mean = mean(deaths_m_orig),
    deaths_f_orig_mean = mean(deaths_f_orig),
    .groups = "drop"
  ),
  aes(x = age + 10 - 1, y = YPL - 1)
) +
  geom_raster(aes(fill = deaths_m_orig_mean - deaths_f_orig_mean)) +
  theme_bw() +
  labs(x = "Életkor", y = "YPL", title = "A halálozási mátrixok különbsége azonos demográfiára vetítve (nõi)", fill = "Fõ") +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 18, face = "bold"),
    title = element_text(size = 20),
    legend.text = element_text(size = 12)
  ) +
  scale_fill_gradient2(low = "black", high = "red", mid = "purple") +
  facet_grid(cols = vars(c_code))

expl_diff <- ggplot(IFR.diff.vectors, aes((age + 20 - 1), explained_diff * 100)) +
  geom_line(aes(col = sse, group = paste(id, c_code))) +
  scale_color_gradient2(high = "black", low = "red", mid = "purple", midpoint = min(IFR.diff.vectors$sse) + ((max(IFR.diff.vectors$sse) - min(IFR.diff.vectors$sse)) / 2)) +
  ylim(0, 100) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 18, face = "bold"),
    title = element_text(size = 20),
    legend.text = element_text(size = 12)
  ) +
  labs(title = "Differences in the IFR between genders explained by the concept of frailty", x = "Age", y = "Explained difference[%]") +
  facet_grid(cols = vars(c_code))

# pdf("plotResultsGender/Results.pdf", width = 18, height = 7)
frail_diff
mort_diff
fat_diff
expl_diff
# dev.off()
