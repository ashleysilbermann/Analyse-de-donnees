data_A <- read_excel(file.choose())
colnames(data_A)
data_A <- data_A %>%
  mutate(Technicien = factor(Technicien),
         lots = factor(lots),
         Runs = factor(Runs))
ggplot(data_A, aes(x = lots, y = Resultat,
                   color = Technicien)) +
  geom_point(size = 3) +
  theme_bw()
anova_A <- aov(Resultat ~ lots * Technicien, data = data_A)

summary(anova_A)

pdf("Gage_RR_Analyse_E.pdf", width = 12, height = 8)

ss.rr(var = Resultat,
      part = lots,
      appr = Technicien,
      data = data_A,
      tolerance = 10)

dev.off()