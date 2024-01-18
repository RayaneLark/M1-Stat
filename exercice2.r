# Installer et charger les bibliothèques nécessaires
library(ggplot2)

# Charger les données
performance_2012 <- read.table("company2012.dat", header = FALSE)
performance_2017 <- read.table("company2017.dat", header = FALSE)
performance_2022 <- read.table("company2022.dat", header = FALSE)

# Créer des graphiques pour chaque année avec des estimations de densité de noyau
ggplot() +
  # Histogramme et densité pour 2012
  geom_histogram(aes(x = performance_2012$Performance), bins = 30, alpha = 0.5, fill = "blue", color = "black") +
  geom_density(aes(x = performance_2012$Performance), color = "blue") +
  
  # Histogramme et densité pour 2017
  geom_histogram(aes(x = performance_2017$Performance), bins = 30, alpha = 0.5, fill = "green", color = "black") +
  geom_density(aes(x = performance_2017$Performance), color = "green") +
  
  # Histogramme et densité pour 2022
  geom_histogram(aes(x = performance_2022$Performance), bins = 30, alpha = 0.5, fill = "red", color = "black") +
  geom_density(aes(x = performance_2022$Performance), color = "red") +
  
  # Personnaliser les étiquettes et le titre
  labs(title = "Distribution des performances par année",
       x = "Performance",
       y = "Fréquence") +
  
  # Modifier les couleurs des légendes
  scale_fill_manual(values = c("blue", "green", "red")) +
  scale_color_manual(values = c("blue", "green", "red"))
