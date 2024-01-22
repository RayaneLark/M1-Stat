# ---- Question 1----

# Charger les données
data_2012 = read.table("company2012.dat", header = FALSE)
data_2017 = read.table("company2017.dat", header = FALSE)
data_2022 = read.table("company2022.dat", header = FALSE)

# Renommer les colonnes
colnames(data_2012) = colnames(data_2017) = colnames(data_2022) = "Performances"

# Créer une liste de données
data_list = list(data_2012, data_2017, data_2022)
years = c(2012, 2017, 2022)

# Afficher chaque histogramme séparément avec la densité de probabilité sur l'axe y
for (i in seq_along(data_list)) {
  # Créer un nouvel espace graphique
  par(mfrow = c(1, 1))
  
  # Créer un histogramme des performances avec densité sur l'axe y
  hist(data_list[[i]]$Performances, main = paste("Distribution des performances en", years[i]),
       xlab = "Performances", ylab = "Density", col = "lightblue", border = "black", freq = FALSE)
  
  # Ajouter une estimation par noyau de densité
  lines(density(data_list[[i]]$Performances), col = "red", lwd = 2)
}

# ---- Question 2 ----

# Le test de Shapiro-Wilk pour chaque ensemble de données
for (i in seq_along(data_list)) {
  result <- shapiro.test(data_list[[i]]$Performances)
  
  # Afficher le résultat du test
  cat("Shapiro-Wilk test for", years[i], ":", "\n")
  cat("w (Test statistic):", round(result$statistic, 4), "\n")
  cat("p-value:", round(result$p.value, 4), "\n")
  
  # Interprétation du résultat
  if (result$p.value > 0.05) {
    cat("Pas de preuve significative pour rejeter l'hypothèse nulle (distribution normale).\n\n")
  } else {
    cat("Preuve significative pour rejeter l'hypothèse nulle (non-distribution normale).\n\n")
  }
}


