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
  result = shapiro.test(data_list[[i]]$Performances)
  
  # Afficher le résultat du test
  cat("Shapiro-Wilk test for", years[i], ":", "\n")
  cat("w (statistique de test):", round(result$statistic, 4), "\n")
  cat("p-value:", round(result$p.value, 4), "\n")
  
  # Interprétation du résultat
  if (result$p.value > 0.05) {
    cat("Pas de preuve significative pour rejeter l'hypothèse nulle (distribution normale).\n\n")
  } else {
    cat("Preuve significative pour rejeter l'hypothèse nulle (non-distribution normale).\n\n")
  }
}

# ---- Question 3 ----
# Charger les données
data_2012 = read.table("company2012.dat", header = FALSE)
data_2017 = read.table("company2017.dat", header = FALSE)
data_2022 = read.table("company2022.dat", header = FALSE)

# Renommer les colonnes
colnames(data_2012) = colnames(data_2017) = colnames(data_2022) = "Performances"

# Calculer les différences de performances entre les années
diff_2012_2017 = data_2017$Performances - data_2012$Performances
diff_2017_2022 = data_2022$Performances - data_2017$Performances
diff_2012_2022 = data_2022$Performances - data_2012$Performances

# Effectuer les tests de Student appariés
test_2012_2017 = t.test(diff_2012_2017, alternative = "two.sided", mu = 0)
test_2017_2022 = t.test(diff_2017_2022, alternative = "two.sided", mu = 0)
test_2012_2022 = t.test(diff_2012_2022, alternative = "two.sided", mu = 0)

# Afficher les résultats des tests
cat("Test de Student apparié entre 2012 et 2017 : \n")
cat("Différence moyenne :", round(test_2012_2017$estimate, 4), "\n")
cat("Intervalle de confiance à 95% :", round(test_2012_2017$conf.int, 4), "\n")
cat("p-value :", round(test_2012_2017$p.value, 4), "\n")

cat("Test de Student apparié entre 2017 et 2022 : \n")
cat("Différence moyenne :", round(test_2017_2022$estimate, 4), "\n")
cat("Intervalle de confiance à 95% :", round(test_2017_2022$conf.int, 4), "\n")
cat("p-value :", round(test_2017_2022$p.value, 4), "\n")

cat("Test de Student apparié entre 2012 et 2022 : \n")
cat("Différence moyenne :", round(test_2012_2022$estimate, 4), "\n")
cat("Intervalle de confiance à 95% :", round(test_2012_2022$conf.int, 4), "\n")
cat("p-value :", round(test_2012_2022$p.value, 4), "\n")

# ---- Question 4 ----

# Effectuer le test de Wilcoxon signé-rangé pour 2012-2017
wilcox_2012_2017 = wilcox.test(diff_2012_2017, alternative = "two.sided", mu = 0)

# Afficher les résultats du test
cat("Test de Wilcoxon signé-rangé entre 2012 et 2017 : \n")
cat("Statistique de test W :", round(wilcox_2012_2017$statistic, 4), "\n")
cat("p-value :", round(wilcox_2012_2017$p.value, 4), "\n")

# Effectuer le test de Wilcoxon signé-rangé pour 2017-2022
wilcox_2017_2022 = wilcox.test(diff_2017_2022, alternative = "two.sided", mu = 0)

# Afficher les résultats du test
cat("Test de Wilcoxon signé-rangé entre 2017 et 2022 : \n")
cat("Statistique de test W :", round(wilcox_2017_2022$statistic, 4), "\n")
cat("p-value :", round(wilcox_2017_2022$p.value, 4), "\n")

# Effectuer le test de Wilcoxon signé-rangé pour 2012-2022
wilcox_2012_2022 = wilcox.test(diff_2012_2022, alternative = "two.sided", mu = 0)

# Afficher les résultats du test
cat("Test de Wilcoxon signé-rangé entre 2012 et 2022 : \n")
cat("Statistique de test W :", round(wilcox_2012_2022$statistic, 4), "\n")
cat("p-value :", round(wilcox_2012_2022$p.value, 4), "\n")