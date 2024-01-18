# Charger les données
data = read.csv("psychology.csv", TRUE)
print(data)

# ---- Question 1 ----

# Statistiques descriptives pour l'IBE
summary_IBE = summary(data$IBE)
mean_IBE = mean(data$IBE)
median_IBE = median(data$IBE)
sd_IBE = sd(data$IBE)
IQR_IBE = IQR(data$IBE)

# Affichage des résultats
cat("Statistiques descriptives pour l'IBE:\n")
cat("Sommaire:", summary_IBE, "\n")
cat("Moyenne:", mean_IBE, "\n")
cat("Médiane:", median_IBE, "\n")
cat("Écart-type:", sd_IBE, "\n")
cat("IQR:", IQR_IBE, "\n")

# Statistiques descriptives pour l'IP
summary_IP = summary(data$IP)
mean_IP = mean(data$IP)
median_IP = median(data$IP)
sd_IP = sd(data$IP)
IQR_IP = IQR(data$IP)

# Affichage des résultats
cat("\nStatistiques descriptives pour l'IP:\n")
cat("Sommaire:", summary_IP, "\n")
cat("Moyenne:", mean_IP, "\n")
cat("Médiane:", median_IP, "\n")
cat("Écart-type:", sd_IP, "\n")
cat("IQR:", IQR_IP, "\n")

# ---- Question 2 ----

# Créer un histogramme pour la variable IBE
hist(data$IBE, main="Estimation de densité pour IBE", xlab="IBE", col="lightblue", border="black", probability=TRUE)
lines(density(data$IBE), col="red", lwd=2)

# Ajouter une légende
legend("topright", legend="Densité", col="red", lwd=2)

# Créer un histogramme pour la variable IP
hist(data$IP, main="Estimation de densité pour IP", xlab="IP", col="lightgreen", border="black", probability=TRUE)
lines(density(data$IP), col="blue", lwd=2)

# Ajouter une légende
legend("topright", legend="Densité", col="blue", lwd=2)

# ---- Question 3 ----

# Diviser par deux la largeur des intervalles pour IBE
new_breaks_ibe = seq(min(data$IBE), max(data$IBE), length.out = (length(seq(min(data$IBE), max(data$IBE))) - 1) * 2 + 1)

# Créer un histogramme pour la variable IBE avec des intervalles divisés par deux
hist(data$IBE, breaks = new_breaks_ibe, main="Estimation de densité pour IBE (Intervalles divisés par deux)", xlab="IBE", col="lightblue", border="black", probability=TRUE)
lines(density(data$IBE), col="red", lwd=2)

# Ajouter une légende
legend("topright", legend="Densité", col="red", lwd=2)

# Diviser par deux la largeur des intervalles pour IP
new_breaks_ip = seq(min(data$IP), max(data$IP), length.out = (length(seq(min(data$IP), max(data$IP))) - 1) * 2 + 1)

# Créer un histogramme pour la variable IP avec des intervalles divisés par deux
hist(data$IP, breaks = new_breaks_ip, main="Estimation de densité pour IP (Intervalles divisés par deux)", xlab="IP", col="lightgreen", border="black", probability=TRUE)
lines(density(data$IP), col="blue", lwd=2)

# Ajouter une légende
legend("topright", legend="Densité", col="blue", lwd=2)


# ---- Question 4 ----

# Estimation par noyau de la densité pour IBE
density_ibe = density(data$IBE)

# Tracer le KDE pour IBE
plot(density_ibe, main="Estimation par noyau de la densité pour IBE", xlab="IBE", col="blue", lwd=2)

# Estimation par noyau de la densité pour IP
density_ip = density(data$IP)

# Tracer le KDE pour IP
plot(density_ip, main="Estimation par noyau de la densité pour IP", xlab="IP", col="green", lwd=2)

# ---- Question 5 ----

# Estimation par noyau de la densité pour IBE avec noyau triangulaire et validation croisée biaisée
density_ibe_triangular = density(data$IBE, kernel = "triangular", bw = "nrd")

# Tracer le KDE pour IBE avec noyau triangulaire
plot(density_ibe_triangular, main="Estimation par noyau de la densité pour IBE (Noyau triangulaire)", xlab="IBE", col="orange", lwd=2)

# Estimation par noyau de la densité pour IP avec noyau triangulaire et validation croisée biaisée
density_ip_triangular = density(data$IP, kernel = "triangular", bw = "nrd")

# Tracer le KDE pour IP avec noyau triangulaire
plot(density_ip_triangular, main="Estimation par noyau de la densité pour IP (Noyau triangulaire)", xlab="IP", col="purple", lwd=2)

# ---- Question 6 ----

# Afficher le nuage de points entre les deux variables IBE et IP
plot(data$IBE, data$IP, main="Nuage de points entre IBE et IP", xlab="IBE", ylab="IP", col=c("blue", "green"), pch=16)

# Ajouter une régression linéaire pour illustrer la tendance
abline(lm(IP ~ IBE, data=data), col="red", lwd=2)

# Ajouter une légende
legend("topright", legend=c("IBE", "IP", "Régression linéaire"), col=c("blue", "green", "red"), lwd=2)

# Calculer et afficher la corrélation
correlation_coefficient = cor(data$IBE, data$IP)
cat("Coefficient de corrélation entre IBE et IP:", correlation_coefficient, "\n")
cat("La ligne de régression est inclinée vers le haut, cela suggère une relation positive entre les variables (quand l'une augmente, l'autre a tendance à augmenter).")

# ---- Question 7 ----

# Filtrer les valeurs positives avant la transformation logarithmique
positive_values = data$IP > 0
transformed_IBE = log(data$IBE[positive_values])
transformed_IP = log(data$IP[positive_values])

# Afficher le nuage de points avec les données transformées
plot(transformed_IBE, transformed_IP, main="Nuage de points entre IBE et IP (Transformées logarithmiques)", xlab="log(IBE)", ylab="log(IP)", col=c("purple", "orange"), pch=16)

# Ajouter une régression linéaire pour illustrer la tendance
abline(lm(transformed_IP ~ transformed_IBE), col="red", lwd=2)

# Ajouter une légende avec position manuelle
legend("topright", legend=c("log(IBE)", "log(IP)", "Régression linéaire"), col=c("purple", "orange", "red"), lwd=2)

# Calculer et afficher la corrélation pour les données transformées
correlation_coefficient_transformed = cor(transformed_IBE, transformed_IP)
cat("Coefficient de corrélation entre log(IBE) et log(IP):", correlation_coefficient_transformed, "\n")

# ---- Question 8 ----

# Calculer le coefficient de corrélation de Pearson
correlation_pearson = cor(transformed_IBE, transformed_IP, method = "pearson")
cat("Coefficient de corrélation de Pearson entre log(IBE) et log(IP):", correlation_pearson, "\n")

# Calculer le coefficient de corrélation de Spearman
correlation_spearman = cor(transformed_IBE, transformed_IP, method = "spearman")
cat("Coefficient de corrélation de Spearman entre log(IBE) et log(IP):", correlation_spearman, "\n")

# Calculer le coefficient de corrélation de Kendall
correlation_kendall = cor(transformed_IBE, transformed_IP, method = "kendall")
cat("Coefficient de corrélation de Kendall entre log(IBE) et log(IP):", correlation_kendall, "\n")

# ---- Question 9 ----

# Afficher le nuage de points avec les données transformées
plot(transformed_IBE, transformed_IP, main="Nuage de points entre IBE et IP (Transformées logarithmiques)", xlab="log(IBE)", ylab="log(IP)", col=c("purple", "orange"), pch=16)

# Ajouter une régression linéaire pour illustrer la tendance
abline(lm(transformed_IP ~ transformed_IBE), col="red", lwd=2)

# Ajouter une ligne diagonale (pente = 1) en rouge
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)

# Ajouter les noms des axes
text(x = min(transformed_IBE), y = max(transformed_IP), labels = "log(IBE)", pos = 4, col = "purple", cex = 1.2)
text(x = max(transformed_IBE), y = min(transformed_IP), labels = "log(IP)", pos = 2, col = "orange", cex = 1.2)

# Ajouter une légende avec position manuelle
legend("topright", legend=c("log(IBE)", "log(IP)", "Régression linéaire", "Diagonale"), col=c("purple", "orange", "red", "red"), lwd=2)

# Calculer et afficher la corrélation pour les données transformées
correlation_coefficient_transformed = cor(transformed_IBE, transformed_IP)
cat("Coefficient de corrélation entre log(IBE) et log(IP):", correlation_coefficient_transformed, "\n")

# ---- Question 10 ----
# Ouvrir le périphérique graphique pour le format PNG
png("question10_plot.png", width = 900, height = 700)

# Afficher le nuage de points avec les données transformées
plot(transformed_IBE, transformed_IP, main="Nuage de points entre IBE et IP (Transformées logarithmiques)", xlab="log(IBE)", ylab="log(IP)", col=c("purple", "orange"), pch=16)

# Ajouter une régression linéaire pour illustrer la tendance
abline(lm(transformed_IP ~ transformed_IBE), col="red", lwd=2)

# Ajouter une ligne diagonale (pente = 1) en rouge
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)

# Ajouter les noms des axes
text(x = min(transformed_IBE), y = max(transformed_IP), labels = "log(IBE)", pos = 4, col = "purple", cex = 1.2)
text(x = max(transformed_IBE), y = min(transformed_IP), labels = "log(IP)", pos = 2, col = "orange", cex = 1.2)

# Ajouter une légende avec position manuelle
legend("topright", legend=c("log(IBE)", "log(IP)", "Régression linéaire", "Diagonale"), col=c("purple", "orange", "red", "red"), lwd=2)

# Calculer et afficher la corrélation pour les données transformées
correlation_coefficient_transformed = cor(transformed_IBE, transformed_IP)
cat("Coefficient de corrélation entre log(IBE) et log(IP):", correlation_coefficient_transformed, "\n")

# Fermer le périphérique graphique et enregistrer le fichier PNG
dev.off()

