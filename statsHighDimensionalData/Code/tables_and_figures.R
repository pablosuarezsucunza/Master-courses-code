# Load packages -----------------------------------------------------------
library(tidyverse)
library(reshape2)
library(gt)


# Correlation hetmap ------------------------------------------------------
melted_cor <- melt(cor(as.matrix(og_ceo)))
melted_cor %>% 
  ggplot( aes(Var1, Var2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1),
        axis.title = element_blank()) + 
  coord_fixed(ratio = 1)
ggsave(paste0(results_dir, "/correlation_heatmap.png"), width = 6, height = 6, units= "in", dpi=1000)


# Summary stats -----------------------------------------------------------
df_sum_stats <- data.frame()
for (i in names(og_ceo)){
  df_sum_stats <- rbind(df_sum_stats, c(i, mean(og_ceo[[i]]), sd(og_ceo[[i]]),
                                         min(og_ceo[[i]]), 
                                        median(og_ceo[[i]]), max(og_ceo[[i]])))
}
names(df_sum_stats) <- c("Variable", "Mean", "SD", "Min", "Med", "Max")
df_sum_stats$Mean <- round(as.numeric(df_sum_stats$Mean), 3)
df_sum_stats$SD <- round(as.numeric(df_sum_stats$SD), 3)
df_sum_stats$Min <- round(as.numeric(df_sum_stats$Min), 1)
df_sum_stats$Med <- round(as.numeric(df_sum_stats$Med), 1)
df_sum_stats$Max <- round(as.numeric(df_sum_stats$Max), 1)

gt(df_sum_stats) %>% 
  as_latex() %>% 
  gsub("longtable", "tabular", .) %>% 
  writeLines(paste0(results_dir, "/summary_stats.tex"))
  # tab_header(title = "Summary statistics") %>%


# PCA ---------------------------------------------------------------------
#pca coefficients
# round(cbind(pca$rotation[,1]*(-1), pca$rotatio[,2:3]),3)%>%
#   as.data.frame() %>% 
#   rownames_to_column(var = "Variable") %>% 
#   gt() %>% 
#   cols_label(V1 = "PC1") %>%
#   # tab_header(title = "PCA coefficients") %>% 
#   gtsave(paste0(results_dir, "/pca_coefficients.tex"))
# 
# # pca rescaled coeffs
# round(cbind(pca$rotation[,1]*(-1)*pca$sdev[1], pca$rotation[,2]* pca$sdev[2], pca$rotation[,3]* pca$sdev[3]),3)%>%
#   as.data.frame() %>% 
#   rownames_to_column(var = "Variable") %>% 
#   gt() %>% 
#   cols_label(V1 = "PC1", v2 = "PC2", v3 = "PC3") %>%
#   # tab_header(title = "PCA rescaled coefficients") %>% 
#   gtsave(paste0(results_dir, "/pca_rescaled_coefficients.tex"))
# 
# # pca score coeffs
# round(cbind(pca$rotation[,1]*(-1)/pca$sdev[1], pca$rotation[,2]/pca$sdev[2], pca$rotation[,3]/pca$sdev[3]),3) %>% 
#   as.data.frame() %>% 
#   rownames_to_column(var = "Variable") %>% 
#   gt() %>% 
#   cols_label(V1 = "PC1", v2 = "PC2", v3 = "PC3") %>%
#   # tab_header(title = "PCA score coefficients") %>% 
#   gtsave(paste0(results_dir, "/pca_score_coefficients.tex"))


#all coefficients i the same table
cbind(rep("", nrow(pca$rotation)),
      round(cbind(pca$rotation[,1]*(-1), pca$rotatio[,2:3]),3),
      rep("", nrow(pca$rotation)),
      round(cbind(pca$rotation[,1]*(-1)*pca$sdev[1], pca$rotation[,2]* pca$sdev[2], pca$rotation[,3]* pca$sdev[3]),3),
      rep("", nrow(pca$rotation)),
      round(cbind(pca$rotation[,1]*(-1)/pca$sdev[1], pca$rotation[,2]/pca$sdev[2], pca$rotation[,3]/pca$sdev[3]),3)) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Variable") %>% 
  gt() %>% 
  tab_spanner(label = "Coefficients", columns = c(3:5)) %>%
  tab_spanner(label = "Rescaled coefficients", columns = c(7:9)) %>%
  tab_spanner(label = "Score coefficients", columns = c(11:13)) %>%
  cols_label(Variable = "Variable",
             V2 = "PC1",
             V6 = "PC1", V7 = "PC2", V8 = "PC3",
             V10 = "PC1", V11 = "PC2", V12 = "PC3",
             V9 = "", V1 = "", V5 = "") %>% 
  as_latex() %>% 
  gsub("longtable", "tabular", .) %>% 
  writeLines(paste0(results_dir, "/pca_coefficients.tex"))
  # tab_header(title = "PCA coefficients") %>%


#pca variation summary
round(summary(pca)$importance,3) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Variable") %>% 
  gt() %>% 
  cols_label(Variable = "") %>% 
  as_latex() %>% 
  gsub("longtable", "tabular", .) %>% 
  writeLines(paste0(results_dir, "/pca_variation_summary.tex"))
  # tab_header(title = "PCA variation summary") %>%



#scree plot
scree_plot <- data.frame(var = round(summary(pca)$importance[1,],3)^2,
           pc = 1:ncol(ceo)) %>% 
  ggplot(aes(x = pc, y = var)) +
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks = 1:ncol(ceo))+
  labs(x = "Principal component", y = "Variance")+
  theme_classic()
ggsave(scree_plot, filename = paste0(results_dir, "/scree_plot.png"), width = 4, height = 3, units= "in", dpi=1000)


#scatter plot of first three eigen values
pca_plot_12 <- pca$x %>%
  as.data.frame() %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.5)+
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.5)+
  geom_point(size=0.25)+
  ylim(-5,5)+
  xlim(-5,5)+
  labs(x = "PC1", y = "PC2")+
  theme_bw()
ggsave(pca_plot_12, filename = paste0(results_dir, "/pca_plot_12.png"), width = 3, height = 3, units= "in", dpi=1000)

pca_plot_13 <- pca$x %>%
  as.data.frame() %>%
  ggplot(aes(x = PC1, y = PC3)) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.5)+
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.5)+
  geom_point(size=0.25)+
  ylim(-5,5)+
  xlim(-5,5)+
  labs(x = "PC1", y = "PC3")+
  theme_bw()
ggsave(pca_plot_13, filename = paste0(results_dir, "/pca_plot_13.png"), width = 3, height = 3, units= "in", dpi=1000)

pca_plot_23 <- pca$x %>%
  as.data.frame() %>%
  ggplot(aes(x = PC2, y = PC3)) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.5)+
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.5)+
  geom_point(size=0.25)+
  ylim(-5,5)+
  xlim(-5,5)+
  labs(x = "PC2", y = "PC3")+
  theme_bw()
ggsave(pca_plot_23, filename = paste0(results_dir, "/pca_plot_23.png"), width = 3, height = 3, units= "in", dpi=1000)

# pca_plot_14 <- pca$x %>%
#   as.data.frame() %>%
#   ggplot(aes(x = PC1, y = PC4)) +
#   geom_vline(xintercept = 0, linetype = 2, linewidth = 0.5)+
#   geom_hline(yintercept = 0, linetype = 2, linewidth = 0.5)+
#   geom_point(size=0.25)+
#   ylim(-5,5)+
#   xlim(-5,5)+
#   labs(x = "PC1", y = "PC4")+
#   theme_bw()
# ggsave(pca_plot_14, filename = paste0(results_dir, "/pca_plot_14.png"), width = 3, height = 3, units= "in", dpi=1000)
# 
# pca_plot_24 <- pca$x %>%
#   as.data.frame() %>%
#   ggplot(aes(x = PC2, y = PC4)) +
#   geom_vline(xintercept = 0, linetype = 2, linewidth = 0.5)+
#   geom_hline(yintercept = 0, linetype = 2, linewidth = 0.5)+
#   geom_point(size=0.25)+
#   ylim(-5,5)+
#   xlim(-5,5)+
#   labs(x = "PC2", y = "PC4")+
#   theme_bw()
# ggsave(pca_plot_24, filename = paste0(results_dir, "/pca_plot_24.png"), width = 3, height = 3, units= "in", dpi=1000)
# 
# pca_plot_34 <- pca$x %>%
#   as.data.frame() %>%
#   ggplot(aes(x = PC3, y = PC4)) +
#   geom_vline(xintercept = 0, linetype = 2, linewidth = 0.5)+
#   geom_hline(yintercept = 0, linetype = 2, linewidth = 0.5)+
#   geom_point(size=0.25)+
#   ylim(-5,5)+
#   xlim(-5,5)+
#   labs(x = "PC3", y = "PC4")+
#   theme_bw()
# ggsave(pca_plot_34, filename = paste0(results_dir, "/pca_plot_34.png"), width = 3, height = 3, units= "in", dpi=1000)






# Factor analysis ---------------------------------------------------------
#varimax rotation
data.frame(Variable = rownames(fac_3$loadings), 
           Factor12 = round(fac2$loadings[,1],3),
           Factor22 = round(fac2$loadings[,2],3),
           Factor13 = round(fac_3$loadings[,1],3),
           Factor23 = round(fac_3$loadings[,2],3),
           Factor33 = round(fac_3$loadings[,3],3)) %>%
  gt() %>% 
  tab_spanner(label = "2-factor model", columns = c(Factor12, Factor22)) %>% 
  tab_spanner(label = "3-factor model", columns = c(Factor13, Factor23, Factor33)) %>% 
  cols_label(Variable = "Variable",
             Factor12 = "Factor 1",
             Factor22 = "Factor 2",
             Factor13 = "Factor 1",
             Factor23 = "Factor 2",
             Factor33 = "Factor 3") %>%
  as_latex() %>% 
  gsub("longtable", "tabular", .) %>% 
  writeLines(paste0(results_dir, "/factor_loadings_varimax.tex"))
  # tab_header(title = "Factor loadings (Varimax rotation)") %>% 

#oblimin rotation
data.frame(Variable = rownames(fac_3_oblimin$loadings), 
           Factor12 = round(fac2_oblimin$loadings[,1],3),
           Factor22 = round(fac2_oblimin$loadings[,2],3),
           Factor13 = round(fac_3_oblimin$loadings[,1],3),
           Factor23 = round(fac_3_oblimin$loadings[,2],3),
           Factor33 = round(fac_3_oblimin$loadings[,3],3)) %>%
  gt() %>% 
  tab_spanner(label = "2-factor model", columns = c(Factor12, Factor22)) %>% 
  tab_spanner(label = "3-factor model", columns = c(Factor13, Factor23, Factor33)) %>% 
  cols_label(Variable = "Variable",
             Factor12 = "Factor 1",
             Factor22 = "Factor 2",
             Factor13 = "Factor 1",
             Factor23 = "Factor 2",
             Factor33 = "Factor 3") %>% 
  as_latex() %>% 
  gsub("longtable", "tabular", .) %>% 
  writeLines(paste0(results_dir, "/factor_loadings_oblimin.tex"))



#mini table fac1 fac2 fac3
data.frame(value = c("p-value", "% variance explained"),
           fac1 = c(round(fac1$PVAL,3), round(sum(1-fac1$uniquenesses)/ncol(ceo)*100,1)),
           fac2 = c(round(fac2$PVAL,3), round(sum(1-fac2$uniquenesses)/ncol(ceo)*100,1)),
           fac3 = c("-", round(sum(1-fac_3$uniquenesses)/ncol(ceo)*100,1))) %>% 
  gt() %>%
  cols_label(value = "",
             fac1 = "1-factor",
             fac2 = "2-factor",
             fac3 = "3-factor") %>%
  as_latex() %>% 
  gsub("longtable", "tabular", .) %>% 
  writeLines(paste0(results_dir, "/factor_analysis_123.tex"))


#all_vars_communalities
t(data.frame(Communalities = round(1-fac2$uniquenesses,3),
              pctg_variance = round((1-fac2$uniquenesses)/ncol(ceo)*100,1),
              Uniqueness = round(fac2$uniquenesses,3))) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "v1") %>% 
  mutate(v1 = ifelse(v1 == "pctg_variance", "% variance explained", v1)) %>%
  gt() %>% 
  cols_label(v1 = "") %>%
  as_latex() %>% 
  gsub("longtable", "tabular", .) %>% 
  writeLines(paste0(results_dir, "/communalities.tex"))


#observed sestimated corr matrix
cor.m<-cor(ceo)
repcorr<-loadings(fac2)%*%t(loadings(fac2)) #reproduced correlation matrix
round(cor.m-repcorr,3) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Variable") %>%
  gt() %>%
  cols_label(Variable = "") %>%
  as_latex() %>% 
  gsub("longtable", "tabular", .) %>% 
  writeLines(paste0(results_dir, "/obs_est_corr_diff.tex"))
  # tab_header(title = "Observed - estimated correlation matrix difference") %>% 
 

