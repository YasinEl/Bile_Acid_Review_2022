#Install/Load necessary packages
if("data.table" %in% rownames(installed.packages()) == FALSE){
  install.packages("data.table")
}
if("ggplot2" %in% rownames(installed.packages()) == FALSE){
  install.packages("ggplot2")
}
if("ggforce" %in% rownames(installed.packages()) == FALSE){
  install.packages("ggforce")
}
if("ggVennDiagram" %in% rownames(installed.packages()) == FALSE){
  install.packages("ggVennDiagram")
}

library(data.table)
library(ggVennDiagram)
library(ggplot2)
library(ggforce)

#####################
#Generate Figure 1b
#####################
dt = fread("Table_for_figures_1b_and_1c.csv")
dbs_li = list(HMDB = unique(dt[DB == "HMDB" & BA_type != '']$SMILES), 
              LIPIDMAPS = unique(dt[DB == "LIPIDMAPS" & BA_type != '']$SMILES), 
              BILELIB19 = unique(dt[DB == "BILELIB19" & BA_type != '']$SMILES))

ggVennDiagram(dbs_li, label_alpha = 0)  +
  scale_fill_gradientn(colours = c('#8EBAD9', '#EA9293')) + 
  ggtitle(paste0(c(length(unique(dt$SMILES)), "unique bile acid SMILES"), collapse = " ")) +
  theme(text = element_text(size = 9)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


#####################
#Generate Figure 1c
#####################
dt = fread("Table_for_figures_1b_and_1c.csv")
dt = dt[abs(delta_mz) > 40, .(DB = ifelse(length(unique(DB[!is.na(DB)])) > 1, "Multiple", unique(DB[!is.na(DB)])), delta_mz = delta_mz[1]), by = .(alk_mods, ring_mods)]

ggplot(dt) +
  xlab("Oberservable delta mz from unconjugated bile acid") +
  geom_vline(aes(xintercept = round(delta_mz, 3), color = DB), dt, lwd = 1) +
  scale_color_manual(values=c('#8EBAD9', '#EA9293', '#FFBE86', "#C9B2DD")) +
  labs(color = "", text = element_text(size = 9)) +
  ggforce::facet_zoom(xlim = c(40, 220), zoom.size = 1) 


#####################
#Generate Figure Box1
#####################
dt = fread("Table_for_figure_Box1.csv")
dt_points = fread("Table_for_figure_Box1_2.csv")

ggplot(dt, aes(x = reorder(as.factor(name_values), concentration_mean), y = concentration_mean, fill = specimen)) +
  geom_bar(stat="identity", position=position_dodge(preserve = "single")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(size = 10)) +
  scale_fill_manual(values=c('#8EBAD9', '#EA9293')) +
  labs(fill = "") +
  xlab('HMDB name') +
  ylab('Concentration (micromol/L)') + 
  geom_errorbar(aes(ymin=concentration_min, 
                    ymax=concentration_max), 
                lwd = .02,
                width=.2,
                position=position_dodge(.9, preserve = "single")) +
  geom_jitter(data = dt_points, aes(x = name_values, y = concentration_mean, color = specimen),
              position = position_jitterdodge(jitter.width = 0.0, dodge.width = 0.9), fill = 'black', size = 1)  +
  scale_color_manual(values=c('black', 'black')) +
  facet_zoom(ylim = c(0,30), zoom.size = 1)


