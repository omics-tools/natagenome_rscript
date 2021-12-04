#Check for required packages and install them
list.of.packages <- c("ggplot2", "ggrepel","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Load packages
library(ggplot2)
library(ggrepel)
library(tidyverse)

#Plot
df <- read.table("rep_mge_summary.txt",header=T)
labels <- df$Label
selected_labels <- c("FNDC_F1","FNDC_R1","FNDC_R2")
labels[!labels %in% selected_labels] <- ""
formula <- y ~ x
p <- ggplot(df, aes(x= MGEs , y= Repeats, label=labels)) +
  geom_point(alpha = 0.3) +
  facet_grid(Group~Seed, scales = "free") +
  geom_smooth(method = "lm", formula = formula, se = F) +
  geom_text_repel(min.segment.length = 0, seed = 42, box.padding = 0.5,size=2,max.overlaps = Inf)+
  theme_bw()+theme(legend.position = 'none')+xlab("Mobile genetic elements")+ylab("Duplicated Sequence")
ggsave(file = "rep_summary.pdf", plot = p,width=9,heigh=6)

#Correlation and Linear Regression Test
cor_df = df %>%
  group_by(Group,Seed) %>% summarize(R = round(cor.test(Repeats, MGEs)$estimate,3), p = signif(cor.test(Repeats, MGEs)$p.value,4), R_squared = round(summary(lm(Repeats ~ MGEs))$r.squared,3)) %>% as.data.frame()
write.table(cor_df,"cor_reg_results.txt",row.names = FALSE,sep="\t")
