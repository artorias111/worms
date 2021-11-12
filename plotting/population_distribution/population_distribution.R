library("tidyverse")
library("cowplot")
library("ggthemes")

eleg <- read_tsv('elegans_length.tsv')

trop <- read_tsv('tropicalis_length.tsv')

brig <- read_tsv('briggsae_length.tsv')


eleg_plot <- ggplot(eleg,aes(x=avg_telomere_length)) + 
  theme_bw() + 
  theme_clean() +
  geom_histogram(bins = 25)+
  labs(x="Telomere length (kbp)",
       y="")
eleg_plot

ggsave("eleg.png",width=2.5,height=2.5,units = "in",dpi=300)



trop_plot <- ggplot(trop,aes(x=mean_length)) + 
  theme_bw() + 
  theme_clean() +
  geom_histogram(bins=25)+
  labs(x="",
       y="")
  #ggtitle("tropicalis telomere") +
  #xlab("Telomere length (kbp)") + ylab("Count")
trop_plot

ggsave("trop.png",width=2.5,height=2.5,units = "in",dpi=300)


brig_plot <- ggplot(brig,aes(x=tel_length)) + 
  theme_bw() + 
  theme_clean() +
  geom_histogram(bins=25)+
  labs(x="",
       y="Count")
  #gtitle("briggsae telomere") +
  #xlab("Telomere length (kbp)") + ylab("Count")
brig_plot

ggsave("brig.png",width=2.5,height=2.5,units = "in",dpi=300)


plot <- plot_grid(brig_plot,
                  eleg_plot,
                  trop_plot,
            
                  nrow=1
          )

plot <- plot+panel_border(remove = TRUE)
plot

