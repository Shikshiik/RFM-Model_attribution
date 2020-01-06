#### library #####

library(tidyverse)
library(reshape2)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
library(ChannelAttribution)
library(markovchain)

#### Import csv - DF principale - Premier traitement ####

#DF1
csvAll3<-read.csv("V:/Commercial/ID_MARKET/DAM - Data/Analyses Ponctuelles/GRETA/IDMarket_greta_csvAll2.csv",encoding="UTF-8",sep=";", stringsAsFactors = FALSE)
csvAll3$Amount <- as.numeric(gsub(",", ".", gsub("\\.", "", csvAll3$Amount)))


# DF principale avec filtre #
dfUsefull <- csvAll3%>%select(Order.ID, User.id,New.Customer,Timestamp,Channel,
                              Source, Campaign, Type, Win.., Amount,Click,
                              Impression, Touch.points, Time.to.convert)

# DF modifié - Par User.id avec leurs Path et si cela mene a une conv ou pas
df_merge2 <- dfUsefull %>%
  arrange(User.id, Timestamp) %>%
  group_by(User.id) %>%
  summarise(path = paste(Channel, collapse = ' > '),
            conv = any(New.Customer == 1),
            conv_null = !any(New.Customer ==1)) %>%
  ungroup()

df_merge2$path<-substring(df_merge2$path, 4)
df_merge2[df_merge2==TRUE]=1
df_merge2[df_merge2==FALSE]=0
df_merge2$conv_null[is.na(df_merge2$conv_null)] = 1
df_merge2$conv[is.na(df_merge2$conv)] = 0
df_merge2 <- df_merge2 %>% filter(path!="")

#### DF secondaires - Pour FT et LT ####

# DF2 
csvAll5<-read.csv("V:/Commercial/ID_MARKET/DAM - Data/Analyses Ponctuelles/GRETA/IDMarket_greta_csvAll2_Amount.csv",encoding="UTF-8",sep=";", stringsAsFactors = FALSE)

df_ca_ft <- csvAll5%>%select(Order.ID, User.id,New.Customer, Timestamp, Channel,
                             Source,Campaign, Type, Win..,Amount, Click,
                             Impression, Touch.points, Time.to.convert)

df_merge3 <- df_ca_ft %>%
  arrange(User.id, Timestamp) %>%
  group_by(Order.ID, Amount) %>%
  summarise(path = paste(Channel, collapse = ' > '),
            conv = any(New.Customer == 1),
            conv_null = !any(New.Customer ==1)) %>%
  ungroup()

df_merge3$path<-substring(df_merge3$path, 4)
df_merge3[df_merge3==TRUE]=1
df_merge3[df_merge3==FALSE]=0
df_merge3$conv_null[is.na(df_merge3$conv_null)] = 1
df_merge3$conv[is.na(df_merge3$conv)] = 0
df_merge3 <- df_merge3 %>% filter(path!="")

df_hm2 <- df_merge3 %>%
  mutate(channel_name_ft = sub('>.*', '', path),
         channel_name_ft = str_trim(channel_name_ft, side = "right"),#first
         channel_name_lt = sub('.*>', '', path),
         channel_name_lt = str_trim(channel_name_lt, side = "left"))%>%
  filter(conv!=0)

# first-touch conversions
df_ft2 <- df_hm2 %>%
  group_by(channel_name_ft) %>%
  summarise(first_touch_conversions = sum(conv),
            Amount_ft = sum(Amount)) %>%
  ungroup()

# last-touch conversions
df_lt2 <- df_hm2 %>%
  group_by(channel_name_lt) %>%
  summarise(last_touch_conversions = sum(conv),
            Amount_lt = sum(Amount)) %>%
  ungroup()

#### calculating the models (Markov and heuristics) ####
model <- markov_model(df_merge2,
                      var_path = 'path',
                      var_conv = 'conv',
                      var_null = 'conv_null',                     
                      out_more = TRUE)

df_hm <- df_merge2 %>%
  mutate(channel_name_ft = sub('>.*', '', path),
         channel_name_ft = str_trim(channel_name_ft, side = "right"),#first
         channel_name_lt = sub('.*>', '', path),
         channel_name_lt = str_trim(channel_name_lt, side = "left"))%>%
  filter(conv!=0)

# first-touch conversions
df_ft <- df_hm %>%
  group_by(channel_name_ft) %>%
  summarise(first_touch_conversions = sum(conv)) %>%
  ungroup()

# last-touch conversions
df_lt <- df_hm %>%
  group_by(channel_name_lt) %>%
  summarise(last_touch_conversions = sum(conv)) %>%
  ungroup()

h_mod2 <- merge(df_ft, df_lt, by.x = 'channel_name_ft', by.y = 'channel_name_lt')

# merging all models
all_models <- merge(h_mod2, model$result, by.x = 'channel_name_ft', by.y = 'channel_name')
colnames(all_models)[c(1, 4)] <- c('channel_name', 'attrib_model_conversions')

#### visualizations - Matrice et Model FT/LT ##############

# transition matrix heatmap for "real" data            /// test pour enlever les others....
df_plot_trans <- model$transition_matrix %>%
  filter(channel_from!="Other" & channel_to!="Other" & channel_to!="(null)")

cols <- c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e", "#eec73a",
          "#e29421", "#e29421", "#f05336", "#ce472e")
t <- max(df_plot_trans$transition_probability)

ggplot(df_plot_trans, aes(y = channel_from, x = channel_to, fill = transition_probability)) +
  theme_minimal() +
  geom_tile(colour = "white", width = .9, height = .9) +
  scale_fill_gradientn(colours = cols, limits = c(0, t),
                       breaks = seq(0, t, by = t/4),
                       labels = c("0", round(t/4*1, 2), round(t/4*2, 2), round(t/4*3, 2), round(t/4*4, 2)),
                       guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
  geom_text(aes(label = round(transition_probability, 2)), fontface = "bold", size = 4) +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 24, face = "bold"),
        axis.title.y = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain")) +
  ggtitle("Transition matrix heatmap")

# models comparison
all_mod_plot <- melt(all_models, id.vars = 'channel_name', variable.name = 'conv_type')
all_mod_plot$value <- round(all_mod_plot$value)
# slope chart
pal <- colorRampPalette(brewer.pal(10, "Set1"))
ggplot(all_mod_plot, aes(x = conv_type, y = value, group = channel_name)) +
  theme_solarized(base_size = 18, base_family = "", light = TRUE) +
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  geom_line(aes(color = channel_name), size = 2.5, alpha = 0.8) +
  geom_point(aes(color = channel_name), size = 5) +
  geom_label_repel(aes(label = paste0(channel_name, ': ', value), fill = factor(channel_name)),
                   alpha = 0.7,
                   fontface = 'bold', color = 'white', size = 5,
                   box.padding = unit(0.25, 'lines'), point.padding = unit(0.5, 'lines'),
                   max.iter = 100) +
  theme(legend.position = 'none',
        legend.title = element_text(size = 16, color = 'black'),
        legend.text = element_text(size = 16, vjust = 2, color = 'black'),
        plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 24, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 16, face = "bold", color = 'black'),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 16, hjust = 0.5, vjust = 0.5, face = "bold", color = 'black'),
        strip.background = element_rect(fill = "#f0b35f")) +
  labs(x = 'Model', y = 'Conversions') +
  ggtitle('Models comparison') +
  guides(colour = guide_legend(override.aes = list(size = 4)))

####  ggplot normal #####

# Visuel des models par Channel propre
ggplot(all_mod_plot) +
  aes(x = conv_type, fill = channel_name, weight = value) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal()

# NB first touch par channel
ggplot(all_models) +
  aes(x = channel_name, fill = channel_name, weight = first_touch_conversions) +
  geom_bar() +
  labs(title = "first touch par channel") +
  scale_fill_brewer(palette = "YlGn") +
  theme_light()

# NB last touch par channel
ggplot(all_models) +
  aes(x = channel_name, fill = channel_name, weight = last_touch_conversions) +
  geom_bar() +
  labs(title = "last touch par channel") +
  scale_fill_brewer(palette = "YlGn") +
  theme_light()

# CA par channel FT !!!!!!!!!
ggplot(df_ft2) +
  aes(x = channel_name_ft, weight = Amount_ft) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Channel", y = "CA", title = "CA par Channel") +
  theme_light()

#### Divers Calculs ####

# Compte des nouveaux clients et clients fidele
Compte  <- df_merge2 %>%
  select(conv)%>%
  count(conv)

# Compte le nombre d'occurence de chaque valeurs de Amount
CA_n <-csvAll3 %>%
  select(Amount)%>%
  filter(Amount!="") %>%
  count(Amount)

# compte la somme de tout les Amount ( donc CA global ?)
CA_total <-csvAll3 %>%
  select(Amount)%>%
  filter(Amount!="") %>%
  summarize(sum(Amount))

# Amount pour le Channel gagnant et par la source gagnante par ORder.ID ( donc chaque vente )
CA_win <- csvAll3 %>% select(Amount, Winner.Channel..one.touch., Winner.Source..one.touch., Order.ID) %>% group_by(Winner.Channel..one.touch., Order.ID, Amount) %>%
  filter(Amount!="")

# Nombe d'occurence de l'association du channel gagnant et de la source gagnante
CA_nb <- csvAll3 %>% select(Amount, Winner.Channel..one.touch., Winner.Source..one.touch., Order.ID) %>% group_by(Winner.Channel..one.touch., Winner.Source..one.touch.) %>%
  filter(Amount!="") %>% count(Winner.Channel..one.touch.,Winner.Source..one.touch.)

# NB occurence Winner Channel one touch & Winner Source one touch
CA_nb_win <- csvAll3 %>% select(Amount, Winner.Channel..one.touch., Winner.Source..one.touch., Order.ID) %>% group_by(Amount,Winner.Channel..one.touch., Winner.Source..one.touch.) %>%
  filter(Amount!="" & Winner.Channel..one.touch.!="" & Winner.Source..one.touch.!="") %>% count(Winner.Channel..one.touch.,Winner.Source..one.touch.,Amount)

# Amount par association Channel/Source one touch
CA_nb_win2 <- csvAll3 %>% select(Amount, Winner.Channel..one.touch., Winner.Source..one.touch., Order.ID) %>% group_by(Winner.Channel..one.touch., Winner.Source..one.touch.) %>%
  filter(Amount!="" & Winner.Channel..one.touch.!="" & Winner.Source..one.touch.!="") %>% summarise(total_Amount=sum(Amount)) 

# nombre d'occurence de chaque path...
CA_n_path <- df_hm2 %>% select(Amount, path, conv) %>% group_by(path) %>% count(path)

# nombre de chemins different total pour les nouveaux clients
CA_n_path_t <- data.frame(sum(CA_n_path$n))

# Amount de chaque path
CA_amount_path <- df_hm2 %>% select(Amount, path, conv) %>% group_by(path) %>%
  summarise(total_Amount=sum(Amount))

#### Details par Channel pour les FT ####


#### SEM FT

# Nombre de chemins qui ont une conversion commencant par SEM ( SEM Brand dedans...)
CA_sem_first <- df_merge3[grep("^(SEM)", df_merge3$path), ] %>% filter(conv==1)
# Total CA quand SEM en first touch
CA_sem_first_amount <- CA_sem_first %>% select(Amount, path, conv)%>%summarise(total_Amount=sum(Amount))
# Nombre de Path similaire commencant par SEM menant a une conv
CA_sem_first_amount2 <- CA_sem_first %>% select(Amount, path, conv)%>%
  group_by(path)%>%
  summarise(total_Amount=sum(Amount))
# test 2
CA_sem_first_amount2 <- CA_sem_first %>% select(Amount, path, conv)%>%
  group_by(path)%>%
  count
# total des chemins commencant par le channel SEM qui convertissent
CA_sem_first_2 <- df_merge3[grep("^(SEM)", df_merge3$path), ] %>% filter(conv==1)%>%count(conv)

#### SEMBrand FT

# tout les chemin qui commence par SEM et qui convertisse
CA_SEMBrand_first <- df_merge3[grep("^(SEM Brand)", df_merge3$path), ] %>% filter(conv==1)
# Amount quand SEM en first touch
CA_SEMBrand_first_amount <- CA_SEMBrand_first %>% select(Amount, path, conv)%>%summarise(total_Amount=sum(Amount))
# NB de path different pour SEM Brand
CA_SEMBrand_first_amount2 <- CA_SEMBrand_first %>% select(Amount, path, conv)%>%count(path)
# count quand SEM est en first touch
CA_SEMBrand_first_2 <- df_merge3[grep("^(SEM Brand)", df_merge3$path), ] %>% filter(conv==1)%>%count(conv)
# mis avec le SEM seul pour l'instant...

#### Afiiliation FT

# Df des chemins qui ont une conversion commencant par Affiliation
CA_Affiliation_first <- df_merge3[grep("^(Affiliation)", df_merge3$path), ] %>% filter(conv==1)
# Amount quand SEM en first touch
CA_Affiliation_first_amount <- CA_Affiliation_first %>% select(Amount, path, conv)%>%summarise(total_Amount=sum(Amount))
# Nombre de Path similaire commencant par Affiliation menant a une conv
CA_Affiliation_first_amount2 <- CA_Affiliation_first %>% select(Amount, path, conv)%>%
  group_by(path)%>%
  summarise(total_Amount=sum(Amount))
# count quand SEM est en first touch
CA_Affiliation_first_2 <- df_merge3[grep("^(Affiliation)", df_merge3$path), ] %>% filter(conv==1)%>%count(conv)

#### DirectAcces FT

# Df des chemins qui ont une conversion commencant par DirectAcces
CA_DirectAcces_first <- df_merge3[grep("^(Direct)", df_merge3$path), ] %>% filter(conv==1)
# Amount quand SEM en first touch
CA_DirectAcces_first_amount <- CA_DirectAcces_first %>% select(Amount, path, conv)%>%summarise(total_Amount=sum(Amount))
# Nombre de Path similaire commencant par Direct Access menant a une conv
CA_DirectAcces_first_amount2 <- CA_DirectAcces_first %>% select(Amount, path, conv)%>%
  group_by(path)%>%
  summarise(total_Amount=sum(Amount))
# count quand SEM est en first touch
CA_DirectAcces_first_2 <- df_merge3[grep("^(Direct)", df_merge3$path), ] %>% filter(conv==1)%>%count(conv)

#### E-mail FT

# tout les chemin qui commence par SEM et qui convertisse
CA_E_mail_first <- df_merge3[grep("^(E-mail)", df_merge3$path), ] %>% filter(conv==1)
# Amount quand SEM en first touch
CA_E_mail_first_amount <- CA_E_mail_first %>% select(Amount, path, conv)%>%summarise(total_Amount=sum(Amount))
# Nombre de Path similaire commencant par E-mail menant a une conv
CA_E_mail_first_amount2 <- CA_E_mail_first %>% select(Amount, path, conv)%>%
  group_by(path)%>%
  summarise(total_Amount=sum(Amount))
# count quand SEM est en first touch
CA_E_mail_first_2 <- df_merge3[grep("^(E-mail)", df_merge3$path), ] %>% filter(conv==1)%>%count(conv)

#### Referrer FT

# tout les chemin qui commence par SEM et qui convertisse
CA_Referrer_first <- df_merge3[grep("^(Referrer)", df_merge3$path), ] %>% filter(conv==1)
# Amount quand SEM en first touch
CA_Referrer_first_amount <- CA_Referrer_first %>% select(Amount, path, conv)%>%summarise(total_Amount=sum(Amount))
# Nombre de Path similaire commencant par Reffer menant a une conv
CA_Referrer_first_amount2 <- CA_Referrer_first %>% select(Amount, path, conv)%>%
  group_by(path)%>%
  summarise(total_Amount=sum(Amount))
# count quand SEM est en first touch
CA_Referrer_first_2 <- df_merge3[grep("^(Referrer)", df_merge3$path), ] %>% filter(conv==1)%>%count(conv)

#### 	Retargeting FT

# tout les chemin qui commence par SEM et qui convertisse
CA_Retargeting_first <- df_merge3[grep("^(Retargeting)", df_merge3$path), ] %>% filter(conv==1)
# Amount quand SEM en first touch
CA_Retargeting_first_amount <- CA_Retargeting_first %>% select(Amount, path, conv)%>%summarise(total_Amount=sum(Amount))
# Nombre de Path similaire commencant par Retargeting menant a une conv
CA_Retargeting_first_amount <- CA_Retargeting_first %>% select(Amount, path, conv)%>%
  group_by(path)%>%
  summarise(total_Amount=sum(Amount))
# count quand SEM est en first touch
CA_Retargeting_first_2 <- df_merge3[grep("^(Retargeting)", df_merge3$path), ] %>% filter(conv==1)%>%count(conv)

#### SEO FT

# tout les chemin qui commence par SEM et qui convertisse
CA_SEO_first <- df_merge3[grep("^(SEO)", df_merge3$path), ] %>% filter(conv==1)
# Amount quand SEM en first touch
CA_SEO_first_amount <- CA_SEO_first %>% select(Amount, path, conv)%>%summarise(total_Amount=sum(Amount))
# Nombre de Path similaire commencant par SEO menant a une conv
CA_SEO_first_amount <- CA_SEO_first %>% select(Amount, path, conv)%>%
  group_by(path)%>%
  summarise(total_Amount=sum(Amount))
# count quand SEM est en first touch
CA_SEO_first_2 <- df_merge3[grep("^(SEO)", df_merge3$path), ] %>% filter(conv==1)%>%count(conv)


#### SocialAds FT

# tout les chemin qui commence par SEM et qui convertisse
CA_SocialAds_first <- df_merge3[grep("^(Social)", df_merge3$path), ] %>% filter(conv==1)
# Amount quand SEM en first touch
CA_SocialAds_first_amount <- CA_SocialAds_first %>% select(Amount, path, conv)%>%summarise(total_Amount=sum(Amount))
# Nombre de Path similaire commencant par Social Ads menant a une conv
CA_SocialAds_first_amount <- CA_SocialAds_first %>% select(Amount, path, conv)%>%
  group_by(path)%>%
  summarise(total_Amount=sum(Amount))
# count quand SEM est en first touch
CA_SocialAds_first_2 <- df_merge3[grep("^(Social)", df_merge3$path), ] %>% filter(conv==1)%>%count(conv)

#### Bilan test ####

bilan <- data.frame(Channel = c("SEM Brand", "SEM", "Affiliation", "Direct Access", "E-Mail", "Refferer", "Retargeting", "SEO", "Social Ads"),
                    FT_Total = c("651","1852", "187", "822", "259", "1165", "1093", "1728", "74"),
                    FT_by_Path = c("349", "515","142", "328", "220", "648", "810", "549", "35"),
                    Amount_by_channel = c("67421.65", "158661.23", "18185.42", "81422.73", "22259.38", "110434.77", "101813.22", "159486","7039.67"),
                    CA_Moyen_par_FT_du_FT_Total = c("103.57", "85.67", "97.25", "99.06", "85.94", "94.79", "93.15", "92.29", "95.13"),
                    Ratio_NbPath_pour_100_FT = c ("53.61", "27.81", "75.94", "39.9", "84.94", "55.62", "74.11", "31.88", "47.3"), stringsAsFactors = FALSE)


bilan$FT_Total <- as.numeric(as.character(bilan$FT_Total))
bilan$FT_by_Path <- as.numeric(as.character(bilan$FT_by_Path))
bilan$Amount_by_channel <- as.numeric(as.character(bilan$Amount_by_channel))
bilan$CA_Moyen_par_FT_du_FT_Total <- as.numeric(as.character(bilan$CA_Moyen_par_FT_du_FT_Total))
bilan$Ratio_NbPath_pour_100_FT <- as.numeric(as.character(bilan$Ratio_NbPath_pour_100_FT))

#### Bilan intermediaire ####

Bilan_detail <- merge(df_ft2, df_lt2, by.x = 'channel_name_ft', by.y = 'channel_name_lt')
Bilan_detail <- Bilan_detail%>%rename(Channel = channel_name_ft)

#### ggplot selon bilan ####

# CA / Channel avec indice de FT par path
ggplot(bilan) +
  aes(x = Channel, fill = FT_by_Path, weight = Amount_by_channel) +
  geom_bar() +
  scale_fill_distiller(palette = "RdBu") +
  labs(y = "CA généré", fill = "First Touch par Path") +
  theme_minimal()

# Nombre de path sur une base de 100FT par channel
ggplot(bilan) +
  aes(x = Channel, fill = Ratio_NbPath_pour_100_FT, weight = Ratio_NbPath_pour_100_FT) +
  geom_bar() +
  scale_fill_distiller(palette = "RdBu") +
  labs(fill = "Nb path pour 100 FT") +
  theme_minimal()
# CA moyen par channel selon FT total
ggplot(bilan) +
  aes(x = Channel, fill = FT_Total, weight = CA_Moyen_par_FT_du_FT_Total) +
  geom_bar() +
  scale_fill_distiller(palette = "RdYlBu") +
  labs(y = "CA moyen par FT du Channel", fill = "FT par Channel") +
  coord_flip() +
  theme_minimal()

##### Bilan intermediaire 2 ####

Bilan_detail2 <- Bilan_detail%>%mutate(CA_moyen_ft=Amount_ft/first_touch_conversions)
Bilan_detail2 <- Bilan_detail2[, c(1,2,3,6,4,5)]
Bilan_detail_ft_lt_mid <- Bilan_detail2%>%mutate(CA_moyen_lt=Amount_lt/last_touch_conversions)


Bilan_detail_ft_lt_mid$Channel <- replace(Bilan_detail_ft_lt_mid$Channel, Bilan_detail_ft_lt_mid$Channel == "E-mail", "E_mail")
Bilan_detail_ft_lt_mid$Channel <- replace(Bilan_detail_ft_lt_mid$Channel, Bilan_detail_ft_lt_mid$Channel == "Direct Access", "Direct_Access")
Bilan_detail_ft_lt_mid$Channel <- replace(Bilan_detail_ft_lt_mid$Channel, Bilan_detail_ft_lt_mid$Channel == "Social Ads", "Social_Ads")
Bilan_detail_ft_lt_mid$Channel <- replace(Bilan_detail_ft_lt_mid$Channel, Bilan_detail_ft_lt_mid$Channel == "SEM Brand", "SEM_Brand")

#### Changement de nom de df pour la suite ####
df42 <- df_merge3

##### Calcul des Channels generant le moins de click au FIRST click ####
t<-""
for (i in df42$path){
  a=lapply(strsplit(i," > ",fixed = TRUE),as.list)
  taille=length(a[[1]])-1
  for (elem in listeChannel){
    if (grepl(elem,a[[1]][[1]])){t=paste(t,elem," | ",taille," > ")}
  }
}
d=lapply(strsplit(t," > ",fixed= TRUE),as.list)
df <- as.data.frame(t(data.frame(matrix(unlist(d), nrow=length(d), byrow=T))),stringsAsFactors = FALSE)
out <- strsplit(as.character(df$V1),' | ')
df <- data.frame(matrix(unlist(out), nrow=length(out), byrow=T),stringsAsFactors = FALSE)

dfChannelFT <- df%>%select(X2,X6) %>% group_by(X2) %>% count(X2)
dfChannelFT$n[8]<-dfChannelFT$n[8]-dfChannelFT$n[3]
dfPointFT <- df%>%select(X2,X6) %>% group_by(X2) %>% summarise(nbPointFT=sum(as.numeric(X6)))
dfPointFT$nbPointFT[8]<-dfPointFT$nbPointFT[8]-dfPointFT$nbPointFT[3]

##### Calcul des Channels generant le moins de click au LAST click ####
t<-""
for (i in df42$path){
  a=lapply(strsplit(i," > ",fixed = TRUE),as.list)
  taille=length(a[[1]])
  for (elem in listeChannel){
    if (grepl(elem,a[[1]][[taille]])){t=paste(t,elem," | ",taille-1," > ")}
  }
}
d=lapply(strsplit(t," > ",fixed= TRUE),as.list)
df <- as.data.frame(t(data.frame(matrix(unlist(d), nrow=length(d), byrow=T))),stringsAsFactors = FALSE)
out <- strsplit(as.character(df$V1),' | ')
df <- data.frame(matrix(unlist(out), nrow=length(out), byrow=T),stringsAsFactors = FALSE)

dfChannelLT <- df%>%select(X2,X6) %>% group_by(X2) %>% count(X2)
dfChannelLT$n[8]<-dfChannelLT$n[8]-dfChannelLT$n[3]
dfPointLT <- df%>%select(X2,X6) %>% group_by(X2) %>% summarise(nbPointLT=sum(as.numeric(X6)))
dfPointLT$nbPointLT[8]<-dfPointLT$nbPointLT[8]-dfPointLT$nbPointLT[3]

##### Join des 3 DF (CA,FT,LT) ####
dfJoinLT<-left_join(dfChannelLT,dfPointLT, by="X2")
dfJoinLT<-dfJoinLT %>% mutate(ratioPointParChannelLT=nbPointLT/n) %>% rename(Channel=X2,nbChannelLT=n)

dfJoinFT<-left_join(dfChannelFT,dfPointFT, by="X2")
dfJoinFT<-dfJoinFT %>% mutate(ratioPointParChannelFT=nbPointFT/n) %>% rename(Channel=X2,nbChannelFT=n)

dfJoinCa<-left_join(dfJoinLT,dfJoinFT, by="Channel")
dfJoinCa<-left_join(dfJoinCa,dfCA,by="Channel")

dfHM<-dfJoinCa%>%mutate(ratioFtLt=ratioPointParChannelLT+ratioPointParChannelFT)
dfHM <- dfHM[, c(1,8, 5,2,6,3,7,4,9)]

##### FIRST CLICK - 2eme partie ####
affil=0
affilNb=0
referrer=0
referrerNb=0
sem=0
semNb=0
seo=0
seoNb=0
mail=0
mailNb=0
retar=0
retarNb=0
direct=0
directNb=0
ads=0
adsNb=0
semBrand=0
semBrandNb=0
error=0
c2=0

for (i in df42$path){
  a=lapply(strsplit(i," > ",fixed = TRUE),as.list)
  c2=c2+1
  if (grepl("Ads",a[[1]][[1]])){
    ads=ads+df42$Amount[c2]
    adsNb=adsNb+1
  }
  else if (grepl("Brand",a[[1]][[1]])){
    semBrand=semBrand+df42$Amount[c2]
    semBrandNb=semBrandNb+1
  }
  else if (grepl("SEM",a[[1]][[1]])){
    sem=sem+df42$Amount[c2]
    semNb=semNb+1
  }
  else if (grepl("SEO",a[[1]][[1]])){
    seo=seo+df42$Amount[c2]
    seoNb=seoNb+1
  }
  else if (grepl("Affiliation",a[[1]][[1]])){
    affil=affil+df42$Amount[c2]
    affilNb=affilNb+1
  }
  else if (grepl("Direct",a[[1]][[1]])){
    direct=direct+df42$Amount[c2]
    directNb=directNb+1
  }
  else if (grepl("mail",a[[1]][[1]])){
    mail=mail+df42$Amount[c2]
    mailNb=mailNb+1
  }
  else if (grepl("Referrer",a[[1]][[1]])){
    referrer=referrer+df42$Amount[c2]
    referrerNb=referrerNb+1
  }
  else if (grepl("Retar",a[[1]][[1]])){
    retar=retar+df42$Amount[c2]
    retarNb=retarNb+1
  }
  else{error=error+1
  print(elem)}
}
# Transformation du resultat en Dataframe
dfCA <- data.frame(Affiliation=affil,Referrer=referrer,SEM=sem,SEO=seo,
                   E_mail=mail,Retargeting=retar,Direct_Acess=direct,Social_Ads=ads,
                   SEM_Brand=semBrand)
# Transposition de la DF
dfCAFt <- as.data.frame(t(dfCA))
dfCAFt <- data.frame(Channel = row.names(dfCAFt), CaChannelFT=dfCAFt$V1)

dfNbFt<- data.frame(Affiliation=affilNb,Referrer=referrerNb,SEM=semNb,SEO=seoNb,
                    E_mail=mailNb,Retargeting=retarNb,Direct_Acess=directNb,Social_Ads=adsNb,
                    SEM_Brand=semBrandNb)
dfNbFt <- as.data.frame(t(dfNbFt))
dfNbFt <- data.frame(Channel = row.names(dfNbFt), nbFT=dfNbFt$V1)
dfFT <- left_join(dfNbFt,dfCAFt, by="Channel")
dfFT <- dfFT%>%mutate(pmttcFT=CaChannelFT/nbFT)
##### LAST CLICK - 2eme partie ####
affil=0
affilNb=0
referrer=0
referrerNb=0
sem=0
semNb=0
seo=0
seoNb=0
mail=0
mailNb=0
retar=0
retarNb=0
direct=0
directNb=0
ads=0
adsNb=0
semBrand=0
semBrandNb=0
error=0
c2=0
for (i in df42$path){
  a=lapply(strsplit(i," > ",fixed = TRUE),as.list)
  taille=length(a[[1]])
  c2=c2+1
  if (grepl("Ads",a[[1]][[taille]])){
    ads=ads+df42$Amount[c2]
    adsNb=adsNb+1
  }
  else if (grepl("Brand",a[[1]][[taille]])){
    semBrand=semBrand+df42$Amount[c2]
    semBrandNb=semBrandNb+1
  }
  else if (grepl("SEM",a[[1]][[taille]])){
    sem=sem+df42$Amount[c2]
    semNb=semNb+1
  }
  else if (grepl("SEO",a[[1]][[taille]])){
    seo=seo+df42$Amount[c2]
    seoNb=seoNb+1
  }
  else if (grepl("Affiliation",a[[1]][[taille]])){
    affil=affil+df42$Amount[c2]
    affilNb=affilNb+1
  }
  else if (grepl("Direct",a[[1]][[taille]])){
    direct=direct+df42$Amount[c2]
    directNb=directNb+1
  }
  else if (grepl("mail",a[[1]][[taille]])){
    mail=mail+df42$Amount[c2]
    mailNb=mailNb+1
  }
  else if (grepl("Referrer",a[[1]][[taille]])){
    referrer=referrer+df42$Amount[c2]
    referrerNb=referrerNb+1
  }
  else if (grepl("Retar",a[[1]][[taille]])){
    retar=retar+df42$Amount[c2]
    retarNb=retarNb+1
  }
  else{error=error+1
  print(elem)}
}
# Transformation du resultat en Dataframe
dfCA <- data.frame(Affiliation=affil,Referrer=referrer,SEM=sem,SEO=seo,
                   E_mail=mail,Retargeting=retar,Direct_Acess=direct,Social_Ads=ads,
                   SEM_Brand=semBrand)
# Transposition de la DF
dfCALt <- as.data.frame(t(dfCA))
dfCALt <- data.frame(Channel = row.names(dfCALt), CaChannelLT=dfCALt$V1)

dfNbLt<- data.frame(Affiliation=affilNb,Referrer=referrerNb,SEM=semNb,SEO=seoNb,
                    E_mail=mailNb,Retargeting=retarNb,Direct_Acess=directNb,Social_Ads=adsNb,
                    SEM_Brand=semBrandNb)
dfNbLt <- as.data.frame(t(dfNbLt))
dfNbLt <- data.frame(Channel = row.names(dfNbLt), nbLT=dfNbLt$V1)
dfLT <- left_join(dfNbLt,dfCALt, by="Channel")
dfLT <- dfLT%>%mutate(pmttcLT=CaChannelLT/nbLT)
##### MID CLICK 2 - pour le total de leurs representation dans les path ####
affil=0
affilNb=0
referrer=0
referrerNb=0
sem=0
semNb=0
seo=0
seoNb=0
mail=0
mailNb=0
retar=0
retarNb=0
direct=0
directNb=0
ads=0
adsNb=0
semBrand=0
semBrandNb=0
error=0
c2=0
for (i in df42$path){
  a=lapply(strsplit(i," > ",fixed = TRUE),as.list)
  taille=length(a[[1]])
  c2=c2+1
  c=0
  for (elem in a[[1]]){
    c=c+1
    if (c!=1 & c!=taille){
      if (grepl("Ads",a[[1]][[c]])){
        ads=ads+df42$Amount[c2]
        adsNb=adsNb+1
      }
      else if (grepl("Brand",a[[1]][[c]])){
        semBrand=semBrand+df42$Amount[c2]
        semBrandNb=semBrandNb+1
      }
      else if (grepl("SEM",a[[1]][[c]])){
        sem=sem+df42$Amount[c2]
        semNb=semNb+1
      }
      else if (grepl("SEO",a[[1]][[c]])){
        seo=seo+df42$Amount[c2]
        seoNb=seoNb+1
      }
      else if (grepl("Affiliation",a[[1]][[c]])){
        affil=affil+df42$Amount[c2]
        affilNb=affilNb+1
      }
      else if (grepl("Direct",a[[1]][[c]])){
        direct=direct+df42$Amount[c2]
        directNb=directNb+1
      }
      else if (grepl("mail",a[[1]][[c]])){
        mail=mail+df42$Amount[c2]
        mailNb=mailNb+1
      }
      else if (grepl("Referrer",a[[1]][[c]])){
        referrer=referrer+df42$Amount[c2]
        referrerNb=referrerNb+1
      }
      else if (grepl("Retar",a[[1]][[c]])){
        retar=retar+df42$Amount[c2]
        retarNb=retarNb+1
      }
      else{error=error+1
      print(elem)}
    }
  }
}
# Transformation du resultat en Dataframe
dfCA <- data.frame(Affiliation=affil,Referrer=referrer,SEM=sem,SEO=seo,
                   E_mail=mail,Retargeting=retar,Direct_Acess=direct,Social_Ads=ads,
                   SEM_Brand=semBrand)
# Transposition de la DF
dfCAFt <- as.data.frame(t(dfCA))
dfCAFt <- data.frame(Channel = row.names(dfCAFt), CaChannelMid2=dfCAFt$V1)

dfNbFt<- data.frame(Affiliation=affilNb,Referrer=referrerNb,SEM=semNb,SEO=seoNb,
                    E_mail=mailNb,Retargeting=retarNb,Direct_Acess=directNb,Social_Ads=adsNb,
                    SEM_Brand=semBrandNb)
dfNbFt <- as.data.frame(t(dfNbFt))
dfNbFt <- data.frame(Channel = row.names(dfNbFt), nbMid2=dfNbFt$V1)
dfMid2 <- left_join(dfNbFt,dfCAFt, by="Channel")
dfMid2 <- dfMid2%>%mutate(pmttcMid2=CaChannelMid2/nbMid2)
##### MID CLICK 1 - pour une unique occurence dans le path ####
affil=0
affilNb=0
referrer=0
referrerNb=0
sem=0
semNb=0
seo=0
seoNb=0
mail=0
mailNb=0
retar=0
retarNb=0
direct=0
directNb=0
ads=0
adsNb=0
semBrand=0
semBrandNb=0
error=0
c2=0

for (i in df42$path){
  a=lapply(strsplit(i," > ",fixed = TRUE),as.list)
  taille=length(a[[1]])
  mid=1
  if (taille>2){
    mid=mid/(taille-2)}
  c=0
  c2=c2+1
  for (elem in a[[1]]){
    c=c+1
    if (c!=1 & c!=taille){
      if(taille>2 & c!=1 & c!=taille){
        if (grepl("Ads",elem)){
          ads=ads+mid*df42$Amount[c2]
          adsNb=adsNb+1
        }
        else if (grepl("Brand",elem)){
          semBrand=semBrand+mid*df42$Amount[c2]
          semBrandNb=semBrandNb+1
        }
        else if (grepl("SEM",elem)){
          sem=sem+mid*df42$Amount[c2]
          semNb=semNb+1
        }
        else if (grepl("SEO",elem)){
          seo=seo+mid*df42$Amount[c2]
          seoNb=seoNb+1
        }
        else if (grepl("Affiliation",elem)){
          affil=affil+mid*df42$Amount[c2]
          affilNb=affilNb+1
        }
        else if (grepl("Direct",elem)){
          direct=direct+mid*df42$Amount[c2]
          directNb=directNb+1
        }
        else if (grepl("mail",elem)){
          mail=mail+mid*df42$Amount[c2]
          mailNb=mailNb+1
        }
        else if (grepl("Referrer",elem)){
          referrer=referrer+mid*df42$Amount[c2]
          referrerNb=referrerNb+1
        }
        else if (grepl("Retar",elem)){
          retar=retar+mid*df42$Amount[c2]
          retarNb=retarNb+1}
      }
    }
  }
}
dfCA <- data.frame(Affiliation=affil,Referrer=referrer,SEM=sem,SEO=seo,
                   E_mail=mail,Retargeting=retar,Direct_Acess=direct,Social_Ads=ads,
                   SEM_Brand=semBrand)
# Transposition de la DF
dfCAFt <- as.data.frame(t(dfCA))
dfCAFt <- data.frame(Channel = row.names(dfCAFt), CaChannelMid=dfCAFt$V1)

dfNbFt<- data.frame(Affiliation=affilNb,Referrer=referrerNb,SEM=semNb,SEO=seoNb,
                    E_mail=mailNb,Retargeting=retarNb,Direct_Acess=directNb,Social_Ads=adsNb,
                    SEM_Brand=semBrandNb)

dfNbFt <- as.data.frame(t(dfNbFt))
dfNbFt <- data.frame(Channel = row.names(dfNbFt), nbMid=dfNbFt$V1)
dfMid <- left_join(dfNbFt,dfCAFt, by="Channel")
dfMid <- dfMid%>%mutate(pmttcMid=CaChannelMid/nbMid)

##### Affinite // enlever le 6 si probleme ####
model6 <- markov_model(df42,
                       var_path = 'path',
                       var_conv = 'conv',
                       var_null = 'conv_null',                    
                       out_more = TRUE)

df_plot_trans6 <- model6$transition_matrix
df_plot_trans6 <- df_plot_trans6 %>%filter(channel_from!="Other")%>%
  filter(channel_to!="Other")%>%filter(channel_to!="(null)")

dfAffil<-df_plot_trans6 %>% filter(channel_from=="Affiliation" & channel_to!="(conversion)")%>%
  arrange(channel_to)%>%select(channel_to,transition_probability)
affil=""
for (i in dfAffil){
  affil=paste(affil,i)}

dfRefer<-df_plot_trans %>% filter(channel_from=="Referrer" & channel_to!="(conversion)")%>%
  arrange(channel_to)%>%select(channel_to,transition_probability)
refer=""
for (i in dfRefer){
  refer=paste(refer,i)}

dfSem<-df_plot_trans %>% filter(channel_from=="SEM" & channel_to!="(conversion)")%>%
  arrange(channel_to)%>%select(channel_to,transition_probability)
sem=""
for (i in dfSem){
  sem=paste(sem,i)}

dfSeo<-df_plot_trans %>% filter(channel_from=="SEO" & channel_to!="(conversion)")%>%
  arrange(channel_to)%>%select(channel_to,transition_probability)
seo=""
for (i in dfSeo){
  seo=paste(seo,i)}

dfBrand<-df_plot_trans %>% filter(channel_from=="SEM Brand" & channel_to!="(conversion)")%>%
  arrange(channel_to)%>%select(channel_to,transition_probability)
brand=""
for (i in dfBrand){
  brand=paste(brand,i)}

dfAds<-df_plot_trans %>% filter(channel_from=="Social Ads" & channel_to!="(conversion)")%>%
  arrange(channel_to)%>%select(channel_to,transition_probability)
ads=""
for (i in dfAds){
  ads=paste(ads,i)}

dfRetar<-df_plot_trans %>% filter(channel_from=="Retargeting" & channel_to!="(conversion)")%>%
  arrange(channel_to)%>%select(channel_to,transition_probability)
retar=""
for (i in dfRetar){
  retar=paste(retar,i)}

dfMail<-df_plot_trans %>% filter(channel_from=="E-mail" & channel_to!="(conversion)")%>%
  arrange(channel_to)%>%select(channel_to,transition_probability)
mail=""
for (i in dfMail){
  mail=paste(mail,i,sep = ":")}

dfDirect<-df_plot_trans %>% filter(channel_from=="Direct Access" & channel_to!="(conversion)")%>%
  arrange(channel_to)%>%select(channel_to,transition_probability)
direct=""
for (i in dfDirect){
  direct=paste(direct,i)}



dfAffinite <- data.frame(Affiliation=affil,Referrer=refer,SEM=sem,SEO=seo,
                         E_mail=mail,Retargeting=retar,Direct_Acess=direct,Social_Ads=ads,
                         SEM_Brand=brand)
# Transposition de la DF
dfAffinite <- as.data.frame(t(dfAffinite))
dfAffinite$test<-paste(dfAffinite$V1,"|",dfAffinite$V2,"|",dfAffinite$V3,"|",dfAffinite$V4,"|",dfAffinite$V5,"|",dfAffinite$V6,"|",dfAffinite$V7,"|",dfAffinite$V7,"|",dfAffinite$V8,"|",dfAffinite$V9)
dfAffinite <- data.frame(Channel = row.names(dfAffinite), Affinite=dfAffinite$test)

dfTotal <- left_join(dfFT,dfLT, by="Channel")
dfTotal <- left_join(dfTotal,dfMid, by="Channel")
dfTotal <- left_join(dfTotal,dfMid2, by="Channel")
dfTotal <- left_join(dfTotal,dfAffinite,by="Channel")

dfTotal$Channel <- as.character(dfTotal$Channel)

dfTotal$Channel[dfTotal$Channel=='Direct_Acess'] <- "Direct_Access"


##### Bilan Complet  ####

dfTotal2 <- dfTotal %>% select(Channel,CaChannelMid,pmttcMid, CaChannelMid2, pmttcMid2, Affinite)
dfTotal2$Channel <- as.character(dfTotal2$Channel)
dfTotal2$Channel <-  replace(dfTotal2$Channel, dfTotal2$Channel == "Direct Access", "Direct_Access")

BILAN2 <- left_join(Bilan_detail_ft_lt_mid,dfTotal2, by='Channel', stringsAsFactors = FALSE)
BILAN2 <- BILAN2%>% rename(Amount_mid=CaChannelMid, Amount_mid2=CaChannelMid2, CA_moyen_mid=pmttcMid, CA_moyen_mid2=pmttcMid2)


##### Bilan Time tab ####

timeTab<-csvAll3%>%select(Touch.points,Time.to.convert,New.Customer)%>%filter(Time.to.convert!="" & New.Customer==1 & Touch.points!=0)

# timeTab$Time.to.convert=as.numeric(timeTab$Time.to.convert)
# 
# timeTab<-timeTab%>% select(Time.to.convert,Touch.points)%>%group_by(Time.to.convert)%>%
#   count(Time.to.convert)%>%arrange(Time.to.convert)



c=0
for (i in timeTab$Time.to.convert){
  c=c+1
  if(grepl("d",i)){
    d=str_locate(i,"d")
    timeTab$Time.to.convert[c]=str_sub(timeTab$Time.to.convert[c],1,d-1)}
  else{
    timeTab$Time.to.convert[c]="0"}}

timeTab$Time.to.convert=as.numeric(timeTab$Time.to.convert)
time_sum<-timeTab%>% select(Time.to.convert,Touch.points)%>%group_by(Time.to.convert)%>%count(Time.to.convert)%>%arrange(Time.to.convert)

time_sum0<-time_sum%>%filter(Time.to.convert==0)%>% ungroup()%>%summarise(sum(n))
time_sum1<-time_sum%>%filter(Time.to.convert==1)%>%ungroup()%>%summarise(sum(n))
time_sum2<-time_sum%>%filter(Time.to.convert==2)%>%ungroup()%>%summarise(sum(n))
time_sum3_5<-time_sum%>%filter(Time.to.convert>2 & Time.to.convert<6)%>%ungroup()%>%summarise(sum(n))
time_sum6_9<-time_sum%>%filter(Time.to.convert>5 & Time.to.convert<10)%>%ungroup()%>%summarise(sum(n))
time_sum10_14<-time_sum%>%filter(Time.to.convert>9 & Time.to.convert<15)%>%ungroup()%>%summarise(sum(n))
time_sum15_19<-time_sum%>%filter(Time.to.convert>14 & Time.to.convert<20)%>%ungroup()%>%summarise(sum(n))
time_sum20_24<-time_sum%>%filter(Time.to.convert>19 & Time.to.convert<25)%>%ungroup()%>%summarise(sum(n))
time_sum25_29<-time_sum%>%filter(Time.to.convert>24 & Time.to.convert<30)%>%ungroup()%>%summarise(sum(n))
time_sum30_more<-time_sum%>%filter(Time.to.convert>29)%>%ungroup()%>%summarise(sum(n))

time_decil<-data.frame(time_sum0,time_sum1,time_sum2,time_sum3_5,time_sum6_9,
                       time_sum10_14,time_sum15_19,time_sum20_24,time_sum25_29,time_sum30_more,
                       stringsAsFactors = FALSE)

time_decil<-time_decil%>%rename(`<1 jour`='sum.n.',`1 jour`='sum.n..1',`2 jours`='sum.n..2',`3-5 jours`='sum.n..3',
                                `6-9 jours`='sum.n..4',`10-14 jours`='sum.n..5',
                                `15-19 jours`='sum.n..6',`20-24 jours`='sum.n..7',
                                `25-29 jours`='sum.n..8',`30+ jours`='sum.n..9')

time_decil <- as.data.frame(t(time_decil),stringsAsFactors = FALSE)

time_sum <- data.frame(jour = row.names(time_decil), sum_time=time_decil$V1,stringsAsFactors = FALSE)

time <- sum(time_sum$sum_time)
time_sum <- time_sum%>%mutate(pourcent_decil=sum_time/time*100)


##### Bilan Touch Tab #####

touchtab<-csvAll3%>%select(Touch.points,Time.to.convert,New.Customer,Order.ID,User.id)%>%
  filter(Time.to.convert!="" & New.Customer==1)

touchtab$Touch.points=as.numeric(touchtab$Touch.points)

touchtab<-touchtab%>% select(Time.to.convert,Touch.points)%>%group_by(Touch.points)%>%
  count(Touch.points)%>%arrange(Touch.points)


touch_sum1<-touchtab%>%filter(Touch.points==1)%>%ungroup()%>%summarise(sum(n))
touch_sum2<-touchtab%>%filter(Touch.points==2)%>%ungroup()%>%summarise(sum(n))
touch_sum3_4<-touchtab%>%filter(Touch.points>2 & Touch.points<5) %>% ungroup()%>%summarise(sum(n))
touch_sum5_6<-touchtab%>%filter(Touch.points>4 & Touch.points<7)%>%ungroup()%>%summarise(sum(n))
touch_sum7_9<-touchtab%>%filter(Touch.points>6 & Touch.points<10)%>%ungroup()%>%summarise(sum(n))
touch_sum10_14<-touchtab%>%filter(Touch.points>9 & Touch.points<15)%>%ungroup()%>%summarise(sum(n))
touch_sum15_19<-touchtab%>%filter(Touch.points>14 & Touch.points<20)%>%ungroup()%>%summarise(sum(n))
touch_sum20_24<-touchtab%>%filter(Touch.points>19 & Touch.points<25)%>%ungroup()%>%summarise(sum(n))
touch_sum25_29<-touchtab%>%filter(Touch.points>24 & Touch.points<30)%>%ungroup()%>%summarise(sum(n))
touch_sum30_more<-touchtab%>%filter(Touch.points>29)%>%ungroup()%>%summarise(sum(n))

touch_decil<-data.frame(touch_sum1,touch_sum2,touch_sum3_4,touch_sum5_6,touch_sum7_9,
                        touch_sum10_14,touch_sum15_19,touch_sum20_24,touch_sum25_29,touch_sum30_more,
                        stringsAsFactors = FALSE)

touch_decil<-touch_decil%>%rename(`1 touch points`='sum.n.',`2 touch points`='sum.n..1',
                                  `3-4 touch points`='sum.n..2',`5-6 touch points`='sum.n..3',
                                  `7-9 touch points`='sum.n..4',`10-14 touch points`='sum.n..5',
                                  `15-19 touch points`='sum.n..6',`20-24 touch points`='sum.n..7',
                                  `25-29 touch points`='sum.n..8',`30+ touch points`='sum.n..9')

touch_decil <- as.data.frame(t(touch_decil),stringsAsFactors = FALSE)

touch_sum <- data.frame(jour = row.names(touch_decil), sum_touch=touch_decil$V1,stringsAsFactors = FALSE)

touch <- sum(touch_sum$sum_touch)
touch_sum <- touch_sum%>%mutate(pourcent_decil_touch=sum_touch/touch*100)
touch_sum <- touch_sum%>%rename(touch=jour)


#### bilan avec 4eme colonne du CA moyen ####  

touchnombre<-csvAll3%>%select(Amount,Touch.points,Time.to.convert,New.Customer,Order.ID,User.id)%>%
  filter(Time.to.convert!="")

touchnombre$Touch.points=as.numeric(touchnombre$Touch.points)

touchnombre<-touchnombre%>% select(Amount,Touch.points)%>%group_by(Touch.points)%>%
  summarise(n=mean(Amount))%>%arrange(Touch.points)


touche_sum1<-touchnombre%>%filter(Touch.points==1)%>%ungroup()%>%summarise(mean(n))
touche_sum2<-touchnombre%>%filter(Touch.points==2)%>%ungroup()%>%summarise(mean(n))
touche_sum3_4<-touchnombre%>%filter(Touch.points>2 & Touch.points<5) %>% ungroup()%>%summarise(mean(n))
touche_sum5_6<-touchnombre%>%filter(Touch.points>4 & Touch.points<7)%>%ungroup()%>%summarise(mean(n))
touche_sum7_9<-touchnombre%>%filter(Touch.points>6 & Touch.points<10)%>%ungroup()%>%summarise(mean(n))
touche_sum10_14<-touchnombre%>%filter(Touch.points>9 & Touch.points<15)%>%ungroup()%>%summarise(mean(n))
touche_sum15_19<-touchnombre%>%filter(Touch.points>14 & Touch.points<20)%>%ungroup()%>%summarise(mean(n))
touche_sum20_24<-touchnombre%>%filter(Touch.points>19 & Touch.points<25)%>%ungroup()%>%summarise(mean(n))
touche_sum25_29<-touchnombre%>%filter(Touch.points>24 & Touch.points<30)%>%ungroup()%>%summarise(mean(n))
touche_sum30_more<-touchnombre%>%filter(Touch.points>29)%>%ungroup()%>%summarise(mean(n))

touche_decil<-data.frame(touche_sum1,touche_sum2,touche_sum3_4,touche_sum5_6,touche_sum7_9,
                         touche_sum10_14,touche_sum15_19,touche_sum20_24,touche_sum25_29,touche_sum30_more,
                         stringsAsFactors = FALSE)

touche_decil<-touche_decil%>%rename(`1 touch points`='mean.n.',`2 touch points`='mean.n..1',
                                    `3-4 touch points`='mean.n..2',`5-6 touch points`='mean.n..3',
                                    `7-9 touch points`='mean.n..4',`10-14 touch points`='mean.n..5',
                                    `15-19 touch points`='mean.n..6',`20-24 touch points`='mean.n..7',
                                    `25-29 touch points`='mean.n..8',`30+ touch points`='mean.n..9')

touche_decil <- as.data.frame(t(touche_decil),stringsAsFactors = FALSE)

touche_sum <- data.frame(jour = row.names(touche_decil), sum_touche=touche_decil$V1,stringsAsFactors = FALSE)
touche_sum <- touche_sum%>%rename(touch=jour)
touche_sum <- left_join(touche_sum, touch_sum, by="touch")
touche_sum <- touche_sum%>%rename(CA_moyen=sum_touche)

# touche <- sum(touche_sum$sum_touche)
# touche_sum <- touche_sum%>%mutate(pourcent_decil_touch=sum_touche/touche*100)
# touche_sum <- touche_sum%>%rename(touche=jour)











#### bilan avec 5eme colonnne temps pour achat ####
timeTab<-csvAll3%>%select(Touch.points,Time.to.convert,New.Customer)%>%filter(Time.to.convert!="" & New.Customer==1 & Touch.points!=0)

c=0
for (i in timeTab$Time.to.convert){
  c=c+1
  if(grepl("d",i)){
    d=str_locate(i,"d")
    timeTab$Time.to.convert[c]=str_sub(timeTab$Time.to.convert[c],1,d-1)}
  else{
    timeTab$Time.to.convert[c]="0"}}


timeTab$Time.to.convert=as.numeric(timeTab$Time.to.convert)

timenombre<-timeTab%>%select(Touch.points,Time.to.convert)%>%
  filter(Time.to.convert!="")

# timenombre$Touch.points=as.numeric(timenombre$Time.to.convert)

timenombre<-timenombre%>% select(Time.to.convert,Touch.points)%>%group_by(Touch.points)%>%
  summarise(n=mean(Time.to.convert))%>%arrange(Touch.points)


time_sum1<-timenombre%>%filter(Touch.points==1)%>%ungroup()%>%summarise(mean(n))
time_sum2<-timenombre%>%filter(Touch.points==2)%>%ungroup()%>%summarise(mean(n))
time_sum3_4<-timenombre%>%filter(Touch.points>2 & Touch.points<5) %>% ungroup()%>%summarise(mean(n))
time_sum5_6<-timenombre%>%filter(Touch.points>4 & Touch.points<7)%>%ungroup()%>%summarise(mean(n))
time_sum7_9<-timenombre%>%filter(Touch.points>6 & Touch.points<10)%>%ungroup()%>%summarise(mean(n))
time_sum10_14<-timenombre%>%filter(Touch.points>9 & Touch.points<15)%>%ungroup()%>%summarise(mean(n))
time_sum15_19<-timenombre%>%filter(Touch.points>14 & Touch.points<20)%>%ungroup()%>%summarise(mean(n))
time_sum20_24<-timenombre%>%filter(Touch.points>19 & Touch.points<25)%>%ungroup()%>%summarise(mean(n))
time_sum25_29<-timenombre%>%filter(Touch.points>24 & Touch.points<30)%>%ungroup()%>%summarise(mean(n))
time_sum30_more<-timenombre%>%filter(Touch.points>29)%>%ungroup()%>%summarise(mean(n))

time_decil2<-data.frame(time_sum1,time_sum2,time_sum3_4,time_sum5_6,time_sum7_9,
                        time_sum10_14,time_sum15_19,time_sum20_24,time_sum25_29,time_sum30_more,
                        stringsAsFactors = FALSE)

time_decil2<-time_decil2%>%rename(`1 touch points`='mean.n.',`2 touch points`='mean.n..1',
                                  `3-4 touch points`='mean.n..2',`5-6 touch points`='mean.n..3',
                                  `7-9 touch points`='mean.n..4',`10-14 touch points`='mean.n..5',
                                  `15-19 touch points`='mean.n..6',`20-24 touch points`='mean.n..7',
                                  `25-29 touch points`='mean.n..8',`30+ touch points`='mean.n..9')

time_decil2 <- as.data.frame(t(time_decil2),stringsAsFactors = FALSE)



time_sum2b <- data.frame(touch = row.names(time_decil2), sum_time=time_decil2$V1,stringsAsFactors = FALSE)
touche_sum <- left_join(touche_sum, time_sum2b, by="touch")
touche_sum <- touche_sum%>%rename(temps_moyen=sum_time)



#### ajout colonne au bilan - CA TOTAL par touch - A NE PAS AJOUTER####

touchtabtest<-csvAll3%>%select(Amount,Touch.points,Time.to.convert,New.Customer,Order.ID,User.id)%>%
  filter(Time.to.convert!="" & New.Customer==1)

touchtabtest$Touch.points=as.numeric(touchtabtest$Touch.points)

touchtabtest<-touchtabtest%>% select(Amount,Time.to.convert,Touch.points)%>%group_by(Touch.points)%>%summarise(n=sum(Amount))%>%
  arrange(Touch.points)


touch_sum1test<-touchtabtest%>%filter(Touch.points==1)%>%ungroup()%>%summarise(sum(n))
touch_sum2test<-touchtabtest%>%filter(Touch.points==2)%>%ungroup()%>%summarise(sum(n))
touch_sum3_4test<-touchtabtest%>%filter(Touch.points>2 & Touch.points<5) %>% ungroup()%>%summarise(sum(n))
touch_sum5_6test<-touchtabtest%>%filter(Touch.points>4 & Touch.points<7)%>%ungroup()%>%summarise(sum(n))
touch_sum7_9test<-touchtabtest%>%filter(Touch.points>6 & Touch.points<10)%>%ungroup()%>%summarise(sum(n))
touch_sum10_14test<-touchtabtest%>%filter(Touch.points>9 & Touch.points<15)%>%ungroup()%>%summarise(sum(n))
touch_sum15_19test<-touchtabtest%>%filter(Touch.points>14 & Touch.points<20)%>%ungroup()%>%summarise(sum(n))
touch_sum20_24test<-touchtabtest%>%filter(Touch.points>19 & Touch.points<25)%>%ungroup()%>%summarise(sum(n))
touch_sum25_29test<-touchtabtest%>%filter(Touch.points>24 & Touch.points<30)%>%ungroup()%>%summarise(sum(n))
touch_sum30_moretest<-touchtabtest%>%filter(Touch.points>29)%>%ungroup()%>%summarise(sum(n))

touch_deciltest<-data.frame(touch_sum1test,touch_sum2test,touch_sum3_4test,touch_sum5_6test,touch_sum7_9test,
                            touch_sum10_14test,touch_sum15_19test,touch_sum20_24test,touch_sum25_29test,touch_sum30_moretest,
                            stringsAsFactors = FALSE)

touch_deciltest<-touch_deciltest%>%rename(`1 touch points`='sum.n.',`2 touch points`='sum.n..1',
                                          `3-4 touch points`='sum.n..2',`5-6 touch points`='sum.n..3',
                                          `7-9 touch points`='sum.n..4',`10-14 touch points`='sum.n..5',
                                          `15-19 touch points`='sum.n..6',`20-24 touch points`='sum.n..7',
                                          `25-29 touch points`='sum.n..8',`30+ touch points`='sum.n..9')

touch_deciltest <- as.data.frame(t(touch_deciltest),stringsAsFactors = FALSE)

touch_sumtest <- data.frame(touch = row.names(touch_deciltest), sum_touchtest=touch_deciltest$V1,stringsAsFactors = FALSE)
touch_sumtest <- touch_sumtest%>%rename(CA_Total_par_Nb_touch=sum_touchtest)

touche_sum <- left_join(touche_sum, touch_sumtest, by="touch")

touche_sum <- touche_sum[, c(1,2,6,3,4,5)]
# touche_sum <- touche_sum%>%rename(Nb_touch=sum_touch)
# touche_sum <- touche_sum%>%rename(Pourcent_par_touch=pourcent_decil_touch)




##### Initialisation des variables du CA #### 
affil=0
affilSoloAmount=0
affilMidAmount=0
affilLastAmount=0
affilSolo=0
affilFirst=0
affilMid=0
affilLast=0
referrer=0 
referrerSoloAmount=0
referrerMidAmount=0
referrerLastAmount=0
referrerSolo=0
referrerFirst=0
referrerMid=0
referrerLast=0
sem=0
semSoloAmount=0
semMidAmount=0
semLastAmount=0
semSolo=0
semFirst=0
semMid=0
semLast=0
seo=0
seoSoloAmount=0
seoMidAmount=0
seoLastAmount=0
seoSolo=0
seoFirst=0
seoMid=0
seoLast=0
mail=0
mailSoloAmount=0
mailMidAmount=0
mailLastAmount=0
mailSolo=0
mailFirst=0
mailMid=0
mailLast=0
retar=0
retarSoloAmount=0
retarLastAmount=0
retarMidAmount=0
retarSolo=0
retarFirst=0
retarMid=0
retarLast=0
direct=0
directSoloAmount=0
directMidAmount=0
directLastAmount=0
directSolo=0
directFirst=0
directMid=0
directLast=0
ads=0
adsSoloAmount=0
adsFirstAmount=0
adsMidAmount=0
adsLastAmount=0
adsSolo=0
adsFirst=0
adsMid=0
adsLast=0
semBrand=0
semBrandSoloAmount=0
semBrandMidAmount=0
semBrandLastAmount=0
semBrandSolo=0
semBrandFirst=0
semBrandMid=0
semBrandLast=0
error=0
c2=0


##### test du modele croissant problem #####

df42 <- df_hm2


ads=0
semBrand=0
sem=0
seo=0
affil=0
direct=0
mail=0
referrer=0
retar=0
c2=0
for (i in df42$path){
  a=lapply(str_split(i," > "),as.list)
  taille=length(a[[1]])
  if(taille%%2==0){
    denominateur=taille*(taille/2+0.5)
  }
  else{
    denominateur=taille*(taille/2+0.5)
  }
  c=0
  c2=c2+1
  for(elem in a[[1]]){
    c=c+1
    if (grepl("Ads",elem)){
      ads=ads+(c/denominateur)*df42$Amount[c2]
      adsFirst=adsFirst+1}
    else if (grepl("Brand",elem)){
      semBrand=semBrand+(c/denominateur)*df42$Amount[c2]
      semBrandFirst=semBrandFirst+1}
    else if (grepl("SEM",elem)){
      sem=sem+(c/denominateur)*df42$Amount[c2]
      semFirst=semFirst+1}
    else if (grepl("SEO",elem)){
      seo=seo+(c/denominateur)*df42$Amount[c2]
      seoFirst=seoFirst+1}
    else if (grepl("Affiliation",elem)){
      affil=affil+(c/denominateur)*df42$Amount[c2]
      affilFirst=affilFirst+1}
    else if (grepl("Direct",elem)){
      direct=direct+(c/denominateur)*df42$Amount[c2]
      directFirst=directFirst+1}
    else if (grepl("mail",elem)){
      mail=mail+(c/denominateur)*df42$Amount[c2]
      mailFirst=mailFirst+1}
    else if (grepl("Referrer",elem)){
      referrer=referrer+(c/denominateur)*df42$Amount[c2]
      referrerFirst=referrerFirst+1}
    else if (grepl("Retar",elem)){
      retar=retar+(c/denominateur)*df42$Amount[c2]
      retarFirst=retarFirst+1}
  }
  
}
df11 <- data.frame(Affiliation=affil,Referrer=referrer,
                   SEM=sem,SEO=seo,
                   E_mail=mail,Retargeting=retar,
                   Direct_Access=direct,Social_Ads=ads,
                   SEM_Brand=semBrand,stringsAsFactors = FALSE)
# Transposition de la DF
df11 <- as.data.frame(t(df11))
df11 <- data.frame(Channel = row.names(df11), Ca_Croissant=df11$V1,stringsAsFactors = FALSE)
croissant <- df11

#### Modele dépréciation dans le temps décroissant ###############################################



ads=0
semBrand=0
sem=0
seo=0
affil=0
direct=0
mail=0
referrer=0
retar=0
c2=0
for (i in df42$path){
  a=lapply(str_split(i," > "),as.list)
  taille=length(a[[1]])
  if(taille%%2==0){
    denominateur=taille*(taille/2+0.5)
  }
  else{
    denominateur=taille*(taille/2+0.5)
  }
  c=taille+1
  c2=c2+1
  for(elem in a[[1]]){
    c=c-1
    if (grepl("Ads",elem)){
      ads=ads+(c/denominateur)*df42$Amount[c2]
      adsFirst=adsFirst+1}
    else if (grepl("Brand",elem)){
      semBrand=semBrand+(c/denominateur)*df42$Amount[c2]
      semBrandFirst=semBrandFirst+1}
    else if (grepl("SEM",elem)){
      sem=sem+(c/denominateur)*df42$Amount[c2]
      semFirst=semFirst+1}
    else if (grepl("SEO",elem)){
      seo=seo+(c/denominateur)*df42$Amount[c2]
      seoFirst=seoFirst+1}
    else if (grepl("Affiliation",elem)){
      affil=affil+(c/denominateur)*df42$Amount[c2]
      affilFirst=affilFirst+1}
    else if (grepl("Direct",elem)){
      direct=direct+(c/denominateur)*df42$Amount[c2]
      directFirst=directFirst+1}
    else if (grepl("mail",elem)){
      mail=mail+(c/denominateur)*df42$Amount[c2]
      mailFirst=mailFirst+1}
    else if (grepl("Referrer",elem)){
      referrer=referrer+(c/denominateur)*df42$Amount[c2]
      referrerFirst=referrerFirst+1}
    else if (grepl("Retar",elem)){
      retar=retar+(c/denominateur)*df42$Amount[c2]
      retarFirst=retarFirst+1}
  }
  
}
df13 <- data.frame(Affiliation=affil,Referrer=referrer,
                   SEM=sem,SEO=seo,
                   E_mail=mail,Retargeting=retar,
                   Direct_Access=direct,Social_Ads=ads,
                   SEM_Brand=semBrand,stringsAsFactors = FALSE)
# Transposition de la DF
df13 <- as.data.frame(t(df13))
df13 <- data.frame(Channel = row.names(df13), Ca_Decroissant=df13$V1,stringsAsFactors = FALSE)
decroissant <- df13
# ddd<-lef#######t_join(df11,df12,by="Channel")





#### Join croissant et decroissant ####

depreciation <- left_join(croissant,decroissant, by='Channel')
BILAN3 <- left_join(BILAN2, depreciation, by='Channel')
BILAN3 <- BILAN3[, c(1,13,14,2,3,4,5,6,7,8,9,10,11,12)]
BILAN3 <- BILAN3%>%select(Channel,Ca_Croissant,Ca_Decroissant,first_touch_conversions,Amount_ft,CA_moyen_ft,last_touch_conversions,
                          Amount_lt,CA_moyen_lt,Amount_mid,CA_moyen_mid,Amount_mid2,CA_moyen_mid2)

BILAN_FT_LT_CR_DE <- BILAN3

# rename.values(x, fish="newfish")
  # df11 <- data.frame(Affiliation=affil,Referrer=referrer,
  #                    SEM=sem,SEO=seo,
  #                    E_mail=mail,Retargeting=retar,
  #                    Direct_Access=direct,Social_Ads=ads,
  #                    SEM_Brand=semBrand,stringsAsFactors = FALSE)




##### Fonctions pour faire les df lié au model en U --- Deuxieme fct -> A raccourcir, decoupage en pls fct? ####
dfFunction<-function(affil,referrer,sem,seo,mail,retar,direct,ads,semBrand){
  df <- data.frame(Affi=affil,Refer=referrer,
                   SEM=sem,SEO=seo,
                   mail=mail,Retar=retar,
                   Direct=direct,Ads=ads,
                   Brand=semBrand,stringsAsFactors = FALSE)
  # Transposition de la DF
  df <- as.data.frame(t(df))
  df <- data.frame(Channel = row.names(df), CaChannel=df$V1,stringsAsFactors = FALSE)
  return(df)
}
ufct<-function(df42){
  for (i in df42$path){
    a=lapply(strsplit(i," > ",fixed = TRUE),as.list)
    taille=length(a[[1]])
    if (taille>2){mid=0.2/(taille-2)}
    c=0
    c2=c2+1
    for (elem in a[[1]]){
      c=c+1
      if (c==1 & taille>2){
        if (grepl("Ads",elem)){
          ads=ads+0.4*df42$Amount[c2]
          adsFirst=adsFirst+1}
        else if (grepl("Brand",elem)){
          semBrand=semBrand+0.4*df42$Amount[c2]
          semBrandFirst=semBrandFirst+1}
        else if (grepl("SEM",elem)){
          sem=sem+0.4*df42$Amount[c2]
          semFirst=semFirst+1}
        else if (grepl("SEO",elem)){
          seo=seo+0.4*df42$Amount[c2]
          seoFirst=seoFirst+1}
        else if (grepl("Affiliation",elem)){
          affil=affil+0.4*df42$Amount[c2]
          affilFirst=affilFirst+1}
        else if (grepl("Direct",elem)){
          direct=direct+0.4*df42$Amount[c2]
          directFirst=directFirst+1}
        else if (grepl("mail",elem)){
          mail=mail+0.4*df42$Amount[c2]
          mailFirst=mailFirst+1}
        else if (grepl("Referrer",elem)){
          referrer=referrer+0.4*df42$Amount[c2]
          referrerFirst=referrerFirst+1}
        else if (grepl("Retar",elem)){
          retar=retar+0.4*df42$Amount[c2]
          retarFirst=retarFirst+1}
      }
      else if (c==1 & taille==2){
        if (grepl("Ads",elem)){
          ads=ads+0.5*df42$Amount[c2]
          adsFirst=adsFirst+1}
        else if (grepl("Brand",elem)){
          semBrand=semBrand+0.5*df42$Amount[c2]
          semBrandFirst=semBrandFirst+1}
        else if (grepl("SEM",elem)){
          sem=sem+0.5*df42$Amount[c2]
          semFirst=semFirst+1}
        else if (grepl("SEO",elem)){
          seo=seo+0.5*df42$Amount[c2]
          seoFirst=seoFirst+1}
        else if (grepl("Affiliation",elem)){
          affil=affil+0.5*df42$Amount[c2]
          affilFirst=affilFirst+1}
        else if (grepl("Direct",elem)){
          direct=direct+0.5*df42$Amount[c2]
          directFirst=directFirst+1}
        else if (grepl("mail",elem)){
          mail=mail+0.5*df42$Amount[c2]
          mailFirst=mailFirst+1}
        else if (grepl("Referrer",elem)){
          referrer=referrer+0.5*df42$Amount[c2]
          referrerFirst=referrerFirst+1}
        else if (grepl("Retar",elem)){
          retar=retar+0.5*df42$Amount[c2]
          retarFirst=retarFirst+1}
      }
      else if(c==1 & taille==1) {
        if (grepl("Ads",elem)){
          adsSoloAmount=adsSoloAmount+df42$Amount[c2]
          adsSolo=adsSolo+1}
        else if (grepl("Brand",elem)){
          semBrandSoloAmount=semBrandSoloAmount+df42$Amount[c2]
          semBrandSolo=semBrandSolo+1}
        else if (grepl("SEM",elem)){
          semSoloAmount=semSoloAmount+df42$Amount[c2]
          semSolo=semSolo+1}
        else if (grepl("SEO",elem)){
          seoSoloAmount=seoSoloAmount+df42$Amount[c2]
          seoSolo=semSolo+1}
        else if (grepl("Affiliation",elem)){
          affilSoloAmount=affilSoloAmount+df42$Amount[c2]
          affilSolo=affilSolo+1}
        else if (grepl("Direct",elem)){
          directSoloAmount=directSoloAmount+df42$Amount[c2]
          directSolo=directSolo+1}
        else if (grepl("mail",elem)){
          mailSoloAmount=mailSoloAmount+df42$Amount[c2]
          mailSolo=mailSolo+1}
        else if (grepl("Referrer",elem)){
          referrerSoloAmount=referrerSoloAmount+df42$Amount[c2]
          referrerSolo=referrerSolo+1}
        else if (grepl("Retar",elem)){
          retarSoloAmount=retarSoloAmount+df42$Amount[c2]
          retarSolo=retarSolo+1}
      }
      else if (c==2 & taille==2){
        if (grepl("Ads",elem)){
          adsLastAmount=adsLastAmount+0.5*df42$Amount[c2]
          adsLast=adsLast+1}
        else if (grepl("Brand",elem)){
          semBrandLastAmount=semBrandLastAmount+0.5*df42$Amount[c2]
          semBrandLast=semBrandLast+1}
        else if (grepl("SEM",elem)){
          semLastAmount=semLastAmount+0.5*df42$Amount[c2]
          semLast=semLast+1}
        else if (grepl("SEO",elem)){
          seoLastAmount=seoLastAmount+0.5*df42$Amount[c2]
          seoLast=seoLast+1}
        else if (grepl("Affiliation",elem)){
          affilLastAmount=affilLastAmount+0.5*df42$Amount[c2]
          affilLast=affilLast+1}
        else if (grepl("Direct",elem)){
          directLastAmount=directLastAmount+0.5*df42$Amount[c2]
          directLast=directLast+1}
        else if (grepl("mail",elem)){
          mailLastAmount=mailLastAmount+0.5*df42$Amount[c2]
          mailLast=mailLast+1}
        else if (grepl("Referrer",elem)){
          referrerLastAmount=referrerLastAmount+0.5*df42$Amount[c2]
          referrerLast=referrerLast+1}
        else if (grepl("Retar",elem)){
          retarLastAmount=retarLastAmount+0.5*df42$Amount[c2]
          retarLast=retarLast+1}
      }
      else if (c==taille & taille>2){
        if (grepl("Ads",elem)){
          adsLastAmount=adsLastAmount+0.4*df42$Amount[c2]
          adsLast=adsLast+1}
        else if (grepl("Brand",elem)){
          semBrandLastAmount=semBrandLastAmount+0.4*df42$Amount[c2]
          semBrandLast=semBrandLast+1}
        else if (grepl("SEM",elem)){
          semLastAmount=semLastAmount+0.4*df42$Amount[c2]
          semLast=semLast+1}
        else if (grepl("SEO",elem)){
          seoLastAmount=seoLastAmount+0.4*df42$Amount[c2]
          seoLast=seoLast+1}
        else if (grepl("Affiliation",elem)){
          affilLastAmount=affilLastAmount+0.4*df42$Amount[c2]
          affilLast=affilLast+1}
        else if (grepl("Direct",elem)){
          directLastAmount=directLastAmount+0.4*df42$Amount[c2]
          directLast=directLast+1}
        else if (grepl("mail",elem)){
          mailLastAmount=mailLastAmount+0.4*df42$Amount[c2]
          mailLast=mailLast+1}
        else if (grepl("Referrer",elem)){
          referrerLastAmount=referrerLastAmount+0.4*df42$Amount[c2]
          referrerLast=referrerLast+1}
        else if (grepl("Retar",elem)){
          retarLastAmount=retarLastAmount+0.4*df42$Amount[c2]
          retarLast=retarLast+1}
      }
      else if(taille>2 & c!=1 & c!=taille){
        if (grepl("Ads",elem)){
          adsMidAmount=adsMidAmount+mid*df42$Amount[c2]
          adsMid=adsMid+1}
        else if (grepl("Brand",elem)){
          semBrandMidAmount=semBrandMidAmount+mid*df42$Amount[c2]
          semBrandMid=semBrandMid+1
        }
        else if (grepl("SEM",elem)){
          semMidAmount=semMidAmount+mid*df42$Amount[c2]
          semMid=semMid+1}
        else if (grepl("SEO",elem)){
          seoMidAmount=seoMidAmount+mid*df42$Amount[c2]
          seoMid=seoMid+1}
        else if (grepl("Affiliation",elem)){
          affilMidAmount=affilMidAmount+mid*df42$Amount[c2]
          affilMid=affilMid+1}
        else if (grepl("Direct",elem)){
          directMidAmount=directMidAmount+mid*df42$Amount[c2]
          directMid=directMid+1}
        else if (grepl("mail",elem)){
          mailMidAmount=mailMidAmount+mid*df42$Amount[c2]
          mailMid=mailMid+1}
        else if (grepl("Referrer",elem)){
          referrerMidAmount=referrerMidAmount+mid*df42$Amount[c2]
          referrerMid=referrerMid+1}
        else if (grepl("Retar",elem)){
          retarMidAmount=retarMidAmount+mid*df42$Amount[c2]
          retarMid=retarMid+1}}}}
  
  dfFirstAmount<-dfFunction(affil,referrer,sem,seo,mail,retar,direct,ads,semBrand)
  dfFirstAmount<-dfFirstAmount%>%rename(CaFirst=CaChannel)
  dfSoloAmount<-dfFunction(affilSoloAmount,referrerSoloAmount,semSoloAmount,seoSoloAmount,mailSoloAmount,retarSoloAmount,directSoloAmount,adsSoloAmount,semBrandSoloAmount)
  dfSoloAmount<-dfSoloAmount%>%rename(CaSolo=CaChannel)
  dfMidAmount<-dfFunction(affilMidAmount,referrerMidAmount,semMidAmount,seoMidAmount,mailMidAmount,retarMidAmount,directMidAmount,adsMidAmount,semBrandMidAmount)
  dfMidAmount<-dfMidAmount%>%rename(CaMid=CaChannel)
  dfLastAmount<-dfFunction(affilLastAmount,referrerLastAmount,semLastAmount,seoLastAmount,mailLastAmount,retarLastAmount,directLastAmount,adsLastAmount,semBrandLastAmount)
  dfLastAmount<-dfLastAmount%>%rename(CaLast=CaChannel)
  dfFirstClick<-dfFunction(affilFirst,referrerFirst,semFirst,seoFirst,mailFirst,retarFirst,directFirst,adsFirst,semBrandFirst)
  dfFirstClick<-dfFirstClick%>%rename(nbFirstClick=CaChannel)
  dfSoloClick<-dfFunction(affilSolo,referrerSolo,semSolo,seoSolo,mailSolo,retarSolo,directSolo,adsSolo,semBrandSolo)
  dfSoloClick<-dfSoloClick%>%rename(nbSoloClick=CaChannel)
  dfMidClick<-dfFunction(affilMid,referrerMid,semMid,seoMid,mailMid,retarMid,directMid,adsMid,semBrandMid)
  dfMidClick<-dfMidClick%>%rename(nbMidClick=CaChannel)
  dfLastClick<-dfFunction(affilLast,referrerLast,semLast,seoLast,mailLast,retarLast,directLast,adsLast,semBrandLast)
  dfLastClick<-dfLastClick%>%rename(nbLastClick=CaChannel)
  
  dfTotalU<-left_join(left_join(left_join(left_join(left_join(left_join(left_join(dfFirstClick, dfFirstAmount, by="Channel"),dfSoloClick),dfSoloAmount),dfMidClick),dfMidAmount),dfLastClick),dfLastAmount)
  dfTotalU<-dfTotalU%>%mutate(TotalCa=CaFirst+CaSolo+CaMid+CaLast)
  return(dfTotalU)
}
test<-ufct(df42)
##### Fonctions pour faire les df lié au model en U --- V2 50-20-30####



dfFunction<-function(affil,referrer,sem,seo,mail,retar,direct,ads,semBrand){
  df <- data.frame(Affi=affil,Refer=referrer,
                   SEM=sem,SEO=seo,
                   mail=mail,Retar=retar,
                   Direct=direct,Ads=ads,
                   Brand=semBrand,stringsAsFactors = FALSE)
  # Transposition de la DF
  df <- as.data.frame(t(df))
  df <- data.frame(Channel = row.names(df), CaChannel=df$V1,stringsAsFactors = FALSE)
  return(df)
}
ufct<-function(df42){
  for (i in df42$path){
    a=lapply(strsplit(i," > ",fixed = TRUE),as.list)
    taille=length(a[[1]])
    if (taille>2){mid=0.2/(taille-2)}
    c=0
    c2=c2+1
    for (elem in a[[1]]){
      c=c+1
      if (c==1 & taille>2){
        if (grepl("Ads",elem)){
          ads=ads+0.5*df42$Amount[c2]
          adsFirst=adsFirst+0.5}
        else if (grepl("Brand",elem)){
          semBrand=semBrand+0.5*df42$Amount[c2]
          semBrandFirst=semBrandFirst+0.5}
        else if (grepl("SEM",elem)){
          sem=sem+0.5*df42$Amount[c2]
          semFirst=semFirst+0.5}
        else if (grepl("SEO",elem)){
          seo=seo+0.5*df42$Amount[c2]
          seoFirst=seoFirst+0.5}
        else if (grepl("Affiliation",elem)){
          affil=affil+0.5*df42$Amount[c2]
          affilFirst=affilFirst+0.5}
        else if (grepl("Direct",elem)){
          direct=direct+0.5*df42$Amount[c2]
          directFirst=directFirst+0.5}
        else if (grepl("mail",elem)){
          mail=mail+0.5*df42$Amount[c2]
          mailFirst=mailFirst+0.5}
        else if (grepl("Referrer",elem)){
          referrer=referrer+0.5*df42$Amount[c2]
          referrerFirst=referrerFirst+0.5}
        else if (grepl("Retar",elem)){
          retar=retar+0.5*df42$Amount[c2]
          retarFirst=retarFirst+0.5}
      }
      else if (c==1 & taille==2){
        if (grepl("Ads",elem)){
          ads=ads+0.6*df42$Amount[c2]
          adsFirst=adsFirst+0.6}
        else if (grepl("Brand",elem)){
          semBrand=semBrand+0.6*df42$Amount[c2]
          semBrandFirst=semBrandFirst+0.6}
        else if (grepl("SEM",elem)){
          sem=sem+0.6*df42$Amount[c2]
          semFirst=semFirst+0.6}
        else if (grepl("SEO",elem)){
          seo=seo+0.6*df42$Amount[c2]
          seoFirst=seoFirst+0.6}
        else if (grepl("Affiliation",elem)){
          affil=affil+0.6*df42$Amount[c2]
          affilFirst=affilFirst+0.6}
        else if (grepl("Direct",elem)){
          direct=direct+0.6*df42$Amount[c2]
          directFirst=directFirst+0.6}
        else if (grepl("mail",elem)){
          mail=mail+0.6*df42$Amount[c2]
          mailFirst=mailFirst+0.6}
        else if (grepl("Referrer",elem)){
          referrer=referrer+0.6*df42$Amount[c2]
          referrerFirst=referrerFirst+0.6}
        else if (grepl("Retar",elem)){
          retar=retar+0.6*df42$Amount[c2]
          retarFirst=retarFirst+0.6}
      }
      else if(c==1 & taille==1) {
        if (grepl("Ads",elem)){
          ads=ads+df42$Amount[c2]
          adsFirst=adsFirst+1}
        else if (grepl("Brand",elem)){
          semBrand=semBrand+df42$Amount[c2]
          semBrandFirst=semBrandFirst+1}
        else if (grepl("SEM",elem)){
          sem=sem+df42$Amount[c2]
          semFirst=semFirst+1}
        else if (grepl("SEO",elem)){
          seo=seo+df42$Amount[c2]
          seoFirst=seoFirst+1}
        else if (grepl("Affiliation",elem)){
          affil=affil+df42$Amount[c2]
          affilFirst=affilFirst+1}
        else if (grepl("Direct",elem)){
          direct=direct+df42$Amount[c2]
          directFirst=directFirst+1}
        else if (grepl("mail",elem)){
          mail=mail+df42$Amount[c2]
          mailFirst=mailFirst+1}
        else if (grepl("Referrer",elem)){
          referrer=referrer+df42$Amount[c2]
          referrerFirst=referrerFirst+1}
        else if (grepl("Retar",elem)){
          retar=retar+df42$Amount[c2]
          retarFirst=retarFirst+1}
      }
      else if (c==2 & taille==2){
        if (grepl("Ads",elem)){
          ads=ads+0.4*df42$Amount[c2]
          adsFirst=adsFirst+0.4}
        else if (grepl("Brand",elem)){
          semBrand=semBrand+0.4*df42$Amount[c2]
          semBrandFirst=semBrandFirst+0.4}
        else if (grepl("SEM",elem)){
          sem=sem+0.4*df42$Amount[c2]
          semFirst=semFirst+0.4}
        else if (grepl("SEO",elem)){
          seo=seo+0.4*df42$Amount[c2]
          seoFirst=seoFirst+0.4}
        else if (grepl("Affiliation",elem)){
          affil=affil+0.4*df42$Amount[c2]
          affilFirst=affilFirst+0.4}
        else if (grepl("Direct",elem)){
          direct=direct+0.4*df42$Amount[c2]
          directFirst=directFirst+0.4}
        else if (grepl("mail",elem)){
          mail=mail+0.4*df42$Amount[c2]
          mailFirst=mailFirst+0.4}
        else if (grepl("Referrer",elem)){
          referrer=referrer+0.4*df42$Amount[c2]
          referrerFirst=referrerFirst+0.4}
        else if (grepl("Retar",elem)){
          retar=retar+0.4*df42$Amount[c2]
          retarFirst=retarFirst+0.4}
      }
      else if (c==taille & taille>2){
        if (grepl("Ads",elem)){
          ads=ads+0.3*df42$Amount[c2]
          adsFirst=adsFirst+0.3}
        else if (grepl("Brand",elem)){
          semBrand=semBrand+0.3*df42$Amount[c2]
          semBrandFirst=semBrandFirst+0.3}
        else if (grepl("SEM",elem)){
          sem=sem+0.3*df42$Amount[c2]
          semFirst=semFirst+0.3}
        else if (grepl("SEO",elem)){
          seo=seo+0.3*df42$Amount[c2]
          seoFirst=seoFirst+0.3}
        else if (grepl("Affiliation",elem)){
          affil=affil+0.3*df42$Amount[c2]
          affilFirst=affilFirst+0.3}
        else if (grepl("Direct",elem)){
          direct=direct+0.3*df42$Amount[c2]
          directFirst=directFirst+0.3}
        else if (grepl("mail",elem)){
          mail=mail+0.3*df42$Amount[c2]
          mailFirst=mailFirst+0.3}
        else if (grepl("Referrer",elem)){
          referrer=referrer+0.3*df42$Amount[c2]
          referrerFirst=referrerFirst+0.3}
        else if (grepl("Retar",elem)){
          retar=retar+0.3*df42$Amount[c2]
          retarFirst=retarFirst+0.3}
      }
      else if(taille>2 & c!=1 & c!=taille){
        if (grepl("Ads",elem)){
          ads=ads+mid*df42$Amount[c2]
          adsFirst=adsFirst+mid}
        else if (grepl("Brand",elem)){
          semBrand=semBrand+mid*df42$Amount[c2]
          semBrandFirst=semBrandFirst+mid
        }
        else if (grepl("SEM",elem)){
          sem=sem+mid*df42$Amount[c2]
          semFirst=semFirst+mid}
        else if (grepl("SEO",elem)){
          seo=seo+mid*df42$Amount[c2]
          seoFirst=seoFirst+mid}
        else if (grepl("Affiliation",elem)){
          affil=affil+mid*df42$Amount[c2]
          affilFirst=affilFirst+mid}
        else if (grepl("Direct",elem)){
          direct=direct+mid*df42$Amount[c2]
          directFirst=directFirst+mid}
        else if (grepl("mail",elem)){
          mail=mail+mid*df42$Amount[c2]
          mailFirst=mailFirst+mid}
        else if (grepl("Referrer",elem)){
          referrer=referrer+mid*df42$Amount[c2]
          referrerFirst=referrerFirst+mid}
        else if (grepl("Retar",elem)){
          retar=retar+mid*df42$Amount[c2]
          retarFirst=retarFirst+mid}}}}
  
  
  dfFirstAmount<-dfFunction(affil,referrer,sem,seo,mail,retar,direct,ads,semBrand)
  dfFirstAmount<-dfFirstAmount%>%rename(CaFirst=CaChannel)
  dfSoloAmount<-dfFunction(affilSoloAmount,referrerSoloAmount,semSoloAmount,seoSoloAmount,mailSoloAmount,retarSoloAmount,directSoloAmount,adsSoloAmount,semBrandSoloAmount)
  dfSoloAmount<-dfSoloAmount%>%rename(CaSolo=CaChannel)
  dfMidAmount<-dfFunction(affilMidAmount,referrerMidAmount,semMidAmount,seoMidAmount,mailMidAmount,retarMidAmount,directMidAmount,adsMidAmount,semBrandMidAmount)
  dfMidAmount<-dfMidAmount%>%rename(CaMid=CaChannel)
  dfLastAmount<-dfFunction(affilLastAmount,referrerLastAmount,semLastAmount,seoLastAmount,mailLastAmount,retarLastAmount,directLastAmount,adsLastAmount,semBrandLastAmount)
  dfLastAmount<-dfLastAmount%>%rename(CaLast=CaChannel)
  dfFirstClick<-dfFunction(affilFirst,referrerFirst,semFirst,seoFirst,mailFirst,retarFirst,directFirst,adsFirst,semBrandFirst)
  dfFirstClick<-dfFirstClick%>%rename(nbFirstClick=CaChannel)
  dfSoloClick<-dfFunction(affilSolo,referrerSolo,semSolo,seoSolo,mailSolo,retarSolo,directSolo,adsSolo,semBrandSolo)
  dfSoloClick<-dfSoloClick%>%rename(nbSoloClick=CaChannel)
  dfMidClick<-dfFunction(affilMid,referrerMid,semMid,seoMid,mailMid,retarMid,directMid,adsMid,semBrandMid)
  dfMidClick<-dfMidClick%>%rename(nbMidClick=CaChannel)
  dfLastClick<-dfFunction(affilLast,referrerLast,semLast,seoLast,mailLast,retarLast,directLast,adsLast,semBrandLast)
  dfLastClick<-dfLastClick%>%rename(nbLastClick=CaChannel)
  
  # dfTotalU<-left_join(left_join(dfFirstClick, dfMidClick, by="Channel"),dfSoloClick),dfSoloAmount),dfMidClick),dfMidAmount),dfLastClick),dfLastAmount)
  dfTotalU<-left_join(dfFirstAmount,dfFirstClick, by="Channel")
  dfTotalU<- dfTotalU%>%rename(CA_Channel=CaFirst)
  dfTotalU<-dfTotalU%>%rename(ValeurCommande=nbFirstClick)
  # dfTotalU<-left_join(left_join(left_join(left_join(left_join(left_join(left_join(dfFirstClick, dfFirstAmount, by="Channel"),dfSoloClick),dfSoloAmount),dfMidClick),dfMidAmount),dfLastClick),dfLastAmount)
  # dfTotalU<-dfTotalU%>%mutate(TotalCa=CaFirst+CaSolo+CaMid+CaLast)
  return(dfTotalU)
}
test<-ufct(df42)

#### extract to excel #####

# install.packages("xlsx")
library("xlsx")

write.xlsx(BILAN2, "V:/Commercial/ID_MARKET/DAM - Data/Analyses Ponctuelles/GRETA/Excel_Nico/BILAN.xlsx", sheetName = "BILAN2", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

write.xlsx(touch_sum, "V:/Commercial/ID_MARKET/DAM - Data/Analyses Ponctuelles/GRETA/Excel_Nico/touch_sum.xlsx", sheetName = "touch_sum", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

write.xlsx(time_sum, "V:/Commercial/ID_MARKET/DAM - Data/Analyses Ponctuelles/GRETA/Excel_Nico/time_sum.xlsx", sheetName = "time_sum", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


write.xlsx(touche_sum, "V:/Commercial/ID_MARKET/DAM - Data/Analyses Ponctuelles/GRETA/Excel_Nico/colonne_moyenne.xlsx", sheetName = "touch_sum", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

write.xlsx(BILAN3, "V:/Commercial/ID_MARKET/DAM - Data/Analyses Ponctuelles/GRETA/BILAN3.xlsx", sheetName = "BILAN3", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

write.xlsx(test, "V:/Commercial/ID_MARKET/DAM - Data/Analyses Ponctuelles/GRETA/BILAN_U.xlsx", sheetName = "BILAN_U", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

write.xlsx(BILAN_FT_LT_CR_DE, "V:\Commercial\ID_MARKET\DAM - Data\Analyses Ponctuelles\GRETA\Nicolas - Modèle d'attributionBILAN_FT_LT_DEPRECIATION.xlsx", sheetName = "BILAN_FT_LT_DEPRECIATION", 
           col.names = TRUE, row.names = TRUE, append = FALSE)



##### TEST decil #########



touchtabtest<-csvAll3%>%select(Touch.points,Time.to.convert,New.Customer,Order.ID,User.id)%>%
  filter(Time.to.convert!="" & New.Customer==1)

touchtabtest$Touch.points=as.numeric(touchtabtest$Touch.points)

touchtabtest<-touchtabtest%>% select(Time.to.convert,Touch.points)%>%group_by(Touch.points)%>%
  count(Touch.points)%>%arrange(Touch.points)


touch_sum1test<-touchtabtest%>%filter(Touch.points==1)%>%ungroup()%>%summarise(sum(n))
touch_sum2test<-touchtabtest%>%filter(Touch.points==2)%>%ungroup()%>%summarise(sum(n))
touch_sum3_4test<-touchtabtest%>%filter(Touch.points>2 & Touch.points<5) %>% ungroup()%>%summarise(sum(n))
touch_sum5_6test<-touchtabtest%>%filter(Touch.points>4 & Touch.points<7)%>%ungroup()%>%summarise(sum(n))
touch_sum7_9test<-touchtabtest%>%filter(Touch.points>6 & Touch.points<10)%>%ungroup()%>%summarise(sum(n))
touch_sum10_14test<-touchtabtest%>%filter(Touch.points>9 & Touch.points<15)%>%ungroup()%>%summarise(sum(n))
touch_sum15_24test<-touchtabtest%>%filter(Touch.points>14 & Touch.points<25)%>%ungroup()%>%summarise(sum(n))
touch_sum25_45test<-touchtabtest%>%filter(Touch.points>24 & Touch.points<46)%>%ungroup()%>%summarise(sum(n))
touch_sum46_79test<-touchtabtest%>%filter(Touch.points>45 & Touch.points<80)%>%ungroup()%>%summarise(sum(n))
touch_sum80_moretest<-touchtabtest%>%filter(Touch.points>79)%>%ungroup()%>%summarise(sum(n))

touch_deciltest<-data.frame(touch_sum1test,touch_sum2test,touch_sum3_4test,touch_sum5_6test,touch_sum7_9test,
                            touch_sum10_14test,touch_sum15_24test,touch_sum25_45test,touch_sum46_79test,touch_sum80_moretest,
                            stringsAsFactors = FALSE)

touch_deciltest<-touch_deciltest%>%rename(`1 touch points`='sum.n.',`2 touch points`='sum.n..1',
                                          `3-4 touch points`='sum.n..2',`5-6 touch points`='sum.n..3',
                                          `7-9 touch points`='sum.n..4',`10-14 touch points`='sum.n..5',
                                          `15-24 touch points`='sum.n..6',`25-45 touch points`='sum.n..7',
                                          `46-79 touch points`='sum.n..8',`80+ touch points`='sum.n..9')

touch_deciltest <- as.data.frame(t(touch_deciltest),stringsAsFactors = FALSE)

touch_sumtest <- data.frame(jour = row.names(touch_deciltest), sum_touchtest=touch_deciltest$V1,stringsAsFactors = FALSE)

touchtest <- sum(touch_sumtest$sum_touchtest)
touch_sumtest <- touch_sumtest%>%mutate(pourcent_decil_touch=sum_touchtest/touchtest*100)
touch_sumtest <- touch_sumtest%>%rename(touchtest=jour)






















##### test modele FT #########################################################☺

df_FT_CA <- df_hm2 %>% select(channel_name_ft,Amount)%>%group_by(channel_name_ft)%>%summarise(CA=sum(Amount))


dexp(df_FT_CA$CA, rate=df_FT_CA$channel_name_ft)

rpois(df_FT_CA$channel_name_ft, df_FT_CA$CA)


test2 <- df_hm2%>%
  select(path,Amount)

test2b <- test2%>%
  select(path,Amount)%>%
  group_by(path)%>%
  summarise(CA=sum(Amount))


check <- test2%>% mutate(Nb_Touch=str_count(path, ">")+1)


cum_check <- check%>% mutate(cumsum(Nb_Touch))
plot(cum_check, 
     xlab = "check$Amount", 
     main = "Cumulative Probability Distribution") 






test <- df_hm2 %>%
  select(path,Amount)%>%
  group_by(path) %>%
  mutate(z = Amount/sum(Amount))




