pacotes <- c("rgdal","plotly","tidyverse","knitr","kableExtra","gridExtra",
             "png","grid","magick","rgl","devtools","GISTools","rayshader",
             "tmap","broom")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Carregando um shapefile do Estado de Santa Catarina (SC):
shp_sc <- readOGR(dsn = "shapefile_sc",
                  layer = "sc_state")

# Visualizando o objeto shp_sc:
tm_shape(shp = shp_sc) + 
  tm_borders()

# Para quem não sabe onde fica o Estado de SC:
tmap_mode("view") # adiciona o restante do mapa ao redor da nossa análise

tm_shape(shp = shp_sc) + 
  tm_borders()

tmap_mode("plot")

# Carregando dados sobre a pobreza em SC:
load("dados_sc.RData")

# Observando a base de dados dados_sc:
dados_sc %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

#kable pega os outputs do R e transorma em html

summary(dados_sc) # Note que há missing values

# Passo 1: Transformando o objeto shp_sc num data frame:
#ggploty só trabalha com dataframe
shp_sc_df <- tidy(shp_sc, region = "CD_GEOCMU") %>% 
  rename(CD_GEOCMU = id)

# Passo 2: Juntando as informações da base de dados dados_sc ao objeto 
# shp_sc_df:
shp_sc_df <- shp_sc_df %>% 
  left_join(dados_sc, by = "CD_GEOCMU")

# Passo 3: Gerando um mapa no ggplot2
shp_sc_df %>%
  ggplot(aes(x = long,
             y = lat, 
             group = group, 
             fill = poverty)) +  #vai preencher cada poligono com os dados da pobreza
  geom_polygon() +
  scale_fill_gradient(limits = range(shp_sc_df$poverty), #preencher o mapa de forma gradiente a respeito da pobreza
                      low = "#FFF3B0", 
                      high="#E09F3E") +
  layer(geom = "path", #adiciona camada com bostas
        stat = "identity", 
        position = "identity",
        mapping = aes(x = long, 
                      y = lat, 
                      group = group, 
                      color = I('#FFFFFF'))) +
  theme(legend.position = "none", 
        axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())

#quanto mais escuro o gráfico, mais pobre a região/cidade
#municipios na cor cinza é pq não temos infos a respeito da pobreza

# Passo 4: Salvando o mapa gerado no Passo 3 num objeto:
mapa_sc <- shp_sc_df %>%
  ggplot(aes(x = long,
             y = lat, 
             group = group, 
             fill = poverty)) +
  geom_polygon() +
  scale_fill_gradient(limits = range(shp_sc_df$poverty),
                      low = "#FFF3B0", 
                      high="#E09F3E") +
  layer(geom = "path", 
        stat = "identity", 
        position = "identity",
        mapping = aes(x = long, 
                      y = lat, 
                      group = group, 
                      color = I('#FFFFFF'))) +
  theme(legend.position = "none", 
        axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())

# Passo 5: Salvando o objeto mapa_sc como um arquivo de extensão *.png, com uma
# boa resolução:
xlim <- ggplot_build(mapa_sc)$layout$panel_scales_x[[1]]$range$range
ylim <- ggplot_build(mapa_sc)$layout$panel_scales_y[[1]]$range$range

#bound box para a variável mapa_sc
#x é minimo e máximo em longitude
#y é minimo e máximo de latitude

#
ggsave(filename = "mapa_co_dsa.png",
       width = diff(xlim) * 4, #diferença do comprimeiro da long. * 4
       height = diff(ylim) * 4, #diferença do comprimeiro da lat. * 4
       units = "cm") #unidade de medida

# Passo 6: Carregando o arquivo mapa_co_dsa.png:
background_mapa <- readPNG("mapa_co_dsa.png")

# Passo 7: Capturando as coordenadas dos centroides de cada município de SC num 
# no centroide de cada municipio estará as barrinhas dos gráficos de pobreza
# data frame:
coordinates(shp_sc) %>% #captura as coordenadas do shape file
  data.frame() %>%  #transforma em date frame
  rename(longitude = 1,  #renomeando as variáveis
         latitude = 2) %>% 
  mutate(CD_GEOCMU = shp_sc@data$CD_GEOCMU) %>% #criando variável
  dplyr::select(latitude, everything()) -> coords_sc #selecionando o que eu quero e salvando tudo na variável coords.sc


# Passo 8: Adicionando as coordenadas dos municípios do Centro-Oeste no objeto
# map_data_centro_oeste
shp_sc_df <- shp_sc_df %>% 
  left_join(coords_sc, by = "CD_GEOCMU")


# Passo 9: Georreferenciando a imagem PNG e plotando marcações sobre a pobreza 
# em SC nos centroides de cada polígono
shp_sc_df %>%
  ggplot() + 
  annotation_custom(        
    rasterGrob(background_mapa, #abertura do arquivo de imagem que salvamos
               width=unit(1,"npc"), 
               height=unit(1,"npc")),-Inf, Inf, -Inf, Inf) + 
  xlim(xlim[1],xlim[2]) + # x-axis Mapping
  ylim(ylim[1],ylim[2]) + # y-axis Mapping
  geom_point(aes(x = longitude, y = latitude, color = poverty), size = 1.5) + 
  scale_colour_gradient(name = "Poverty", 
                        limits = range(shp_sc_df$poverty), 
                        low = "#FCB9B2", 
                        high = "#B23A48") + 
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())


# Passo 10: Salvando o resultado do Passo 9 num objeto
mapa_pobreza <- shp_sc_df %>%
  ggplot() + 
  annotation_custom(
    rasterGrob(background_mapa, 
               width=unit(1,"npc"),
               height=unit(1,"npc")),-Inf, Inf, -Inf, Inf) + 
  xlim(xlim[1],xlim[2]) + # x-axis Mapping
  ylim(ylim[1],ylim[2]) + # y-axis Mapping
  geom_point(aes(x = longitude, y = latitude, color = poverty), size = 1.5) + 
  scale_colour_gradient(name = "Poverty", 
                        limits = range(shp_sc_df$poverty), 
                        low = "#FCB9B2", 
                        high = "#B23A48") + 
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())

# Passo 11: Gerando o mapa 3D da pobreza em SC (não feche a janela pop-up que
# foi aberta até completar esse script até o final)
plot_gg(ggobj = mapa_pobreza, 
        width = 11, #larguras 
        height = 6, #alturas
        scale = 300, #nível de resolução minimo
        multicore = TRUE, 
        windowsize = c(1000, 800))

# Passo 12: Melhorando o resultado do Passo 11:
# ajuste de câmera no gráfico
render_camera(fov = 70, #
              zoom = 0.5, 
              theta = 130, 
              phi = 35)

# Opções de salvamento em vídeo do mapa gerado nos Passos 11 e 12:

azimute_metade <- 30 + 60 * 1/(1 + exp(seq(-7, 20, length.out = 180)/2))
azimute_completo <- c(azimute_metade, rev(azimute_metade))

rotacao <- 0 + 45 * sin(seq(0, 359, length.out = 360) * pi/180)

zoom_metade <- 0.45 + 0.2 * 1/(1 + exp(seq(-5, 20, length.out = 180)))
zoom_completo <- c(zoom_metade, rev(zoom_metade))

render_movie(filename = "resutado1_sc", 
             type = "custom", 
             frames = 360, 
             phi = azimute_completo, 
             zoom = zoom_completo, 
             theta = rotacao)

rm(list = ls())
gc()
plot.new()

