

## IMPORTAR LIBRERIAS
library(xml2)
library(tidyverse)
library(ggrepel)



#########################################
######## CREAR CAMPO FUNCION ############
#########################################

crear_Campo <- function(grass_colour = "#F9F9F9", 
                        line_colour = "#8F8F8F", 
                        background_colour = "#F9F9F9", 
                        goal_colour = "#000000", 
                        goaltype = "box", 
                        # middlethird = TRUE, 
                        BasicFeatures = FALSE, 
                        arcs = TRUE, 
                        padding = 5){
  
  ## blank pitch theme 
  ## set theme for blank pitch
  theme_blankPitch = function(size=12) {
    theme(
      #axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      #axis.ticks.y=element_text(size=size),
      #   axis.ticks=element_blank(),
      axis.ticks.length=unit(0, "lines"),
      #axis.ticks.margin=unit(0, "lines"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.background=element_rect(fill=background_colour, colour=NA),
      legend.key=element_rect(colour=background_colour,fill=background_colour),
      legend.key.size=unit(1.2, "lines"),
      legend.text=element_text(size=size),
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = background_colour, fill = background_colour, size = .5),
      panel.background=element_rect(fill=background_colour,colour=background_colour),
      #       panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.spacing=element_blank(),
      plot.background=element_blank(),
      plot.margin=unit(c(0, 0, 0, 0), "lines"),
      plot.title=element_text(size=size*1.2),
      strip.text.y=element_text(colour=background_colour,size=size,angle=270),
      strip.text.x=element_text(size=size*1))}
  
  ## Basic Plotting 
  p <- ggplot() + xlim(c(0-padding,100+padding)) + ylim(c(0-padding,100+padding)) + theme_blankPitch()
  p
  
  ## Add Middle Third Rectangle 
  # if(middlethird == TRUE){p <- p + geom_rect(aes(xmin=33.3, xmax=66.6, ymin=0, ymax=100), colour = NA, fill = "black", alpha = 0.10)}else{}
  
  ## Basic Features
  p <- p +
    # add the base rectangle of the pitch
    geom_rect(aes(xmin=0, xmax=50, ymin=0, ymax=100), fill = NA, colour = line_colour, size = 1) +
    geom_rect(aes(xmin=50, xmax=100, ymin=0, ymax=100), fill = NA, colour = line_colour, size = 1) +
    # add the 18 yard box defensive
    geom_rect(aes(xmin=0, xmax=17, ymin=21.1, ymax=78.9), fill = grass_colour, colour = line_colour, size = 1) +
    # add the 18 yard box offensive
    geom_rect(aes(xmin=83, xmax=100, ymin=21.1, ymax=78.9), fill = grass_colour, colour = line_colour, size = 1) +
    # add halfway line
    geom_segment(aes(x = 50, y = 0, xend = 50, yend = 100),colour = line_colour,size=1)
  
  ## Add Goals 
  
  ## LINE TYPE
  if(goaltype == "line"){
    p <- p +
      # add the goal Defensive
      geom_segment(aes(x = 0, y = 45.2, xend = 0, yend = 54.8),colour = goal_colour, size = 1) +
      # add the goal offensive
      geom_segment(aes(x = 100, y = 45.2, xend = 100, yend = 54.8),colour = goal_colour, size = 1)
    
  }else{}
  
  
  ## BOX TYPE
  if(goaltype == "box"){
    p <- p +
      # add the goal Defensive
      geom_rect(aes(xmin = -1 , ymin = 45.2, xmax = 0 , ymax =  54.8), fill = grass_colour, colour = line_colour, size = 1) +
      # add the goal offensive
      geom_rect(aes(xmin = 100, ymin = 45.2, xmax = 101, ymax =  54.8), fill = grass_colour, colour = line_colour, size = 1)
  }else{}
  
  ## Add extra features 
  if(BasicFeatures == FALSE){
    p <- p + 
      # add the six yard box Defensive
      geom_rect(aes(xmin=0, xmax=5.8, ymin=36.8, ymax=63.2), fill = grass_colour, colour = line_colour, size = 1)  +
      # add the six yard box offensive
      geom_rect(aes(xmin=94.2, xmax=100, ymin=36.8, ymax=63.2), fill = grass_colour, colour = line_colour, size = 1) +
      # add centre circle
      annotate("path",
               x=50+8.7*cos(seq(0,2*pi,length.out=100)),
               y=50+8.7*sin(seq(0,2*pi,length.out=100)),col = line_colour, size = 1) + 
      # add penalty spot left
      geom_point(aes(x = 11.5 , y = 50), colour = line_colour, size = 0.9) +
      # add penalty spot right
      geom_point(aes(x = 88.5 , y = 50), colour = line_colour, size = 0.9) +
      # add centre spot
      geom_point(aes(x = 50 , y = 50), colour = line_colour, size = 0.9)
  }else{}
  
  ## Add the arcs 
  if(arcs == TRUE){
    p <- p +
      # vertical tram lines
      annotate("path",
               x = (11.5) + 9.2 * cos(seq(-0.3*pi, 0.3*pi, length.out = 500)),
               y = 50 + 9.2 * sin(seq(-0.3*pi, 0.3*pi, length.out = 500)),
               col = line_colour, size = 1) +
      annotate("path",
               x = (88.5) - 9.2 * cos(seq(-0.3*pi, 0.3*pi, length.out = 500)),
               y = 50 - 9.2  * sin(seq(-0.3*pi, 0.3*pi, length.out = 500)),
               col = line_colour, size = 1)
  }else{}
  
  ## Add attacking direction 
  p <- p + 
    geom_segment(aes(x = 40, y = -2, xend = 60, yend = -2),colour = line_colour, arrow = arrow(length = unit(0.1, "cm"), type="closed"))
  ## JdeP --- add at later date 
  
  ## return plot 
  return(p)
  
}



#####################################
# FUNCION PARA LEER FICHERO F27     #
#####################################

leer_f27 <- function(xml_filename){
  

# Leer XML
datos <- read_xml(xml_filename)

meta_datos <- datos %>%
  xml_attrs() %>% 
  t() %>% 
  as_tibble()


root_players <- xml_find_all(datos, '/SoccerFeed/Player') %>%
  xml_attrs() %>%
  map(~as_tibble(t(.))) %>% 
  bind_rows()


node_players <- xml_find_all(datos, '/SoccerFeed/Player') %>% 
  map(~.x %>% 
        xml_children() %>% 
        map(function(.y) {.y %>% 
            xml_attrs() %>% 
            t() %>% 
            as_tibble() %>% 
            mutate(n_passes = xml_text(.y))}) %>% 
        bind_rows())


# 3  Expandir root_players join root and node players  
root_players <- root_players %>% 
  mutate(n = map(node_players, count) %>% unlist()) %>% 
  uncount(weights = n)


## new code rename  player_id player_name
colnames(root_players)[colnames(root_players) == 'player_id'] <- 'player_id_root'
colnames(root_players)[colnames(root_players) == 'player_name'] <- 'player_name_root'




## 4  crear all_data

all_data <- meta_datos %>% 
  mutate(n = nrow(bind_rows(node_players))) %>% 
  uncount(weights = n) %>% 
  bind_cols(root_players) %>% 
  bind_cols(bind_rows(node_players))


## remove temporary structures
rm(node_players,root_players,meta_datos)


##### ADD destination coordinates to df pass receiver
players <- xml_find_all(datos, '/SoccerFeed/Player') %>%
  xml_attrs() %>%
  map(~as_tibble(t(.))) %>% 
  bind_rows()%>%
  select( player_id,x,y)%>%
  rename( player_id1=player_id, x_end=x, y_end=y)




##### ADD new columns all_data left join to player_destination position
all_data <- merge(x = all_data, y = players , by.x = "player_id",by.y = "player_id1", all.x = TRUE)



# CAST TYPES
all_data$n_passes <- as.integer(all_data$n_passes)
all_data$cross <- as.integer(all_data$cross)
all_data$cross_success <- as.integer(all_data$cross_success) 
all_data$pass_success <- as.integer(all_data$pass_success) 
all_data$x <- as.numeric(all_data$x) 
all_data$y <- as.numeric(all_data$y)
all_data$x_end <- as.numeric(all_data$x_end) 
all_data$y_end <- as.numeric(all_data$y_end)


## Renombrar columnas en df final
# renombrar receptores del pase

colnames(all_data)[colnames(all_data) == 'player_id'] <- 'player_id_receptor'
colnames(all_data)[colnames(all_data) == 'player_name'] <- 'player_name_receptor'

colnames(all_data)[colnames(all_data) == 'player_id_root'] <- 'player_id'
colnames(all_data)[colnames(all_data) == 'player_name_root'] <- 'player_name'


return (all_data)
}

#######################
# LEER FICHERO F27    #
#######################


###########################################################################################################
# CAMBIAR EL NOMBRE DEL FICHERO XML AQUI (tiene que estar guardado en la misma ruta que este fichero)
###########################################################################################################

# Real Madrid
# df <- leer_f27("pass_matrix_23_2018_g1009345_t186.xml")

# Leganes
df <- leer_f27("pass_matrix_23_2018_g1009345_t957.xml")


#################################
## ver datos para un jugador
#################################
# player_name pasador
# player_name_receptor receptor


# Si seleccionamos el Real Madrid aqui podemos ver las lineas de Casemiro
df %>%
  filter(player_name=="Casemiro")%>%
  select(player_name,x,y, player_name_receptor,n_passes)


# Si seleccionamos el Leganes aqui podemos ver las lineas de Juanfran
df %>%
  filter(player_name=="Juanfran")%>%
  select(player_name,x,y, player_name_receptor,n_passes)


####################################################
## ONCE TITULAR 11 posicion distinta a Substitute ##
####################################################


titulares <- df %>%
  filter(position !="Substitute")





################################################
## GGPLOT 0 Campo ##
###############################################

crear_Campo()


################################################
## GGPLOT 0 Passes number and colour position ##
################################################

crear_Campo()+
  geom_point(data=titulares, aes(x=x, y=y, colour=position, size= pass_success))




################################################
## GGPLOT 1 Passes number and colour position ##
################################################

 crear_Campo()+
    geom_point(data=titulares, aes(x=x, y=y, colour=position, size= pass_success))+
    geom_text_repel(data=distinct(titulares,player_name,.keep_all=TRUE), aes(x=x, y=y, label=player_name))


################################################
## GGPLOT 2 Passes number and colour position ##
## SCALING PASS SUCCESS  #######################
################################################

crear_Campo()+
  geom_point(data=titulares, aes(x=x, y=y, colour=position, size= pass_success))+
  geom_text_repel(data=distinct(titulares,player_name,.keep_all=TRUE), 
                  aes(x=x, y=y, 
                      label=paste(player_name,pass_success)
                     )
                  ) + 
  scale_size(range = c(0, 10))+
  theme(legend.position="none")




######################################
## GGPLOT 3 -- PASSES ################
######################################

   crear_Campo()+
    geom_curve(data = titulares %>%filter(n_passes>2), 
               aes(x = x, y = y, xend = x_end, yend = y_end,size= n_passes,color=player_name),
               position="nudge",
               lineend="round",
               arrow = arrow(length = unit(0.02, "npc")),
               curvature = -0.2)+
 
    geom_point(data=titulares, aes(x=x, y=y, size= pass_success))+ 
    geom_text_repel(data=distinct(titulares,player_name,.keep_all=TRUE), 
                    aes(x=x, y=y, label=paste(player_name,pass_success)) ,fontface="bold")+
   scale_size(range = c(0, 10))+
   # theme(legend.position="none")+
     ggtitle("Minimo 3 Passes - Player Average Position based on ball contacts") 


######################################
## GGPLOT 4 -- PASSES ################
######################################

crear_Campo()+
  geom_curve(data = titulares %>%filter(n_passes>2), 
             aes(x = x, y = y, xend = x_end, yend = y_end,size= n_passes,color=player_name),
             position="nudge",
             lineend="round",
             arrow = arrow(length = unit(0.02, "npc")),
             curvature = -0.2)+
  scale_size(range = c(0, 3))+
  geom_point(data=titulares, aes(x=x, y=y))+ 
  geom_text_repel(data=distinct(titulares,player_name,.keep_all=TRUE), 
                  aes(x=x, y=y, label=paste(player_name,pass_success)) ,fontface="bold")+
  # theme(legend.position="none")+
  ggtitle("Minimo 3 Passes - Player Average Position based on ball contacts")

  
  

#################################
## GGPLOT 5 GEOM LINE  ##########
#################################  

    crear_Campo()+
    geom_segment(data = titulares %>%filter(n_passes>3), 
               aes(x = x, y = y, xend = x_end, yend = y_end,size= n_passes,color=player_name),
               position="nudge", arrow = arrow(length = unit(0.2, "inches") ) )+
    geom_point(data=titulares, aes(x=x, y=y),size=3)+ 
    geom_text_repel(data=distinct(titulares,player_name,.keep_all=TRUE), aes(x=x, y=y, label=player_name))+
    theme(legend.direction = "vertical", legend.box = "vertical")




######################################
## GGPLOT 6 GEOM LINE  ###############
# with point size=pass_success  ######
######################################

crear_Campo()+
  geom_segment(data = titulares %>%filter(n_passes>3), 
               aes(x = x, y = y, xend = x_end, yend = y_end,size= n_passes,color=player_name),
               position="nudge", arrow = arrow(length = unit(0.2, "inches") ) )+
  geom_point(data=titulares, aes(x=x, y=y, size= pass_success))+ 
  #scale_size(name = "Passes", range = c(3,7))+
  geom_text_repel(data=distinct(titulares,player_name,.keep_all=TRUE), aes(x=x, y=y, label=player_name))+
  theme(legend.direction = "vertical", legend.box = "vertical")
  
  
  

  
  
###################################### 
## GGPLOT 7 GEOM LINE  ###############
# with point size=pass_success  ######
######################################

crear_Campo()+
  geom_segment(data = titulares %>%filter(n_passes>2), 
               aes(x = x, y = y, xend = x_end, yend = y_end,size= n_passes,color=player_name),
               position="nudge"  )+
  geom_point(data=titulares, aes(x=x, y=y, size= pass_success))+ 
  geom_text_repel(data=distinct(titulares,player_name,.keep_all=TRUE), aes(x=x, y=y, label=player_name))+
  theme(legend.direction = "vertical", legend.box = "vertical")

  
  
  