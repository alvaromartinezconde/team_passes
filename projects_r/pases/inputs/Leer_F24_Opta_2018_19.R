

library(xml2)
library(tidyverse)
library(plyr)
library(ggrepel)
library(xslt)






leer_f24 <- function(xml.filename){
  
  #// Define functions to be used ---------------------------------------------------------//
      
      ## event parsing functions
    
    grab.the.qualifiers <- function(xlm.2.spread) {
    
      Value <- ifelse(is.na(Qualifiers.List["value"]), 1, Qualifiers.List["value"])
      temp <- data.frame(Q = as.character(Value), stringsAsFactors = F)
      colnames(temp) <- Qualifiers.List["qualifier_id"]
    
      return(bind_rows(results.temp, temp))
     
    }
    
    ## convert string to numeric 
    
    string_to_numeric <- function(x){as.numeric(as.character(x))}
    
    ## Pick the Maximum (non-NA) Values
    
    pick.out.the.maximum.values <- function(qualifier.values){
        
        max.values <- list()
        for (c in 1:NCOL(qualifier.values)) {
        col.2.test <- qualifier.values[,c]
        max.val <- col.2.test[!is.na(col.2.test)][1]
        max.values <- append(unlist(max.values), max.val)
        }
        results.Q <- t(as.data.frame(max.values))
        colnames(results.Q) <- colnames(qualifier.values)
        return(results.Q)
    }
    
    ## The Main Unpacking Function
    
    
    convert.event.node.row <- function(xml.2.spread){
      
        ## convert the info in the event node header into a dataframe 
        results <- as.data.frame(t(as.data.frame((xml.2.spread$attrs))))
        rownames(results) <- NULL
    
        ## find the number of qualifiers for this event 
        no.of.qualifiers <- lengths(xml.2.spread$value)
        
        if(no.of.qualifiers > 0){
        ## create a list of qualifiers 
        Qualifier.Unpacked.Step1 <- data.frame(stringsAsFactors = F)
      
        ## loop through each qualifer and pull out the info then bind it to the results .. above 
        for (Q in 1:no.of.qualifiers) {
        Qualifier.unpacked <- unlist(xml.2.spread$value[1][[1]][Q])
        Value <- ifelse(is.na(Qualifier.unpacked["value"]), 1, Qualifier.unpacked["value"])
        temp <- data.frame(Q = as.character(Value), stringsAsFactors = F)
        colnames(temp) <- Qualifier.unpacked["qualifier_id"]
        Qualifier.Unpacked.Step1 <- bind_rows(Qualifier.Unpacked.Step1, temp)
        }
        
        ## keep the maximum values in the dataframe (the only none NA values) return as a 
        ## dataframe for use 
         Qualifier.unpacked.df <- pick.out.the.maximum.values(Qualifier.Unpacked.Step1)
         rownames(Qualifier.unpacked.df) <- NULL  
        
        #Qualifier.Unpacked.Step1[1,] <- Qualifier.Unpacked.Step1[is.not.na(Qualifier.Unpacked.Step1)]
        #Qualifier.unpacked.df <- as.data.frame(Qualifier.Unpacked.Step1[1,], stringsAsFactors = F)
        results <- cbind(results, Qualifier.unpacked.df)}
        
        return(results)
    } # end of function 
    
      
  #// Read in the XML File ----------------------------------------------------------------//
  
  pbpParse <- read_xml(xml.filename, encoding = "", as_html = TRUE, options = "NOERROR")
  
  
  #// Spilt the XML File ------------------------------------------------------------------//

    all.event.nodes <- pbpParse %>% 
        xml_find_all('//event') %>% 
        map_df(~list(attrs = list(xml_attrs(.x)), value = list(map(xml_children(.x), xml_attrs))))

  #// Convert all evvents and store in a dataframe ----------------------------------------//

    events <- all.event.nodes %>% 
      split(1:nrow(.)) %>% 
      purrr::map(convert.event.node.row) %>% 
      dplyr::bind_rows()
      
    
  #// convert strings to numerics ---------------------------------------------------------//
    
    events$min     <- string_to_numeric(events$min)
    events$sec     <- string_to_numeric(events$sec)
    events$x       <- string_to_numeric(events$x)
    events$y       <- string_to_numeric(events$y)
	
	# Pass ends
    events$`140`   <- string_to_numeric(events$`140`) # pass end x
    events$`141`   <- string_to_numeric(events$`141`) # pass end y
	# Angles in radians and Lengths
    events$`213`   <- string_to_numeric(events$`213`) # angle in radians
    events$`212`   <- string_to_numeric(events$`212`) # length of ball travel
	
	# GK Coordinates X Y
	events$`230`   <- string_to_numeric(events$`230`) # GK X Coordinate
  events$`231`   <- string_to_numeric(events$`231`) # GK y Coordinate
	
	# GoalMouth Coordinates Y Z
	 events$`102`   <- string_to_numeric(events$`102`) # Goal mouth y co-ordinate
    events$`103`   <- string_to_numeric(events$`103`) # Goal mouth z co-ordinate

	
    events$outcome <- string_to_numeric(events$outcome)  # success yes or no 0 1
    
  #// Return the resulting dataframe -----------------------------------------------------//


    return(events)
    
} # end of parse.f24 function 
   







######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################




#' A pitch plot base that is designed for the OPTA values for x and y of 0-100
#' Uses ggplot and returns a plot ready for further data to be plotted over the top.   
# grass_colour Colour of the grass, Default = "#F9F9F9", HEXCODE or accept colour string i.e. "red" 
# line_colour Colour of the line, Default = "#8F8F8F", HEXCODE or accept colour string i.e. "red" 
# background_colour Colour of the Default = "#F9F9F9", border area, HEXCODE or accept colour string i.e. "red" 
# goaltype Type of goal used. Default "line" and "box" also available
# middlethird Default = FALSE, Adds a shading to the middle third of the pitch 
# BasicFeatures Default = FALSE, If TRUE removes most of the pitch features to have a more minimalist design
# arcs Default = TRUE, adds the D-arcs
# padding Default = 10, adds some padding around the pitch



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



#########################
# event  type_id
#
# '3' Take on     Regates
#########
# Tiros 
#########
# '13' Shot off target  Tiro Fuera
# '14  Post Tiro al poste
# '15' Shot saved Tiro parado o bloqueado
# '16' Goal  Gol


# qualifiers
#########################
# body_dict 
# "15": "head"
# "72": "left foot"
# "20": "right foot"
# "21": "other body part"
#########################
# shot_play_dict 
# '22': 'regular play'
# '23': 'fast break'
# '24': 'set piece'
# '25': 'from corner'
# '26': 'free kick'
# '96': 'corner situation'
# '112': 'scramble'
# '160': 'throw-in set piece'
# '9': 'penalty'
# '28': 'own goal'
#########################



##########################################
##########################################
# Leer PLANTILLAS DE JUGADORES 2018-2019
##########################################
##########################################
######################################################################################
# cambiar aqui el fichero si se usan datos de temporadas anteriores u otras ligas
######################################################################################
x <- read_xml("srml-23-2018-squads.xml")

#nuevo codigo inicio
style <- read_xml("Style.xsl",package="xslt")

# TRANSFORM
# library("xslt") CARGAR LIBRERIA XSLT e instalar nueva librería
new_xml <- xml_xslt(x,style)



recs <-  xml_find_all(new_xml,"//Player")

df_list <- lapply(recs,function(r)
    data.frame(rbind(setNames(xml_text(xml_children(r)),
                              xml_name(xml_children(r)))),
               stringsAsFactors = FALSE))
                                  

jugadores <- bind_rows(df_list)

jugadores <- select(jugadores,uID,Name,Position, country,loan)
                    


# reemplazar p por blanco 
jugadores$uID <- gsub("p", "", jugadores$uID)

# cambiar el nombre de la columna
names(jugadores)[names(jugadores) == 'uID'] <- 'player_id'


##########################################
##########################################
# REAL MADRID 4 LEGANES 1 2018-2019
##########################################
##########################################

# https://www.sofascore.com/es/leganes-real-madrid/EgbsVgb

## CAMBIAR AQUI POR EL NOMBRE DE FICHERO

eventos <- leer_f24("f24-23-2018-1009345-eventdetails.xml")


# CRUZAR LOS EVENTOS Y LOS JUGADORES PARA INCLUIR LAS COLUMNAS DE JUGADOR

eventos <-merge(eventos,jugadores, all.x=TRUE)

# CREAR NUEVA COLUMNA RESULTADO
eventos$resultado <- if_else(eventos$outcome== 0, "No Completado","Completado")








#####################
# REGATES
#####################

regates_real_madrid <- eventos %>%
  filter(type_id==3 & team_id==186)


regates_carvajal<- eventos %>%
  filter(type_id==3 & Name == 'Daniel Carvajal')



####################
# GRAFICA REGATES
####################

# regates carvajal
crear_Campo()+
  geom_point(data=regates_carvajal,aes(x=x,y=y, color=resultado), size=3)+
  geom_text_repel(data=regates_carvajal,aes(x=x,y=y, label=Name))+
  scale_colour_manual(values = c("No Completado" = "red", "Completado" = "green"))



# regates real madrid

crear_Campo()+
  geom_point(data=regates_real_madrid,aes(x=x,y=y, color=resultado), size=3)+
  geom_text_repel(data=regates_real_madrid,aes(x=x,y=y, label=Name))+
  scale_colour_manual(values = c("No Completado" = "red", "Completado" = "green"))


#####################
# GOLES
#####################

goles <- eventos %>%
  filter(type_id==16)


goles_leganes <- eventos %>%
  filter(type_id==16 & team_id==957)

goles_real_madrid <- eventos %>%
  filter(type_id==16 & team_id==186)


####################
# GRAFICA GOLES
####################
# 102 goalmouth y coordinate
crear_Campo()+
  geom_segment(data=goles,aes(x=x,y=y,xend = 100, yend = `102`, colour=Name), size=1)


## goles real madrid
crear_Campo()+
  geom_segment(data=goles_real_madrid,aes(x=x,y=y,xend = 100, yend = `102`, colour=Name), size=1)




