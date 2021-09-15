datos_comuna <- filter(numero_protestas_com, NOM_COM == "Angol" | NOM_COM == "Los Sauces"|
                         NOM_COM == "Ercilla" | NOM_COM == "Renaico" | NOM_COM == "Purén" |
                         NOM_COM == "Los Álamos" | NOM_COM == "Collipulli" | NOM_COM == "Nacimiento" |
                         NOM_COM == "Curanilahue" | NOM_COM == "Cañete" )%>% group_by(NOM_COM)

group_by(datos_comuna, NOM_COM) %>% summarise(protestas_total = sum(protesta))
ungroup(datos_comuna) %>% summarise(protestas_total = sum(protesta))

sapply(datos_comuna[,3:12], sum)
