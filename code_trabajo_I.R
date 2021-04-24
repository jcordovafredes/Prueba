library(geojsonio)
library(sp)
library(spdep)
library(ggplot2)
library(plyr)
library(rgdal)
library(beepr)
source("huff-tools.r")



## Trabajo : caso I

coords_sup=supermercados[,c(5:4)]
shape_disuelto=geojson_read("shape_disuelto.geojson",what = "sp")

# Caso 1: Empresario quiere saber la localizacion de un supermercado de A metros cuadrados


plot(shape_disuelto)
sample=spsample(shape_disuelto, n=10, type="random")
 
#sample=locator()

points(sample, col=2, pch=19)


super_sample_df=as.data.frame(sample)

A=1000 # area
super_sample_df$area=A

super_obs=data.frame(x=supermercados$Longitud,y=supermercados$Latitud, area=area_sim)
tabla_opciones=data.frame(matrix(0,nrow(super_sample_df),5))

for (i in 1:nrow(super_sample_df)){
  
tabla_atractivo_2=rbind(super_obs,super_sample_df[i,])
tabla_atractivo_2$nombre=as.character(c(1:nrow(tabla_atractivo_2)))

# cálculo distancia centroide zonas censales a supermercados
distance=spDists(as.matrix(tabla_zc[,2:3]),as.matrix(tabla_atractivo_2[,1:2]),longlat = T)  
vector_2=as.vector(distance)
# Preparación final de la tabla input para Huff
nombre_tienda_rep=rep(tabla_atractivo_2[,4],each=1643)
nombre_zc_rep=rep(tabla_zc[,1],nrow(tabla_atractivo_2))
df_dist=data.frame(ID_tienda=nombre_tienda_rep, ID_zc=nombre_zc_rep,dist=vector_2)

#tabla_atractivo$nombre=tabla_atractivo$nombre
names(tabla_atractivo_2)[4]="ID_tienda"

tabla_huff_input=plyr::join(df_dist, tabla_atractivo_2,by="ID_tienda",type="inner")
tabla_huff_input$alpha=rep(3,nrow(tabla_huff_input))
tabla_huff_input$beta=rep(1,nrow(tabla_huff_input))


huff_probs <- huff_basic(tabla_huff_input$ID_tienda, tabla_huff_input$area, tabla_huff_input$ID_zc,
                         tabla_huff_input$dist, tabla_huff_input$alpha, tabla_huff_input$beta)
names(huff_probs)[1]="GEOCODIGO"


#  Y sobre la cantidad de personas esperadas que comprarían en nuevo supermercado (market share)

huff_probs_enr=huff_probs #creacion de respaldo

# agregar cantidad de personas a cada ZC
huff_probs_enr=join(shape_gs_urbano@data[,c(10,18)],huff_probs_enr,by="GEOCODIGO")

# agregar área a cada supermercado
# previo se necesita cambiar el nombre de la columna "destination_name" a "ID_tienda"
names(huff_probs_enr)[3]="ID_tienda" #cambiar nombre columna destination_name
huff_probs_enr=join(huff_probs_enr,tabla_atractivo_2[,c(3,4)],by="ID_tienda")
huff_probs_enr=huff_probs_enr[,c(1,2,3,8,4,5,6,7)] # reordenar columnas
huff_probs_enr$personas_tienda=huff_probs_enr$CANT_PER*huff_probs_enr$huff_probability # personas esperadas por super


tienda_cant_personas=aggregate(personas_tienda ~ ID_tienda, data = huff_probs_enr, FUN = sum)
tienda_cant_personas_loc=join(tienda_cant_personas,tabla_atractivo_2, by="ID_tienda")

index_nueva_tienda=which(tienda_cant_personas_loc$ID_tienda=="554")

tabla_opciones[i,]=tienda_cant_personas_loc[index_nueva_tienda,]
print(i)
beep(1)
}
names(tabla_opciones)=c("ID_tienda","personas_tienda","x","y","area")
beep(3)

# exportar tabla_opciones

write.csv(tabla_opciones, "tabla_opciones.csv")






































