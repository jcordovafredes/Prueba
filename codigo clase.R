library(geojsonio)
library(sp)
library(spdep)
library(ggplot2)
library(plyr)
library(rgdal)


##Simulación al azar
  area_sim=abs(rnorm(553,1000,400))
hist(area_sim,xlab="area [mts cuadrados]", ylab="Frecuencia",main="Histograma datos de area simulados", prob=T)
lines(density(area_sim),col=2,lwd=2)


## Simulación con patrones territoriales

supermercados=read.csv("supermercados.csv")
df=data.frame(a=c(400,3000),den=c(5042,47137))
# regresión lineal
lm_df=lm(a~den,df)
# generación del componente estocástico
res_sim=rnorm(553,0,200)
#simulación del área
area_sim=abs(lm_df$coefficients[1]+lm_df$coefficients[2]*supermercados$densidad_super+res_sim)

area_sim[area_sim<quantile(area_sim,0.25)]=quantile(area_sim,0.25)

# plot de area sim vs densidad
p<-ggplot(data=supermercados, aes(x=densidad_super,y=area_sim)) + 
  geom_point(color="skyblue1")+geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95)+
  xlab("densidad [habs/super]")+ylab("área simulada [mts cuadrados")+ggtitle("Scatter plot area sim vs densidad")

p

## Preparación de datos para el Huff
#1) tabla atributos (destino)


tabla_atractivo=data.frame(supermercados[,4:3],area=area_sim)
tabla_atractivo$nombre=as.character(c(1:nrow(tabla_atractivo)))

# tabla zonas censales (origen)
#l

# shape del GS y zonas censales
shape_gs_urbano=geojson_read("shape_gs_urbano.geojson",what="sp")
# coordenadas de las zonas censales
zc_coords=coordinates(shape_gs_urbano)

# tabla zc con nombre y coordenadas
tabla_zc=data.frame( Geocodigo=shape_gs_urbano$GEOCODIGO,X=zc_coords[,1],Y=zc_coords[,2] )  


# c´laculo distancia centroide zonas censales a supermercados
distance=spDists(as.matrix(tabla_zc[,2:3]),as.matrix(tabla_atractivo[,1:2]),longlat = T)  

vector=read.csv("vector.csv")

# Preparación final de la tabla input para Huff
nombre_tienda_rep=rep(tabla_atractivo[,4],each=1643)
nombre_zc_rep=rep(tabla_zc[,1],553)

##Revisar esto
df_dist=data.frame(ID_tienda=nombre_tienda_rep, ID_zc=nombre_zc_rep,dist=vector[,2])
tabla_atractivo$nombre=tabla_atractivo$nombre
names(tabla_atractivo)[4]="ID_tienda"

tabla_huff_input=plyr::join(df_dist, tabla_atractivo,by="ID_tienda",type="inner")
tabla_huff_input$alpha=rep(1,nrow(tabla_huff_input))
tabla_huff_input$beta=rep(1,nrow(tabla_huff_input))


# Código base para uso de la librería Huff-tools

#El archivo huff-tools.r debe estar en el directorio raíz o especificar su ruta
source("huff-tools.r")

huff_probs <- huff_basic(tabla_huff_input$ID_tienda, tabla_huff_input$area, tabla_huff_input$ID_zc,
                         tabla_huff_input$dist, tabla_huff_input$alpha, tabla_huff_input$beta)


names(huff_probs)[1]="GEOCODIGO"
