
# trabajo caso II

plot(shape_disuelto)
N=5 # numero de iteraciones

tabla_opciones_final=list()

for (i in 1:5){

sample=spsample(shape_disuelto, n=N, type="random")
super_sample_df=as.data.frame(sample)
A=1500

super_obs=data.frame(x=supermercados$Longitud,y=supermercados$Latitud, area=area_sim)
super_sample_df$area=A
tabla_opciones=data.frame(matrix(0,nrow(super_sample_df),5))
tabla_opciones_prom=rep(0,3)
  

tabla_atractivo_2=rbind(super_obs,super_sample_df)
tabla_atractivo_2$nombre=as.character(c(1:nrow(tabla_atractivo_2)))
  
  # calculo distancia centroide zonas censales a supermercados
  distance=spDists(as.matrix(tabla_zc[,2:3]),as.matrix(tabla_atractivo_2[,1:2]),longlat = T)  
  vector_2=as.vector(distance)
  # PreparaciÃ³n final de la tabla input para Huff
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
  
  
  #  Y sobre la cantidad de personas esperadas que comprarÃ­an en nuevo supermercado (market share)
  
  huff_probs_enr=huff_probs #creacion de respaldo
  
  # agregar cantidad de personas a cada ZC
  huff_probs_enr=join(shape_gs_urbano@data[,c(10,18)],huff_probs_enr,by="GEOCODIGO")
  
  # agregar Ã¡rea a cada supermercado
  # previo se necesita cambiar el nombre de la columna "destination_name" a "ID_tienda"
  names(huff_probs_enr)[3]="ID_tienda" #cambiar nombre columna destination_name
  huff_probs_enr=join(huff_probs_enr,tabla_atractivo_2[,c(3,4)],by="ID_tienda")
  huff_probs_enr=huff_probs_enr[,c(1,2,3,8,4,5,6,7)] # reordenar columnas
  huff_probs_enr$personas_tienda=huff_probs_enr$CANT_PER*huff_probs_enr$huff_probability # personas esperadas por super
  
  
  # Que supermercados reciben la mayor cantidad de personas
  tienda_cant_personas=aggregate(personas_tienda ~ ID_tienda, data = huff_probs_enr, FUN = sum)
  tienda_cant_personas_loc=join(tienda_cant_personas,tabla_atractivo_2, by="ID_tienda")
  
  index_nueva_tienda=which(tienda_cant_personas_loc$ID_tienda%in%c((nrow(tabla_atractivo_2)-N+1):nrow(tabla_atractivo_2)))
  
  tabla_opciones=tienda_cant_personas_loc[index_nueva_tienda,]
  tabla_opciones$opcion=i
  tabla_opciones_final[[i]]=tabla_opciones
  mean_caso=mean(tabla_opciones_final[[i]]$personas_tienda)
cat(mean_caso,":",i)
}
beep(3) 

write.csv(tabla_opciones[[]], "tabla_opciones.csv")






