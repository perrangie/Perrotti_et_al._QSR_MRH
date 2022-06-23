###Plotting Proxies and BCP Results###

#logic for building name structure: nickname_datacall "read from" path,/,Stacked Plots/nickname_datacall

lake_names=c('Binnewater','Otisville','Appleman','Bonnet','Silver','Stotzel-Leis','Cupola','White','Page-Ladson','Sheelar') #5-2021 Updated Names

lake_abbrv=c('Binn','Otis','Appl','Bont','Silv','Stot','Cup','Whit','Plad','Shee') #12-2020 Updated name order
proxy=c('StronglyCopro','PollPct','charAcc','minSCD')
fungal_values=c('Conc','Pct')

fungi_conc=c(FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE) 
char_plots=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE) 
char_bckgr=c(FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE) 
fungi_both=c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,TRUE)
fungi_logtrans=FALSE 

#Handling unique labeling for irregular cases ###Updating for manuscript submission CAK 1.5.2022

char_sub = c("Robinson et al. (2005)","Robinson et al. (2005)","Gill et al. (2009)","Fastovich et al., (2020)","Gill et al., (2014)","Watson et al., (2018)","Jones et al., (2018)",NA,"this study",NA)
fung_sub = c("Robinson et al., (2005)","Robinson et al., (2005)","Gill et al. (2009)","recounted this study","recounted this study","recounted this study","recounted this study","recounted this study","Halligan et al., (2016)","recounted this study")
poll_sub = c("Robinson et al., (2005)","Robinson et al., (2005)","Gill et al., (2009)","Fastovich et al., (2020)","Gill et al., (2014)","Watson et al., (2018)","Jones et al., (2018)",NA,"Perrotti (2018)","Krause et al. (in prep)")
mscd_sub = c("this study","this study","Gill et al., (2009)","Fastovich et al., (2020)","Gill et al., (2014)","Watson et al., (2018)","Jones et al., (2018)",NA,"this study","Krause et al. (in prep)")

char_labels = c(rep("MicroCharcoal mm2/cm3",2),rep("Charcoal Pieces/cm2/year",8))
fungi_labels = c(rep("Sporormiella",2),rep("Strong Copr.",8))
char_micr= c(rep('char_mm2cm3',2),rep('charAcc',length(char_plots)-2))
proxy_type = c(rep('Sporormiella',2),rep('StronglyCopro',8))


plot_order=c('Strg.Copr.Fungi','HW Pollen%','minSCD','CharAcc') #adjusting to add charcoal background accum. CAK 1.5.2022
save_pdf=TRUE #Logical control for saving pdfs of plots. 16.10.2020 CAK

uniform_scaling=TRUE #Logical control for uniform scaling of individual lake plots. 16.10.2020 CAK
scale_minBP=8000  #Values controlling limits of plots. 16.10.2020 CAK
scale_maxBP=20000 #Values controlling limits of plots. 16.10.2020 CAK

record_box=FALSE #logical controlling drawing boxes to delimit records
record_lines=FALSE #logical controlling drawing lines to delimit records
inverse_box=TRUE #logical controlling inverse boxes
trim_values=FALSE #logical controlling trimming and marking excessively large values (Stotzel-Leis and Sheelar)

#Use the martix to pull relevant values for bulding matrices and filling them.

age_matrix=matrix(nrow=length(proxy),ncol=length(lake_names))
age_matrix=as.data.frame(age_matrix)
colnames(age_matrix)=lake_abbrv
row.names(age_matrix)=plot_order

#Run separate for loops to pull each lake's data. 

for(i in 1:length(lake_names)){
  age_vector=vector(length=length(proxy))
  if(fungi_conc[i]==FALSE){
    fungi=read.csv(paste0('Data/Stacked_Plots/',lake_abbrv[i],'_',proxy_type[i],'Pct.csv')) #Adjusted to accommodate Sporormiella-only records. 16-8-2021
    StrC_age=fungi[,1]
  } else{ 
    fungi=read.csv(paste0('Data/Stacked_Plots/',lake_abbrv[i],'_',proxy_type[i],'Conc.csv')) #Adjusted to accommodate Sporormiella-only records. 16-8-2021
    StrC_age=fungi[,1]
  }
  age_matrix[1,i]=length(StrC_age)
}

for(i in 1:length(lake_names)){
  if(char_plots[i]==TRUE){
    Char=read.csv(paste0('Data/Stacked_Plots/',lake_abbrv[i],'_',char_micr[i],'.csv')) #Adjusted to deal with microcharcoal mm2/cm3 values
    Char_age=Char[,1]
    age_matrix[4,i]=length(Char_age)
  } else {
    age_matrix[4,i]=NA
  }
}

for(i in 1:length(lake_names)){
  if(lake_names[i]=="White"){
    K_Pollen=read.csv(paste0('Data/Stacked_Plots/',lake_abbrv[i],'Krause','_',proxy[2],'.csv')) #Have to keep these separate in the final polygon plotting.
    P_Pollen=read.csv(paste0('Data/Stacked_Plots/',lake_abbrv[i],'Perrotti','_',proxy[2],'.csv')) #Keeping these separate in final polygon polotting
    Pollen=rbind(K_Pollen,P_Pollen) #One matrix for ease of use, but can grab by rows later?
    Poll_age=Pollen[,1]
    age_matrix[2,i]=length(Poll_age)
  } else {
    Pollen=read.csv(paste0('Data/Stacked_Plots/',lake_abbrv[i],'_',proxy[2],'.csv'))
    Poll_age=Pollen[,1]
    age_matrix[2,i]=length(Poll_age)
  }
}

for(i in 1:length(lake_names)){
  if(lake_names[i]=="White"){
    K_minSCD=read.csv(paste0('Data/Stacked_Plots/',lake_abbrv[i],'Krause','_',proxy[4],'.csv'))
    P_minSCD=read.csv(paste0('Data/Stacked_Plots/',lake_abbrv[i],'Perrotti','_',proxy[4],'.csv'))
    minSCD=rbind(K_minSCD,P_minSCD)
    SCD_age=minSCD[,1]
    age_matrix[3,i]=length(SCD_age)
  } else {
    minSCD=read.csv(paste0('Data/Stacked_Plots/',lake_abbrv[i],'_',proxy[4],'.csv'))
    SCD_age=minSCD[,1]
    age_matrix[3,i]=length(SCD_age)
  }
}

#Make a string to capture column names, useful for plotting later on and organizes flow here. 


plot_colnames=c('Char_age','Charcoal Accumulation cm2/yr','Charcoal Background Accum cm2/yr','HWpoll_age','HW Pollen Posterior Prob.','HW Pollen Percent','StrC_age','Strongly Coprophilic Fungi conc. Posterior Prob.','Strongly Coprophilic Fungi Conc.','Strongly Coprophilic Fungi perc. Posterior Prob.','Strongly Coprophilic Fungi Perc.','minSCD_age','minSCD Posterior Prob.','minSCD')
alt1_colnames=c('Char_age','MicroCharcoal Accumulation mm2/cm3','Charcoal Background Accum cm2/yr','HWpoll_age','HW Pollen Posterior Prob.','HW Pollen Percent','StrC_age','Sporormiellia conc. Posterior Prob.','Sporormiella Conc.','Sporormiella perc. Posterior Prob.','Sporormiella Perc.','minSCD_age','minSCD Posterior Prob.','minSCD')

#Since we're doing it lake by lake, it makes the most sense to build the matrices by hand and then pass them down the loops.

Bonnet_data=matrix(nrow=max(age_matrix$Bont,na.rm=TRUE),ncol=length(plot_colnames))
Bonnet_data=as.data.frame(Bonnet_data)
colnames(Bonnet_data)=plot_colnames 

Cupola_data=matrix(nrow=max(age_matrix$Cup,na.rm=TRUE),ncol=length(plot_colnames))
Cupola_data=as.data.frame(Cupola_data)
colnames(Cupola_data)=plot_colnames

Sheelar_data=matrix(nrow=max(age_matrix$Shee,na.rm=TRUE),ncol=length(plot_colnames))
Sheelar_data=as.data.frame(Sheelar_data)
colnames(Sheelar_data)=plot_colnames

Silver_data=matrix(nrow=max(age_matrix$Silv,na.rm=TRUE),ncol=length(plot_colnames))
Silver_data=as.data.frame(Silver_data)
colnames(Silver_data)=plot_colnames

Stotzel_Leis_data=matrix(nrow=max(age_matrix$Stot,na.rm=TRUE),ncol=length(plot_colnames))
Stotzel_Leis_data=as.data.frame(Stotzel_Leis_data)
colnames(Stotzel_Leis_data)=plot_colnames

Page_Ladson_data=matrix(nrow=max(age_matrix$Plad,na.rm=TRUE,na.rm=TRUE),ncol=length(plot_colnames))
Page_Ladson_data=as.data.frame(Page_Ladson_data)
colnames(Page_Ladson_data)=plot_colnames

Appleman_data=matrix(nrow=max(age_matrix$Appl,na.rm=TRUE,na.rm=TRUE),ncol=length(plot_colnames))
Appleman_data=as.data.frame(Appleman_data)
colnames(Appleman_data)=plot_colnames

White_Pond_data=matrix(nrow=max(age_matrix$Whit,na.rm=TRUE,na.rm=TRUE),ncol=length(plot_colnames))
White_Pond_data=as.data.frame(White_Pond_data)
colnames(White_Pond_data)=plot_colnames

Binnewater_data=matrix(nrow=max(age_matrix$Binn,na.rm=TRUE,na.rm=TRUE),ncol=length(plot_colnames))
Binnewater_data=as.data.frame(Binnewater_data)
colnames(Binnewater_data)=alt1_colnames

Otis_data=matrix(nrow=max(age_matrix$Otis,na.rm=TRUE,na.rm=TRUE),ncol=length(plot_colnames))
Otis_data=as.data.frame(Otis_data)
colnames(Otis_data)=alt1_colnames


Lake_table=lapply(1:length(lake_names),function(x) {
  lapply(1, function(y) {
    list()
  })
})

#Fill the array with the empty matrices.

Lake_table[[1]]=Binnewater_data
Lake_table[[2]]=Otis_data
Lake_table[[3]]=Appleman_data
Lake_table[[4]]=Bonnet_data
Lake_table[[5]]=Silver_data
Lake_table[[6]]=Stotzel_Leis_data
Lake_table[[7]]=Cupola_data
Lake_table[[8]]=White_Pond_data
Lake_table[[9]]=Page_Ladson_data
Lake_table[[10]]=Sheelar_data

#Now we have array/matrices to save the data in, going to run loops to pull data and fill the matrices. Give it appropriate names. 

Lake_table=as.array(Lake_table)
names(Lake_table)=lake_names

minmax_columns=c('Age_min','Age_max','Char_min','Char_max','HWpoll_min','HWpoll_max','StrC_min','StrC_max','SCD_min','SCD_max') #FIX THIS TO ADD CHAR_BCKG ###CAK 1.5.2022
plot_minmax=matrix(ncol=length(minmax_columns),nrow=length(lake_names))
plot_minmax=as.data.frame(plot_minmax)
colnames(plot_minmax)=minmax_columns
row.names(plot_minmax)=lake_abbrv

#Because there's different ages for each proxy, need to pool ages first, then pull minmax. Need to pull/place columns correctly.

pooled_ages=matrix(ncol=length(proxy)*length(lake_names),nrow=max(age_matrix,na.rm=TRUE))
pooled_ages=as.data.frame(pooled_ages)
age_sequence=seq(1,(ncol(pooled_ages)),length(proxy))


#FOR LOOP TO PULL CHARCOAL DATA
for(i in 1:length(lake_names)){
  if(char_plots[i]==TRUE){
    Char=read.csv(paste0('Data/Stacked_Plots/',lake_abbrv[i],'_',char_micr[i],'.csv'))
    if(char_bckgr[i]==TRUE){
      Char_data=Char[,c(1,3,4)]
      Char_bkg=c(Char_data[,3],rep(NA,nrow(Lake_table[[i]])-nrow(Char_data))) #ADD PULL FOR BACKGROUND HERE CAK 1.5.2022
    } else {
      Char_data=Char[,c(1,3)]
      Char_bkg=rep(NA,nrow(Char_data))
    }
    
    Char_age=c(Char_data[,1],rep(NA,nrow(Lake_table[[i]])-nrow(Char_data)))
    Char_acc=c(Char_data[,2],rep(NA,nrow(Lake_table[[i]])-nrow(Char_data)))
    
    Char_min=min(Char_acc,na.rm=TRUE)
    Char_max=max(Char_acc,na.rm=TRUE)
    if(i>2){
      Lake_table[[i]][,plot_colnames[1]]=Char_age
      Lake_table[[i]][,plot_colnames[2]]=Char_acc
      Lake_table[[i]][,plot_colnames[3]]=Char_bkg

    } else {
      Lake_table[[i]][,alt1_colnames[1]]=Char_age
      Lake_table[[i]][,alt1_colnames[2]]=Char_acc
    }
    plot_minmax[i,3]=Char_min
    plot_minmax[i,4]=Char_max
    
    Char_page=c(Char_data[,1],rep(NA,nrow(pooled_ages)-nrow(Char_data)))
    pooled_ages[,age_sequence[i]]=Char_page
    
  } else {
    Lake_table[[i]][,plot_colnames[1]]=rep(NA,nrow(Lake_table[[i]]))
    Lake_table[[i]][,plot_colnames[2]]=rep(NA,nrow(Lake_table[[i]]))
    plot_minmax[i,3]=NA
    plot_minmax[i,4]=NA
    pooled_ages[,age_sequence[i]]=rep(NA,nrow(pooled_ages))
  }
}
#FOR LOOP TO PULL HWPOLLEN DATA
for(i in 1:length(lake_names)){
  if(lake_names[i]=="White"){
    K_Pollen=read.csv(paste0('Data/Stacked_Plots/',lake_abbrv[i],'Krause','_',proxy[2],'.csv')) #Have to keep these separate in the final polygon plotting.
    P_Pollen=read.csv(paste0('Data/Stacked_Plots/',lake_abbrv[i],'Perrotti','_',proxy[2],'.csv')) #Keeping these separate in final polygon polygon plotting
    K_Prows=c(1:nrow(K_Pollen))
    P_Prows=c((nrow(K_Pollen)+1):(nrow(K_Pollen)+nrow(P_Pollen)))
    Pollen=rbind(K_Pollen,P_Pollen) 
  } else {
    Pollen=read.csv(paste0('Data/Stacked_Plots/',lake_abbrv[i],'_',proxy[2],'.csv'))
  }
  Pollen[is.na(Pollen)]=0
  Poll_age=c(Pollen[,1],rep(NA,nrow(Lake_table[[i]])-nrow(Pollen)))
  Poll_prob=c(Pollen[,2],rep(NA,nrow((Lake_table[[i]]))-nrow(Pollen)))
  Poll_pct=c(Pollen[,3],rep(NA,nrow(Lake_table[[i]])-nrow(Pollen)))
  Poll_min=min(Poll_pct,na.rm=TRUE)
  Poll_max=max(Poll_pct,na.rm=TRUE)
  Lake_table[[i]][,plot_colnames[4]]=Poll_age
  Lake_table[[i]][,plot_colnames[5]]=Poll_prob
  Lake_table[[i]][,plot_colnames[6]]=Poll_pct
  plot_minmax[i,5]=Poll_min
  plot_minmax[i,6]=Poll_max
  
  Poll_page=c(Pollen[,1],rep(NA,nrow(pooled_ages)-nrow(Pollen)))
  pooled_ages[,age_sequence[i]+1]=Poll_page
}

#FOR LOOP TO PULL FUNGAL DATA
for(i in 1:length(lake_names)){
  if(fungi_conc[i]==TRUE){
    
    fungi=read.csv(paste0('Data/Stacked_Plots/',lake_abbrv[i],'_',proxy_type[i],'Conc.csv')) ##Rotating through lake_abbrv[i] but need to fix proxy[1] to point at StronglyCorpo
    fungi[is.na(fungi)]=0
    StrC_age=c(fungi[,1],rep(NA,nrow(Lake_table[[i]])-nrow(fungi)))
    StrC_prob=c(fungi[,2],rep(NA,nrow((Lake_table[[i]]))-nrow(fungi)))
    StrC_conc=c(fungi[,3],rep(NA,nrow(Lake_table[[i]])-nrow(fungi)))
    StrC_min=min(StrC_conc,na.rm=TRUE)
    StrC_max=max(StrC_conc,na.rm=TRUE)
    if(i>2){
      Lake_table[[i]][,plot_colnames[7]]=StrC_age
      Lake_table[[i]][,plot_colnames[8]]=StrC_prob
      Lake_table[[i]][,plot_colnames[9]]=StrC_conc
    } else {
      Lake_table[[i]][,alt1_colnames[7]]=StrC_age
      Lake_table[[i]][,alt1_colnames[8]]=StrC_prob
      Lake_table[[i]][,alt1_colnames[9]]=StrC_conc
    }

    plot_minmax[i,7]=StrC_min
    plot_minmax[i,8]=StrC_max
    
    if(lake_abbrv[i]=="Whit"){
      
    } else {
      fungi_pct=read.csv(paste0('Data/Stacked_Plots/',lake_abbrv[i],'_',proxy_type[i],'Pct.csv'))
      fungi_pct[is.na(fungi_pct)]=0
      StrC_prob=c(fungi_pct[,2],rep(NA,nrow((Lake_table[[i]]))-nrow(fungi_pct)))
      StrC_pct=c(fungi_pct[,3],rep(NA,nrow(Lake_table[[i]])-nrow(fungi_pct)))
      if(i>2){
        Lake_table[[i]][,plot_colnames[10]]=StrC_prob
        Lake_table[[i]][,plot_colnames[11]]=StrC_pct
      } else {
        Lake_table[[i]][,alt1_colnames[10]]=StrC_prob
        Lake_table[[i]][,alt1_colnames[11]]=StrC_pct
      }

      
    }
    
  } else { 
    
    fungi=read.csv(paste0('Data/Stacked_Plots/',lake_abbrv[i],'_',proxy_type[i],'Pct.csv')) #Rotating through lake_abbrv[i] but need to fix proxy[1] to point at StronglyCorpo
    fungi[is.na(fungi)]=0
    StrC_age=c(fungi[,1],rep(NA,nrow(Lake_table[[i]])-nrow(fungi)))
    StrC_prob=c(fungi[,2],rep(NA,nrow((Lake_table[[i]]))-nrow(fungi)))
    StrC_pct=c(fungi[,3],rep(NA,nrow(Lake_table[[i]])-nrow(fungi)))
    StrC_min=min(StrC_pct,na.rm=TRUE)
    StrC_max=max(StrC_pct,na.rm=TRUE)
    if(i>2){
      Lake_table[[i]][,plot_colnames[7]]=StrC_age
      Lake_table[[i]][,plot_colnames[10]]=StrC_prob #Adjusting to fit uniform array table format
      Lake_table[[i]][,plot_colnames[11]]=StrC_pct #Adjusting to fit uniform array tabl
    } else {
      Lake_table[[i]][,alt1_colnames[7]]=StrC_age
      Lake_table[[i]][,alt1_colnames[10]]=StrC_prob #Adjusting to fit uniform array table format
      Lake_table[[i]][,alt1_colnames[11]]=StrC_pct #Adjusting to fit uniform array tabl
    }

    plot_minmax[i,7]=StrC_min
    plot_minmax[i,8]=StrC_max
    
    if(fungi_both[i]==TRUE){
      fungi=read.csv(paste0('Data/Stacked_Plots/',lake_abbrv[i],'_',proxy_type[i],'Conc.csv')) ##Rotating through lake_abbrv[i] but need to fix proxy[1] to point at StronglyCorpo
      fungi[is.na(fungi)]=0
      StrC_age=c(fungi[,1],rep(NA,nrow(Lake_table[[i]])-nrow(fungi)))
      StrC_prob=c(fungi[,2],rep(NA,nrow((Lake_table[[i]]))-nrow(fungi)))
      StrC_conc=c(fungi[,3],rep(NA,nrow(Lake_table[[i]])-nrow(fungi)))
      StrC_min=min(StrC_conc,na.rm=TRUE)
      StrC_max=max(StrC_conc,na.rm=TRUE)
      if(i>2){
        Lake_table[[i]][,plot_colnames[7]]=StrC_age
        Lake_table[[i]][,plot_colnames[8]]=StrC_prob
        Lake_table[[i]][,plot_colnames[9]]=StrC_conc
      } else {
        Lake_table[[i]][,alt1_colnames[7]]=StrC_age
        Lake_table[[i]][,alt1_colnames[8]]=StrC_prob
        Lake_table[[i]][,alt1_colnames[9]]=StrC_conc
      }

    }
    
  }
  StrC_page=c(fungi[,1],rep(NA,nrow(pooled_ages)-nrow(fungi)))
  pooled_ages[,age_sequence[i]+2]=StrC_page
}

#FOR LOOP TO PULL MINSCD DATA
for(i in 1:length(lake_names)){
  if(lake_names[i]=="White"){
    K_minSCD=read.csv(paste0('Data/Stacked_Plots/',lake_abbrv[i],'Krause','_',proxy[4],'.csv'))
    P_minSCD=read.csv(paste0('Data/Stacked_Plots/',lake_abbrv[i],'Perrotti','_',proxy[4],'.csv'))
    minSCD_data=rbind(K_minSCD,P_minSCD)
    K_Srows=c(1:nrow(K_minSCD))
    P_Srows=c((nrow(K_minSCD)+1):(nrow(K_minSCD)+nrow(P_minSCD)))
  } else {
    minSCD_data=read.csv(paste0('Data/Stacked_Plots/',lake_abbrv[i],'_',proxy[4],'.csv'))
  }
  minSCD_data[is.na(minSCD_data)]=0
  SCD_age=c(minSCD_data[,1],rep(NA,nrow(Lake_table[[i]])-nrow(minSCD_data)))
  SCD_prob=c(minSCD_data[,2],rep(NA,nrow((Lake_table[[i]]))-nrow(minSCD_data)))
  minSCD=c(minSCD_data[,3],rep(NA,nrow(Lake_table[[i]])-nrow(minSCD_data)))
  SCD_min=min(minSCD_data,na.rm=TRUE)
  SCD_max=max(minSCD_data,na.rm=TRUE)
  Lake_table[[i]][,plot_colnames[12]]=SCD_age  ###Tagging fixes for plotting all lake pcts - going to have to add 2 to the column name assignments here to compensate for adding columns. - CHECK
  Lake_table[[i]][,plot_colnames[13]]=SCD_prob
  Lake_table[[i]][,plot_colnames[14]]=minSCD
  plot_minmax[i,9]=SCD_min
  plot_minmax[i,10]=SCD_max
  
  SCD_page=c(minSCD_data[,1],rep(NA,nrow(pooled_ages)-nrow(minSCD_data)))
  pooled_ages[,age_sequence[i]+3]=SCD_page
  
}

for(i in 1:length(lake_names)){
  age_min=min(pooled_ages[,(1:4)+(4*(i-1))],na.rm=TRUE)
  age_max=max(pooled_ages[,(1:4)+(4*(i-1))],na.rm=TRUE)
  plot_minmax[i,1]=age_min
  plot_minmax[i,2]=age_max
}


age_depth=plot_minmax[,1:2]*-1 #Reversing ages into depths for plotting.

#Function to prep transparent numbers.

## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}

#Set color palette

library(wesanderson)

col_base=wes_palette('Zissou1',100,type="continuous")
minSCD_col=col_base[30]
HWpol_col=col_base[60]
HWpol_col_var1=col_base[40]
StrC_col=col_base[1]

#Make transparency/set colors

minSCD_col=t_col(minSCD_col,perc=30,name="minSCD.col")
prSCD_col=t_col(minSCD_col,perc=60,name="minSCD.col")

HWpol_col=t_col(HWpol_col,perc=30,name="HWpol.col")
prPOL_col=t_col(HWpol_col,perc=60,name="HWpol.col")

HWpol_col_var1=t_col(HWpol_col_var1,perc=30,name="HWpol_var.col")
prPOLv1_col=t_col(HWpol_col_var1,perc=60,name="HWpol_var.col")

StrC_col=t_col(StrC_col,perc=30,name="StrC.col")
prSrC_col=t_col(StrC_col,perc=60,name="StrC.col")
record_col=t_col("gray",perc=60,name="rec.col")

#it makes sense to set the y-values here and feed the 

#Lake_table
breaks=0.5
par(mar=c(7,6,6,0))
for(i in 1:length(lake_names)){
  if(save_pdf==TRUE){ #Logical controlling saving pdf files.
    setEPS()
    pdf(paste0(lake_names[i],'stackplot','.pdf'),width=14.0,height=12.0)
    par(mar=c(7,6,6,0))
  }
  if(uniform_scaling==TRUE & char_plots[i]==TRUE){
    plot(0,0,col=NA,ylim=rev(c(scale_minBP*-1,scale_maxBP*-1)),xlim=c(-0.5,6),axes=FALSE,ann=FALSE) #Plotting using uniform scaling derived from parameters set above.
  } else {
    
    plot(0,0,col=NA,ylim=rev(c(scale_minBP*-1,scale_maxBP*-1)),xlim=c(-0.5,4),axes=FALSE,ann=FALSE) 

  }

  
  title(main=paste0(lake_names[i]),line=4.5,cex=3)
  title(xlab='Paleoecological Proxies and BCP Posterior Probabilities',line=5.0,cex.lab=1.5)
  title(ylab='KA',line=3.5)
  axis(2,las=1,at=1000*(signif(min(age_depth$Age_max),2)/1000):(signif(max(age_depth$Age_min),1)/1000),labels=c(-1*(signif(min(age_depth$Age_max),2)/1000):(signif(max(age_depth$Age_min),1)/1000)))

  #Marking top and bottom of sequences to draw boxes or lines. 17.10.2020
  
  if(char_plots[i]==TRUE){
    Char_base=max(Lake_table[[i]][,1],na.rm=TRUE)*-1
    Char_top=min(Lake_table[[i]][,1],na.rm=TRUE)*-1
  }
  
  Pol_base=max(Lake_table[[i]][,4],na.rm=TRUE)*-1
  Pol_top=min(Lake_table[[i]][,4],na.rm=TRUE)*-1
  
  Fungi_base=max(Lake_table[[i]][,7],na.rm=TRUE)*-1
  Fungi_top=min(Lake_table[[i]][,7],na.rm=TRUE)*-1
  
  minSCD_base=max(Lake_table[[i]][,12],na.rm=TRUE)*-1
  minSCD_top=min(Lake_table[[i]][,12],na.rm=TRUE)*-1
  
  #We can try to mark the records by setting them inside of boxes. Should happen before plotting other polygons so it is in background. 17.10.2020 CAK
  if(record_box==TRUE){
    if(char_plots[i]==TRUE){
      Charbox_x=c(0,0,1,1)
      Charbox_y=c(c(Char_base,Char_top,Char_top,Char_base))
      polygon(Charbox_x,Charbox_y,col="white",lty=0)
    }  
    Polbox_x=c(1,1,2+breaks,2+breaks)
    Polbox_y=c(Pol_base,Pol_top,Pol_top,Pol_base)
    polygon(Polbox_x,Polbox_y,col="white",lty=0)
  
    Funbox_x=c(3-breaks,3-breaks,4,4)
    Funbox_y=c(Fungi_base,Fungi_top,Fungi_top,Fungi_base)
    polygon(Funbox_x,Funbox_y,col="white",lty=0)
    
    SCDbox_x=c(4,4,5.5,5.5)
    SCDbox_y=c(minSCD_base,minSCD_top,minSCD_top,minSCD_base)
    polygon(SCDbox_x,SCDbox_y,col="white",lty=0)
  }
  
  #Drawing inverse boxes
  if(inverse_box==TRUE){
    if(char_plots[i]==TRUE){ #Controlling plotting of charcoal record bounds.
      ch_topbox_x=c(4,4,5.5,5.5)
      ch_botbox_x=c(4,4,5.5,5.5)
      
      if(Char_top>=scale_minBP*-1){
      } else {
        ch_topbox_y=c(Char_top,scale_minBP*-0.1,scale_minBP*-0.1,Char_top)
        polygon(ch_topbox_x,ch_topbox_y,col=record_col,lty=0)
      }
      
      if(Char_base<=scale_maxBP*-1){
      } else {
        ch_botbox_y=c(Char_base,scale_maxBP*-10,scale_maxBP*-10,Char_base)
        polygon(ch_botbox_x,ch_botbox_y,col=record_col,lty=0)
      }
      
    }  
    
    #Pollen record bounds.
    p_topbox_x=c(1,1,2.5,2.5)
    p_botbox_x=c(1,1,2.5,2.5)
    
    if(Pol_top>=scale_minBP*-1){
    } else {
      p_topbox_y=c(Pol_top,scale_minBP*-0.1,scale_minBP*-0.1,Pol_top)
      polygon(p_topbox_x,p_topbox_y,col=record_col,lty=0)
    }
    
    if(Pol_base<=scale_maxBP*-1){
    } else {
      p_botbox_y=c(Pol_base,scale_maxBP*-10,scale_maxBP*-10,Pol_base)
      polygon(p_botbox_x,p_botbox_y,col=record_col,lty=0)
    }
    
    #Coprophilic fungi record bounds.
    f_topbox_x=c(-0.5,-0.5,1,1)
    f_botbox_x=c(-0.5,-0.5,1,1)
    
    if(Fungi_top>=scale_minBP*-1){
    } else {
      f_topbox_y=c(Fungi_top,scale_minBP*-0.1,scale_minBP*-0.1,Fungi_top)
      polygon(f_topbox_x,f_topbox_y,col=record_col,lty=0)
    }
    
    if(Fungi_base<=scale_maxBP*-1){
    } else {
      f_botbox_y=c(Fungi_base,scale_maxBP*-10,scale_maxBP*-10,Fungi_base)
      polygon(f_botbox_x,f_botbox_y,col=record_col,lty=0)
    }
    
    #minSCD record bounds
    s_topbox_x=c(2.5,2.5,4,4)
    s_botbox_x=c(2.5,2.5,4,4)
    
    if(minSCD_top>=scale_minBP*-1){
    } else {
      s_topbox_y=c(minSCD_top,scale_minBP*-0.1,scale_minBP*-0.11,minSCD_top)
      polygon(s_topbox_x,s_topbox_y,col=record_col,lty=0)
    }
    
    if(minSCD_base<=scale_maxBP*-1){
    } else {
      s_botbox_y=c(minSCD_base,scale_maxBP*-10,scale_maxBP*-10,minSCD_base)
      polygon(s_botbox_x,s_botbox_y,col=record_col,lty=0)
    }
    ###The solution applied above is not elegant or entirely sytematic (why 0.1 and 10?), but it works.
  }
  
  #Drawing lines for beginning/ending of records. 17.10.2020 CAK
  if(record_lines==TRUE){
    #Charcoal data top and bottom lines
    if(char_plots[i]==TRUE){
      lines(c(0,1),c(Char_base,Char_base),lty=3)
      lines(c(0,1),c(Char_top,Char_top),lty=3)
    }
    
    #Pollen data top and bottom lines
    lines(c(1,2+breaks),c(Pol_base,Pol_base),lty=3)
    lines(c(1,2+breaks),c(Pol_top,Pol_top),lty=3)
    
    #Fungi data top and bottom lines
    lines(c(3-breaks,4),c(Fungi_base,Fungi_base),lty=3)
    lines(c(3-breaks,4),c(Fungi_top,Fungi_top),lty=3)
    
    #minSCD data top and bottom
    lines(c(4,5.5),c(minSCD_base,minSCD_base),lty=3)
    lines(c(4,5.5),c(minSCD_top,minSCD_top),lty=3)
  }  
  #polygons for posterior probs
  
  if(names(Lake_table[i])=="White"){ #Accomodating separate counts and variable age-depth models for White
    K_Polpr_xmax=c(0,(Lake_table[[i]][K_Prows,5]/2)*-1,0)+1.5 
    K_Polpr_xmin=c(rep(0,length(K_Polpr_xmax)))+1.5
    K_Polpr_x=c(K_Polpr_xmax,K_Polpr_xmin)
    K_Polpr_y=c(max(Lake_table[[i]][K_Prows,4]*-1,na.rm=TRUE),Lake_table[[i]][K_Prows,4]*-1,min(Lake_table[[i]][K_Prows,4]*-1,na.rm=TRUE))
    K_Polpr_y=c(K_Polpr_y,rev(K_Polpr_y))
    polygon(K_Polpr_x,K_Polpr_y,lty=3,col=prPOLv1_col,lwd=1.25) 
    
    P_Polpr_xmax=c(0,(Lake_table[[i]][P_Prows,5]/2)*-1,0)+1.5 
    P_Polpr_xmin=c(rep(0,length(P_Polpr_xmax)))+1.5
    P_Polpr_x=c(P_Polpr_xmax,P_Polpr_xmin)
    P_Polpr_y=c(max(Lake_table[[i]][P_Prows,4]*-1,na.rm=TRUE),Lake_table[[i]][P_Prows,4]*-1,min(Lake_table[[i]][P_Prows,4]*-1,na.rm=TRUE))
    P_Polpr_y=c(P_Polpr_y,rev(P_Polpr_y))
    polygon(P_Polpr_x,P_Polpr_y,lty=3,col=prPOL_col,lwd=1.25) 
  } else {
    Polpr_xmax=c(0,(Lake_table[[i]][,5]/2)*-1,0)+1.5 
    Polpr_xmin=c(rep(0,length(Polpr_xmax)))+1.5
    Polpr_x=c(Polpr_xmax,Polpr_xmin)
    Polpr_y=c(max(Lake_table[[i]][,4]*-1,na.rm=TRUE),Lake_table[[i]][,4]*-1,min(Lake_table[[i]][,4]*-1,na.rm=TRUE))
    Polpr_y=c(Polpr_y,rev(Polpr_y))
    polygon(Polpr_x,Polpr_y,lty=3,col=prPOL_col,lwd=1.25)
  }
  
  if(fungi_conc[i]==TRUE){ #If we're plotting by concentration..
    SCpr_xmax=c(0,(Lake_table[[i]][,8]/2)*-1,0) #Use the old column location
  } else { #If we're plotting by %...
    SCpr_xmax=c(0,(Lake_table[[i]][,10]/2)*-1,0) #Use new column location.
  }
  SCpr_xmin=c(rep(0,length(SCpr_xmax)))
  SCpr_x=c(SCpr_xmax,SCpr_xmin)
  SCpr_y=c(max(Lake_table[[i]][,7]*-1,na.rm=TRUE),Lake_table[[i]][,7]*-1,min(Lake_table[[i]][,7]*-1,na.rm=TRUE))
  SCpr_y=c(SCpr_y,rev(SCpr_y))
  polygon(SCpr_x,SCpr_y,lty=3,col=prSrC_col,lwd=1.25) 
  
  if(names(Lake_table[i])=="White"){ 
    K_SCDpr_xmax=c(0,(Lake_table[[i]][K_Srows,13]/2)*-1,0)+3 
    K_SCDpr_xmin=c(rep(0,length(K_SCDpr_xmax)))+3
    K_SCDpr_x=c(K_SCDpr_xmax,K_SCDpr_xmin)
    K_SCDpr_y=c(max(Lake_table[[i]][K_Srows,12]*-1,na.rm=TRUE),Lake_table[[i]][K_Srows,12]*-1,min(Lake_table[[i]][K_Srows,12]*-1,na.rm=TRUE))
    K_SCDpr_y=c(K_SCDpr_y,rev(K_SCDpr_y))
    polygon(K_SCDpr_x,K_SCDpr_y,lty=3,col=prSCD_col,lwd=1.25) 
    
    P_SCDpr_xmax=c(0,(Lake_table[[i]][P_Srows,13]/2)*-1,0)+3 
    P_SCDpr_xmin=c(rep(0,length(P_SCDpr_xmax)))+3
    P_SCDpr_x=c(P_SCDpr_xmax,P_SCDpr_xmin)
    P_SCDpr_y=c(max(Lake_table[[i]][P_Srows,12]*-1,na.rm=TRUE),Lake_table[[i]][P_Srows,12]*-1,min(Lake_table[[i]][P_Srows,12]*-1,na.rm=TRUE))
    P_SCDpr_y=c(P_SCDpr_y,rev(P_SCDpr_y))
    polygon(P_SCDpr_x,P_SCDpr_y,lty=3,col=prSCD_col,lwd=1.25) 
  }
  
  SCDpr_xmax=c(0,(Lake_table[[i]][,13]/2)*-1,0)+3 
  SCDpr_xmin=c(rep(0,length(SCDpr_xmax)))+3
  SCDpr_x=c(SCDpr_xmax,SCDpr_xmin)
  SCDpr_y=c(max(Lake_table[[i]][,12]*-1,na.rm=TRUE),Lake_table[[i]][,12]*-1,min(Lake_table[[i]][,12]*-1,na.rm=TRUE))
  SCDpr_y=c(SCDpr_y,rev(SCDpr_y))
  polygon(SCDpr_x,SCDpr_y,lty=3,col=prSCD_col,lwd=1.25) 
  
  
  #polygons for paleoecological proxies
  #Plotting Charcoal
  if(char_plots[i]==TRUE){
    
    Char_poly=Lake_table[[i]][,2]
    Char_poly=Char_poly[!is.na(Char_poly)]
    
    Char_polage=Lake_table[[i]][,1]
    Char_polage=Char_polage[!is.na(Char_polage)]
    
    
  Char_xmax=c(0,(Char_poly)/ceiling(max(Char_poly,na.rm=TRUE)),0)+4.5 
  Char_xmin=c(rep(0,length(Char_xmax)))+4.5 
  Char_y=c(max(Char_polage*-1,na.rm=TRUE),Char_polage*-1,min(Char_polage*-1,na.rm=TRUE)) 
  Char_x=c(Char_xmax,Char_xmin) 
  Char_y=c(Char_y,rev(Char_y)) 
  polygon(Char_x,Char_y,col="black")
  
  if(char_bckgr[i]==TRUE){
    Bkg_lines=Lake_table[[i]][,3]
    lines((Bkg_lines/ceiling(max(Bkg_lines,na.rm=TRUE)))+4.5,Char_polage*-1,lty=3)
  }
  
  } else {}
  
  #Plotting Hardwood Pollen
  if(names(Lake_table[i])=="White"){
    P_Pollen_max=c(Lake_table[[i]][P_Prows,6]/ceiling(max(Lake_table[[i]][P_Prows,6],na.rm=TRUE)),0,0)+1.5
    P_Pollen_min=c(Lake_table[[i]][P_Prows,4]*-1,min(Lake_table[[i]][P_Prows,4]*-1,na.rm=TRUE),max(Lake_table[[i]][P_Prows,4]*-1,na.rm=TRUE))
    polygon(P_Pollen_max,P_Pollen_min,col=HWpol_col_var1,lwd=1.25)
    
    K_Pollen_max=c(Lake_table[[i]][K_Prows,6]/ceiling(max(Lake_table[[i]][K_Prows,6],na.rm=TRUE)),0,0)+1.5
    K_Pollen_min=c(Lake_table[[i]][K_Prows,4]*-1,min(Lake_table[[i]][K_Prows,4]*-1,na.rm=TRUE),max(Lake_table[[i]][K_Prows,4]*-1,na.rm=TRUE))
    polygon(K_Pollen_max,K_Pollen_min,col=HWpol_col,lwd=1.25)
    
  } else {
    Pollen_max=c((Lake_table[[i]][!is.na(Lake_table[[i]][,6]),6])/ceiling(max(Lake_table[[i]][,6],na.rm=TRUE)),0,0)+1.5
    Pollen_min=c(Lake_table[[i]][!is.na(Lake_table[[i]][,4]),4]*-1,min(Lake_table[[i]][,4]*-1,na.rm=TRUE),max(Lake_table[[i]][,4]*-1,na.rm=TRUE))
    polygon(Pollen_max,Pollen_min,col=HWpol_col,lwd=1.25)
  }
  
  #Plotting Strongly Coprophilic Fungi
  CopSp_min=c(Lake_table[[i]][!is.na(Lake_table[[i]][,7]),7]*-1,min(Lake_table[[i]][,7]*-1,na.rm=TRUE),max(Lake_table[[i]][,7]*-1,na.rm=TRUE)) #Establish this first.. 14-8-2021
  if(fungi_conc[i]==TRUE){ ###Tagging all pct plot fixes - might need to adjust this for new columns, Lake_table calls... CHECK
    if(names(Lake_table[i])=="Sheelar"){
      CopSp_max=c((Lake_table[[i]][!is.na(Lake_table[[i]][,9]),9])/(50000),0,0)
      
      #INTERVENTION TO TRIM VALUES HERE#
      if(trim_values==TRUE){
        CopSp_lim=max(CopSp_max,na.rm=TRUE)
        if(CopSp_lim>1){
          lim_values=CopSp_max[CopSp_max>1]
          lim_pos=CopSp_min[CopSp_max>1]
          CopSp_max[CopSp_max>1]=1
          text(rep(1.2,length(lim_values)),lim_pos,floor(lim_values*50000),cex=0.8)
          } else {}
      } else {}
      
      axis(1,at=c(0,1),labels=c(0,50000),cex.axis=0.75) #Fungi axis
    } else {

      CopSp_max=c((Lake_table[[i]][!is.na(Lake_table[[i]][,9]),9])/ceiling(max(Lake_table[[i]][,9],na.rm=TRUE)),0,0) 
      axis(1,at=c(0,1),labels=c(0,max(ceiling(Lake_table[[i]][,9]),na.rm=TRUE)),cex.axis=0.75) 
    }
  } else { #.
    if(names(Lake_table[i])=="Stotzel-Leis"){
      CopSp_max=c((Lake_table[[i]][!is.na(Lake_table[[i]][,11]),11])/(25),0,0)
      
      if(trim_values==TRUE){
        CopSp_lim=max(CopSp_max,na.rm=TRUE)
        if(CopSp_lim>1){
          lim_values=CopSp_max[CopSp_max>1]
          lim_pos=CopSp_min[CopSp_max>1]
          lim_pos=c(lim_pos[1]-100,lim_pos[2]+200)
          CopSp_max[CopSp_max>1]=1
          text(rep(1.1,length(lim_values)),lim_pos,floor(lim_values*25),cex=0.8)
        } else {}
      } else {}
      
      axis(1,at=c(0,1),labels=c(0,25),cex.axis=0.75) #Fungi axis
    } else {
     CopSp_max=c((Lake_table[[i]][!is.na(Lake_table[[i]][,11]),11])/ceiling(max(Lake_table[[i]][,11],na.rm=TRUE)),0,0)
     axis(1,at=c(0,1),labels=c(0,max(ceiling(Lake_table[[i]][,11]),na.rm=TRUE)),cex.axis=0.75) #Fungi axis
    }
  }
  polygon(CopSp_max,CopSp_min,col=StrC_col,lwd=1.25)
  
  #Plotting minimum Square Chord Distance data
  if(names(Lake_table[i])=="White"){ 
    P_minSCD_max=c(Lake_table[[i]][P_Srows,14]/ceiling(max(Lake_table[[i]][P_Srows,14],na.rm=TRUE)),0,0)+3
    P_minSCD_min=c(Lake_table[[i]][P_Srows,12]*-1,min(Lake_table[[i]][P_Srows,12]*-1,na.rm=TRUE),max(Lake_table[[i]][P_Srows,12]*-1,na.rm=TRUE))
    polygon(P_minSCD_max,P_minSCD_min,col=minSCD_col,lwd=1.25)
    
    K_minSCD_max=c(Lake_table[[i]][K_Srows,14]/ceiling(max(Lake_table[[i]][K_Srows,14],na.rm=TRUE)),0,0)+3
    K_minSCD_min=c(Lake_table[[i]][K_Srows,12]*-1,min(Lake_table[[i]][K_Srows,12]*-1,na.rm=TRUE),max(Lake_table[[i]][K_Srows,12]*-1,na.rm=TRUE))
    polygon(K_minSCD_max,K_minSCD_min,col=minSCD_col,lwd=1.25)
  } else {
    minSCD_max=c((Lake_table[[i]][!is.na(Lake_table[[i]][,14]),14])/ceiling(max(Lake_table[[i]][,14],na.rm=TRUE)),0,0)+3
    minSCD_min=c(Lake_table[[i]][!is.na(Lake_table[[i]][,12]),12]*-1,min(Lake_table[[i]][,12]*-1,na.rm=TRUE),max(Lake_table[[i]][,12]*-1,na.rm=TRUE))
    polygon(minSCD_max,minSCD_min,col=minSCD_col,lwd=1.25)
  }
  
  #Axes, etc.
  lines(c(0,0),c(signif(min(age_depth$Age_max),2),signif(max(age_depth$Age_min),1)))
  lines(c(0,0)+1.5,c(signif(min(age_depth$Age_max),2),signif(max(age_depth$Age_min),1)))
  lines(c(0,0)+3,c(signif(min(age_depth$Age_max),2),signif(max(age_depth$Age_min),1)))
  lines(c(0,0)+4.5,c(signif(min(age_depth$Age_max),2),signif(max(age_depth$Age_min),1)))
  
  if(char_plots[i]==TRUE){
  axis(1,at=c(4.5,5.5),labels = c(0,ceiling(plot_minmax$Char_max[i])),cex.axis=0.75)
  } else {}
  axis(1,at=c(1.5,2.5),labels=c(0,max(ceiling(Lake_table[[i]][,6]),na.rm=TRUE)),cex.axis=0.75) #Pollen axis
  axis(1,at=c(3,4),labels=c(0,max(ceiling(Lake_table[[i]][,14]),na.rm=TRUE)),cex.axis=0.75) 
  axis(3,at=c(1,1.5),labels=c("1.0","0.0"),cex.axis=0.75)
  axis(3,at=c(2.5,3),labels=c("1.0","0.0"),cex.axis=0.75)
  axis(3,at=c(-0.5,0),labels=c("1.0","0.0"),cex.axis=0.75)
  
  #Toying with labeling alternatives
  axis(1,at=5,labels=paste0(char_labels[i]),cex.axis=0.75,tick=FALSE,line=1.50) ###Labeling charcoal using custom labels
  axis(1,at=5,labels=paste0(char_sub[i]),cex.axis=0.75,tick=FALSE,line=2.5)
  axis(1,at=2,labels="Percent Hardwood Pollen",cex.axis=0.75,tick=FALSE,line=1.50)
  axis(1,at=2,labels=paste0(poll_sub[i]),cex.axis=0.75,tick=FALSE,line=2.5)

  if(fungi_conc[i]==FALSE) {
    axis(1,at=0.5,labels=paste0(fungi_labels[i]," Fungi (%)"),cex.axis=0.75,tick=FALSE,line=1.50) # Custom labeling of fungal data
    axis(1,at=0.5,labels=paste0(fung_sub[i]),cex.axis=0.75,tick=FALSE,line=2.5)
  } else {
    axis(1,at=0.5,labels=paste0(fungi_labels[i]," Fungi (conc.)"),cex.axis=0.75,tick=FALSE,line=1.50) #Custom labeling of fungal data
    axis(1,at=0.5,labels=paste0(fung_sub[i]),cex.axis=0.75,tick=FALSE,line=2.5)
  }
  
  axis(1,at=3.5,labels="Minimum Square Chord Distance",cex.axis=0.75,tick=FALSE,line=1.50)
  axis(1,at=3.5,labels=paste0(mscd_sub[i]),cex.axis=0.75,tick=FALSE,line=2.5)
  
  axis(3,at=1.25,labels="BCP Posterior Probability",cex.axis=0.75,tick=FALSE,line=1.75)
  axis(3,at=2.75,labels="BCP Posterior Probability",cex.axis=0.75,tick=FALSE,line=1.75)
  axis(3,at=-0.25,labels="BCP Posterior Probability",cex.axis=0.75,tick=FALSE,line=1.75)
  if(save_pdf==TRUE){
    dev.off()
  }

}

##########PLOTTING ALL FUNGAL RECORDS TOGETHER####################

lake_names=c('Binnewater','Otisville','Appleman','Bonnet','Silver','Stotzel-Leis','Cupola','White','Page-Ladson','Sheelar') #5-2021 Updated Names
lake_abbrv=c('Binn','Otis','Appl','Bont','Silv','Stot','Cup','Whit','Plad','Shee') #12-2020 Updated name order

proxy=c('StronglyCopro','PollPct','charAcc','minSCD')
fungal_values=c('Conc','Pct')

fungi_conc=c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE) #Updating listings with new taxa
char_plots=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE) 
fungi_logtrans=FALSE #Logical to control labeling of plots, if using log transformed data, make sure this is set to "TRUE". 16.10.2020 CAK

plot_order=c('Strg.Copr.Fungi','HW Pollen%','minSCD','CharAcc')

split_allplot=TRUE #logical control over splitting all lakes plot into panels
panel_vert=FALSE #logical control over organization of panels

save_pdf=TRUE #Logical control for saving pdfs of plots. 16.10.2020 CAK
uniform_scaling=TRUE #Logical control for uniform scaling of individual lake plots. 16.10.2020 CAK
scale_minBP=6000  #Values controlling limits of plots. 16.10.2020 CAK
scale_maxBP=20000 #Values controlling limits of plots. 16.10.2020 CAK

record_box=FALSE #logical controlling drawing boxes to delimit records
record_lines=FALSE #logical controlling drawing lines to delimit records
inverse_box=TRUE #logical controlling inverse boxes
trim_values=FALSE


for(i in 1:length(lake_names)){
  if(fungi_conc[i]==FALSE){
    Lake_table[[i]][,c(8,9)]=0
  } else {
    if(lake_abbrv[i]=="Whit"){
      Lake_table[[i]][,c(10,11)]=0
    } else {}
  }
}

if(split_allplot==TRUE){ #If we're splitting plots
  lake_pct=lake_names
  lake_con=lake_names
  allplot_list=list(lake_pct,lake_con) #Make a list with the two vectors in it
}

#Settings for pdf making and setting par values
if(save_pdf==TRUE){
  setEPS()
  pdf("All_lakes_plot.pdf",width=10,height=12)
  par(mar=c(6,6,6,1))
} else {}

#Settings for splitting plots and organizing panels.
if(split_allplot==TRUE){
  if(panel_vert==TRUE){
    par(mfrow=c(1,2))
    print("horiz split")
  } else {
    par(mfrow=c(2,1))
    print("vert split")
  }
  
  for(j in 1:length(allplot_list)){ #For every group of lake names
    plot(100,100,ylim=c(scale_maxBP,scale_minBP-1000),xlim=c(0,length(allplot_list[[j]])*1.5+1),axes=FALSE,ann=FALSE) #Y values are manually set.
    axis(2,las=1,at=1000*20:7,labels=rev(c(seq(7,20,1))))
    if(j==2){
      title(main='Strongly Coprophilous Spores (conc.) and BCP Posterior Probability by Record',line=4.5,cex=2)
    } else {
      title(main='Strongly Coprophilous Spores (%) and BCP Posterior Probability by Record',line=4.5,cex=2)
    }

    title(ylab='KA',line=2.5)
    spacing=c(seq(0.5,length(allplot_list[[j]]))*1.5,1.5)
    
    for(i in 1:length(allplot_list[[j]])){ #For every lake in the grouped names...

      if(j==2){
        SCpr_max=c((Lake_table[[paste0(allplot_list[[j]][i])]][,8]/2),0,0)*-1+spacing[i] #Defining max posterior prob values.
      } else {
        SCpr_max=c((Lake_table[[paste0(allplot_list[[j]][i])]][,10]/2),0,0)*-1+spacing[i] #Defining max posterior prob values.
      }
      SCpr_min=c(Lake_table[[paste0(allplot_list[[j]][i])]][,7],min(Lake_table[[paste0(allplot_list[[j]][i])]][,7],na.rm=TRUE),max(Lake_table[[paste0(allplot_list[[j]][i])]][,7],na.rm=TRUE))+spacing[i]
      
      if(j==1 & lake_abbrv[i]=="Whit"){ #If we're plotting percents and encounter Whit
        #Don't make any plots 
      } else { #Otherwise..
        if(j==2 & fungi_conc[i]==FALSE){#If we encounter records without concentrations in the concentration run
          #Don't make any plots
        } else { #If it's a percent that's not Whit or a legit concentration value
          polygon(SCpr_max,SCpr_min,lty=3,col=prSrC_col,lwd=1.25) #Make a polygon
        }
      }
      
      
      if(record_lines==TRUE){
        Fungi_base=max(Lake_table[[paste0(allplot_list[[j]][i])]][,7],na.rm=TRUE)*-1
        Fungi_top=min(Lake_table[[paste0(allplot_list[[j]][i])]][,7],na.rm=TRUE)*-1
        lines(c(spacing[i]-breaks,spacing[i]+1),c(Fungi_base,Fungi_base)*-1,lty=3)
        lines(c(spacing[i]-breaks,spacing[i]+1),c(Fungi_top,Fungi_top)*-1,lty=3)
      }
      
      if(inverse_box==TRUE){
        Fungi_base=max(Lake_table[[paste0(allplot_list[[j]][i])]][,7],na.rm=TRUE)*-1
        Fungi_top=min(Lake_table[[paste0(allplot_list[[j]][i])]][,7],na.rm=TRUE)*-1
        #Need a top and bottom box to bound the results.
        topbox_x=c(spacing[i]-breaks,spacing[i]-breaks,spacing[i]+1,spacing[i]+1)
        topbox_y=c(Fungi_top,(scale_minBP-1000)*-1,(scale_minBP-1000)*-1,Fungi_top)*-1
        polygon(topbox_x,topbox_y,col=record_col,lty=0)
        
        if(Fungi_base<=scale_maxBP*-1){
          
        } else {
          botbox_x=c(spacing[i]-breaks,spacing[i]-breaks,spacing[i]+1,spacing[i]+1)
          botbox_y=c(Fungi_base,scale_maxBP*-1,scale_maxBP*-1,Fungi_base)*-1
          polygon(botbox_x,botbox_y,col=record_col,lty=0)
        }
        
      }
      
      #Strongly Coprophilic Fungi value plots
      CopSp_min=c(Lake_table[[paste0(allplot_list[[j]][i])]][!is.na(Lake_table[[paste0(allplot_list[[j]][i])]][,7]),7]*-1,min(Lake_table[[paste0(allplot_list[[j]][i])]][,7]*-1,na.rm=TRUE),max(Lake_table[[paste0(allplot_list[[j]][i])]][,7]*-1,na.rm=TRUE))*-1+spacing[i]
      if(j==2){
        if(allplot_list[[j]][i]=="Sheelar"){
          CopSp_max=c((Lake_table[[paste0(allplot_list[[j]][i])]][!is.na(Lake_table[[paste0(allplot_list[[j]][i])]][,9]),9])/50000,0,0)+spacing[i]
          #INTERVENTION TO TRIM VALUES HERE#
          if(trim_values==TRUE){
            CopSp_lim=max(CopSp_max,na.rm=TRUE)
            if(CopSp_lim>(1+spacing[i])){
              lim_values=CopSp_max[CopSp_max>(1+spacing[i])]
              lim_pos=CopSp_min[CopSp_max>(1+spacing[i])]
              CopSp_max[CopSp_max>(1+spacing[i])]=(1+spacing[i])
              text(rep((1.2+spacing[i]),length(lim_values)),lim_pos,floor(lim_values*50000),cex=0.8)
            } else {}
          } else {}
          axis(1,spacing[i]:(spacing[i]+1),labels=c(0,50000),cex.axis=0.75)
        } else {
          CopSp_max=c((Lake_table[[paste0(allplot_list[[j]][i])]][!is.na(Lake_table[[paste0(allplot_list[[j]][i])]][,9]),9])/ceiling(max(Lake_table[[paste0(allplot_list[[j]][i])]][,9],na.rm=TRUE)),0,0)+spacing[i] #Same as above, just adding spacing[i] values
          axis(1,spacing[i]:(spacing[i]+1),labels=c(0,ceiling(max(Lake_table[[allplot_list[[j]][i]]][,9],na.rm=TRUE))),cex.axis=0.75)  ###Tagging fixes to axis labeling - not reading this list properly
        }
      } else {
        if(allplot_list[[j]][i]=="Stotzel-Leis"){
          CopSp_max=c((Lake_table[[paste0(allplot_list[[j]][i])]][!is.na(Lake_table[[paste0(allplot_list[[j]][i])]][,11]),11])/25,0,0)+spacing[i]
          axis(1,spacing[i]:(spacing[i]+1),labels=c(0,25),cex.axis=0.75) 
          if(trim_values==TRUE){
            CopSp_lim=max(CopSp_max,na.rm=TRUE)
            if(CopSp_lim>(1+spacing[i])){
              lim_values=CopSp_max[CopSp_max>(1+spacing[i])]
              lim_pos=CopSp_min[CopSp_max>(1+spacing[i])]
              lim_pos=c(lim_pos[1]-100,lim_pos[2]+300)
              CopSp_max[CopSp_max>(1+spacing[i])]=(1+spacing[i])
              text(rep((1.1+spacing[i]),length(lim_values)),lim_pos,floor(lim_values*25),cex=0.8)
            } else {}
          } else {}
        } else {
          CopSp_max=c((Lake_table[[paste0(allplot_list[[j]][i])]][!is.na(Lake_table[[paste0(allplot_list[[j]][i])]][,11]),11])/ceiling(max(Lake_table[[paste0(allplot_list[[j]][i])]][,11],na.rm=TRUE)),0,0)+spacing[i] #Same as above, just adding spacing[i] values
          axis(1,spacing[i]:(spacing[i]+1),labels=c(0,ceiling(max(Lake_table[[allplot_list[[j]][i]]][,11],na.rm=TRUE))),cex.axis=0.75) ###Tagging fixes to axis labeling - not reading this list properly
        }
        
      }
      
      if(j==1 & lake_abbrv[i]=="Whit"){ #If we're plotting percents and encounter Whit
        #Don't make any plots 
      } else { #Otherwise..
        if(j==2 & fungi_conc[i]==FALSE){#If we encounter records without concnetrations in the concentration run
          #Don't make any plots
        } else { #If it's a percent that's not Whit or a legit concentration value
          polygon(CopSp_max,CopSp_min,col=StrC_col,lwd=1.25) #Make a polygon
        }
      }

      
      axis(3,at=c(spacing[i]-0.5,spacing[i]),labels=c("1.0","0.0"),cex.axis=0.75)

      if(j==1){
        axis(1,at=spacing[i]+0.5,labels="%",cex.axis=0.75,tick=FALSE,line=1)
      } else {
        axis(1,at=spacing[i]+0.5,labels="Conc.",cex.axis=0.75,tick=FALSE,line=1)
      }
      #}
      axis(3,at=c(spacing[i]-0.25),labels="Post. Prob.",tick=FALSE,cex.axis=0.75,line=1.25)
      
      lines(c(0,0)+spacing[i],c(signif(min(age_depth$Age_max),2)*-1,signif(max(age_depth$Age_min),1)))
      
    }
    
    text(spacing[1:length(spacing)-1]-0.25,5000,srt=90,labels=allplot_list[[j]],xpd=T,adj=1)
    

  } #End of group loop
  par(mfrow=c(1,1))
  if(save_pdf==TRUE){
    dev.off()
  }
} else {}


