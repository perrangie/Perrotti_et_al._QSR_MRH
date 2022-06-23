#This script runs the BCP analysis and saves files in the Stacked Plots folder.
library(bcp)

########FUNGI BCP###################

lake_names <- c('Bonnet','Cupola', 'Sheelar', 'Silver', 'Stotzel-Leis', 'Page Ladson','Appleman','White','Binnewater','Otisville')
lake_abbrv <- c('Bont', 'Cup', 'Shee', 'Silv', 'Stot', 'Plad','Appl','Whit','Binn','Otis')
conc_value = c(FALSE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE) #Logical controlling using pct or conc.
suffix <- c(rep('Fungi_AllTotals_Final',8),rep('fungi_compare_final',2))
proxy_type = c(rep('StronglyCopro',8),rep('Sporormiella',2))

log_transform=FALSE 
plot_log=FALSE 
plot_hist=FALSE 

##establish variables for burnin and mcmc parameters (number of iterations)
n_burnin =1000
n_mcmc =10000

for(i in 1:length(lake_names)){
  spore_data <- read.csv(paste0('Data/',lake_names[i],'/',lake_abbrv[i],suffix[i],'.csv'))
  
  cn <- colnames(spore_data)
  pct = grep(paste0(proxy_type[i],'Pct'),cn,ignore.case=TRUE)
  conc = grep(paste0(proxy_type[i],'Conc'),cn,ignore.case=TRUE)
  
  if(conc_value[i]==TRUE){ 
    col_use=c(conc,pct)
  }else{col_use=c(pct)} 
  
  age_index <- grep('Age', cn, ignore.case=TRUE) 
  age <- spore_data[,age_index]
  
  for(ii in col_use) {
    data_analyze <- spore_data[,ii]
    if(plot_hist==TRUE){
      hist(data_analyze,main=paste0(lake_names[i],' Histogram of Untransformed Data'))
      log_data=log(data_analyze)
      hist(log_data,main=paste0(lake_names[i],' Histogram of Transformed Data'))
    }
    if(log_transform==TRUE){ 
      data_analyze=log1p(data_analyze)
    }
    BCP <- bcp(data_analyze, burnin = n_burnin, p0=.07, w0=.2, mcmc = n_mcmc, return.mcmc = TRUE)
    plot(BCP,main= paste(lake_names[i], cn[ii]))
    bcp_table <- table(BCP$blocks[10001:20000])
    mean_of_block <- BCP$posterior.mean 
    posterior_prob <- BCP$posterior.prob 
    
    plot(age, posterior_prob, type='l', ylim=c(0,1.2), main= paste(lake_names[i], cn[ii]), ylab="Posterior Mean", xlab= "Age")
    if(plot_log==TRUE){ 
      points(age,(log1p(data_analyze))/(max(log(data_analyze))), col=2, pch=16)
    } else {
      points(age,data_analyze/(max(data_analyze)), col=2, pch=16)
    }
    
    lineplot_export<-matrix(c(age,posterior_prob,data_analyze),nrow=length(age),ncol=3,
                            dimnames=list(c(1:length(age)),
                                          c("age","posterior_prob",cn[ii])))
    
    write.table(lineplot_export,file=paste0('Data/Stacked_Plots/',lake_abbrv[i],'_',cn[ii],'.csv'),sep=",",row.names=FALSE)
  }
}

########CHARCOAL BCP################

lake_names <- c('Bonnet','Cupola', 'Sheelar', 'Silver', 'Stotzel-Leis', 'Page Ladson','Appleman','White','Binnewater','Otisville')
lake_abbrv <- c('Bont', 'Cup', 'Shee', 'Silv', 'Stot', 'Plad','Appl','Whit','Binn','Otis')
char_plot = c(TRUE,TRUE,FALSE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE) #Logical to control plotting of charcoal data 12-5-2020 
char_bkg = c(TRUE,TRUE,FALSE,TRUE,TRUE,FALSE,TRUE,TRUE,FALSE,FALSE) #Logical controlling pulling charcoal background data 1-5-2022 CAK
char_micr= c(rep('charAcc',length(char_plot)-2),rep('char_mm2cm3',2))
suffix <- '_Char_Final' 

log_transform=FALSE 
plot_log=FALSE 

##establish variables for burnin and mcmc parameters (number of iterations)

n_burnin =1000
n_mcmc =10000

for(i in 1:length(lake_names)){
  if(char_plot[i]==TRUE){ #Use logical plotting control 12-5-2020 CAK
    char_data <- read.csv(paste0('Data/',lake_names[i],'/',lake_abbrv[i],suffix,'.csv'))
    cn <- colnames(char_data)
    if(char_micr[i]=='char_mm2cm3'){
      accum <- grep('char_mm2cm3', cn, ignore.case=TRUE)
    } else {
      accum <- grep('charAcc', cn, ignore.case=TRUE)
    }
    
    if(char_bkg[i]==TRUE){ #Adding options to collect background accumulation rates for plotting later. CAK 1-5-2022
      bckgr = grep('charBkg',cn,ignore.case=TRUE)
      bckgr_data = char_data[,bckgr]
    }
    
    col_use <- c(accum) 
    
    age_index <- grep('Age', cn, ignore.case=TRUE) 
    age <- char_data[,age_index]
    
    for(ii in col_use) {
      data_analyze <- char_data[,ii]
      if(log_transform==TRUE){
        data_analyze=log1p(data_analyze)
      }
      BCP <- bcp(data_analyze, burnin = n_burnin, p0=.01, w0=.9, mcmc = n_mcmc, return.mcmc = TRUE)
      plot(BCP, main= paste(lake_names[i], cn[ii]))
      
      mean_of_block <- BCP$posterior.mean
      posterior_prob_average <- BCP$posterior.prob
      
      plot(age, posterior_prob_average, type='l', ylim=c(0,1.2), main= paste(lake_names[i], cn[ii]), ylab="Posterior Mean", xlab= "Age")
      if(plot_log==TRUE){
        points(age,(log1p(data_analyze))/(max(log(data_analyze))), col=2, pch=16)
      } else {
        points(age,data_analyze/(max(data_analyze)), col=2, pch=16)
      }
      
      if(char_bkg[i]==TRUE){
        lineplot_export<-matrix(c(age,posterior_prob_average,data_analyze,bckgr_data),nrow=length(age),ncol=4,
                                dimnames=list(c(1:length(age)),
                                              c("age","posterior_prob",cn[ii],"char_bkgr")
                                ))
      } else {
        lineplot_export<-matrix(c(age,posterior_prob_average,data_analyze),nrow=length(age),ncol=3,
                                dimnames=list(c(1:length(age)),
                                              c("age","posterior_prob",cn[ii])
                                ))
      }
      
      write.table(lineplot_export,file=paste0('Data/Stacked_Plots/',lake_abbrv[i],'_',char_micr[i],'.csv'),sep=",",row.names=FALSE)
      
    }
  }
}

########minSCD BCP################

lake_names <- c('Bonnet','Cupola', 'Sheelar', 'Silver', 'Stotzel-Leis', 'Page Ladson','Appleman','White','White','Binnewater','Otisville')
lake_abbrv <- c('Bont', 'Cup', 'Shee', 'Silv', 'Stot', 'Plad','Appl','WhitKrause','WhitPerrotti','Binn','Otis')
suffix <- 'minscd_Final'

##establish variables for burnin and mcmc parameters (number of iterations)
n_burnin =1000
n_mcmc =10000
 
for(i in 1:length(lake_names)){
  scd_data <- read.csv(paste0('Data/minSCD/',lake_abbrv[i],'_',suffix,'.csv'))
  cn <- colnames(scd_data)
  scd<- grep('minSCD',cn,ignore.case=TRUE) 
  
  age_index <- grep('age', cn, ignore.case=TRUE) 
  age <- scd_data[,age_index]
  
  data_analyze <- scd_data[,scd]
  BCP <- bcp(data_analyze, burnin = n_burnin, p0=.07, w0=.2, mcmc = n_mcmc, return.mcmc = TRUE)
  plot(BCP,main= paste(lake_names[i],suffix))
  posterior_prob<- as.matrix(BCP$posterior.prob)
  y.lim <- c(0, max(data_analyze))

    plot(age, posterior_prob, type='l', ylim=c(0,1), main= paste(lake_names[i], suffix), ylab="Posterior Prob", xlab= "Age")
  points(age,data_analyze, col=2, pch=16)
  
  lineplot_export<-matrix(c(age,posterior_prob,data_analyze),nrow=length(age),ncol=3,
                          dimnames=list(c(1:length(age)),
                                        c("age","posterior_prob","minSCD")))

  write.table(lineplot_export,file=paste0('Data/Stacked_Plots/',lake_abbrv[i],'_','minSCD','.csv'),sep=",",row.names=FALSE)
} 

##############POLLEN################

##establish variables for burnin and mcmc parameters (number of iterations)
n_burnin =1000
n_mcmc =10000

lake_names <- c('Bonnet','Cupola', 'Sheelar', 'Silver', 'Stotzel-Leis', 'Page Ladson','Appleman','White','White','Binnewater','Otisville')
lake_abbrv <- c('Bont', 'Cup', 'Shee', 'Silv', 'Stot', 'Plad','Appl','WhitKrause','WhitPerrotti','Binn','Otis')
suffix <- 'PollPct_Final'

for(i in 1:length(lake_names)){
  pol_data <- read.csv(paste0('Data/',lake_names[i],'/',lake_abbrv[i],'_',suffix,'.csv'))
  cn <- colnames(pol_data)
  
  exceptions<-c(grep('Total',cn,ignore.case=TRUE),c(grep('.tot',cn))) 
  
  if(length(exceptions>0)){
    pol_data<-pol_data[,-exceptions]
  }
  cn <- colnames(pol_data)
  
  #searching columns for relevant taxa
  hwd_taxa<-c(grep('Acer',cn,ignore.case=TRUE),grep('Alnus',cn,ignore.case=TRUE),grep('Betula',cn,ignore.case=TRUE),grep('Carya',cn,ignore.case=TRUE),grep('Castanea',cn,ignore.case=TRUE),grep('Celtis',cn,ignore.case=TRUE),grep('Cornaceae',cn,ignore.case=TRUE),grep('Cornus',cn,ignore.case=TRUE),grep('Corylus',cn,ignore.case=TRUE),grep('Fagaceae',cn,ignore.case=TRUE),
              grep('Fagus',cn,ignore.case=TRUE),grep('Fraxinus',cn,ignore.case=TRUE),grep('Juglans',cn,ignore.case=TRUE),grep('Juniperus',cn,ignore.case=TRUE),grep('Liquidambar',cn,ignore.case=TRUE),grep('Morus',cn,ignore.case=TRUE),grep('Nyssa',cn,ignore.case=TRUE),grep('Ostrya/Carpinus',cn,ignore.case=TRUE),grep('Platanus',cn,ignore.case=TRUE),
              grep('Populus',cn,ignore.case=TRUE),grep('Quercus',cn,ignore.case=TRUE),grep('Salix',cn,ignore.case=TRUE),grep('Tilia',cn,ignore.case=TRUE),grep('Ulmus',cn,ignore.case=TRUE))
  
  hwd_pol<-pol_data[,hwd_taxa]
  hwd_pct_sum<-apply(hwd_pol,1,sum)
  
  age_index <- grep('Age', cn, ignore.case=TRUE) 
  age <- pol_data[,age_index]
  
  data_analyze <- hwd_pct_sum
  BCP <- bcp(data_analyze, burnin = n_burnin, p0=.07, w0=.2, mcmc = n_mcmc, return.mcmc = TRUE)
  plot(BCP,main= paste(lake_names[i],suffix))
  posterior_prob<- as.matrix(BCP$posterior.prob)

    plot(age, posterior_prob*100, type='l', ylim=c(0,100), main= paste(lake_names[i], suffix), ylab="Posterior Prob", xlab= "Age")
  points(age,data_analyze, col=2, pch=16)
  
  lineplot_export<-matrix(c(age,posterior_prob,data_analyze),nrow=length(age),ncol=3,
                          dimnames=list(c(1:length(age)),
                                        c("age","posterior_prob","pct_hardwood")))
  
  write.table(lineplot_export,file=paste0('Data/Stacked_Plots/',lake_abbrv[i],'_','PollPct','.csv'),sep=",",row.names=FALSE)
}  

