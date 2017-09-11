function(input, output) {
  library(ncdf4)
  # source('/home/taha/Projets/ICCAT_VPA/Rscripts/OutputsVPA2NetCDF.R')
  NetCDFtoRdata <- function(nc_name,nc_path="/home/taha/R1/monR-2013/esp_travail"){
    
    # nc_name = "data_retro_Run_1_911_v4.nc"
    library(reshape)
    ##obj
    # obj <- ncatt_get(nc,0)
    nc <- nc_open(paste(nc_path,nc_name,sep=""))
    obj <- ncvar_get(nc,"obj")
    cname <- nc$var[["obj"]]$dim[[1+2]]$unit
    indelet <-  as.vector(gregexpr(pattern =':',nc$var[["obj"]]$dim[[1+2]]$unit)[[1]])
    for(r in indelet){
      if(r==2){cname <- substr(cname, 3, nchar(cname)) 
      }else{
        cname <- paste(substr(cname, 1, r-3-2), substr(cname, r+1-2, nchar(cname)), sep='///')}
    }
    cname <- strsplit(cname,"///")[[1]]
    
    names(obj) <- cname
    obj <- as.list(obj)
    ##residuals
    RES <-  list()
    rr=1
    for( resi in c("residuals","residuals_obseved","residuals_predicted")){
      residuals <- ncvar_get(nc,resi)
      
      rownames(residuals) <- nc$var[[resi]]$dim[[1+2]]$vals
      cname <- nc$var[[resi]]$dim[[2+2]]$unit
      indelet <-  as.vector(gregexpr(pattern =':',nc$var[[resi]]$dim[[2+2]]$unit)[[1]])
      for(r in indelet){
        if(r==2){cname <- substr(cname, 3, nchar(cname)) 
        }else{
          cname <- paste(substr(cname, 1, r-3-2), substr(cname, r+1-2, nchar(cname)), sep='///')}
      }
      cname <- strsplit(cname,"///")[[1]]
      colnames(residuals) <- cname
      residuals <- melt(residuals)
      resi <- strsplit(resi,"_")[[1]][length(strsplit(resi,"_")[[1]])]
      colnames(residuals) <- c("Year","CPUE",resi)
      RES[[rr]] <- residuals
      rr <- rr+1
    }
    residuals <- na.omit(cbind(RES[[1]],RES[[2]],RES[[3]]))
    residuals <- residuals[, unique(colnames(residuals))]
    # residuals <- na.omit(Reduce(merge,RES))
    # residuals <- residuals[with(residuals, order(CPUE)), ]
    
    ##main
    main <- ncvar_get(nc,"main")
    cname <- nc$var[["main"]]$dim[[2+2]]$unit
    indelet <-  as.vector(gregexpr(pattern =':',nc$var[["main"]]$dim[[2+2]]$unit)[[1]])
    for(r in indelet){
      if(r==2){cname <- substr(cname, 3, nchar(cname)) 
      }else{
        cname <- paste(substr(cname, 1, r-3-2), substr(cname, r+1-2, nchar(cname)), sep='///')}
    }
    cname <- strsplit(cname,"///")[[1]]
    for(i in 1:dim(main)[3]){
      MAIN <- main[,,i]
      colnames(MAIN) <- cname
      rownames(MAIN)<- nc$var[["main"]]$dim[[1+2]]$vals
      MAIN <- MAIN[rowSums(MAIN,na.rm = T)!=0, ]
      MAIN <- melt(MAIN);colnames(MAIN)[1:2]<- c("Year","variable")
      MAIN$Retros <- nc$var[["main"]]$dim[[3+2]]$vals[i]
      MAIN <- MAIN[,c("Retros","Year","variable","value")]
      if(i==1){main1 <- MAIN}
      if(i>1){main1 <- rbind(main1,MAIN)}
    }
    main <- main1
    
    
    ##faa
    faa <- ncvar_get(nc,"faa")
    faa <- t(faa)
    cname <- nc$var[["faa"]]$dim[[2+2]]$unit
    indelet <-  as.vector(gregexpr(pattern =':',nc$var[["faa"]]$dim[[2+2]]$unit)[[1]])
    for(r in indelet){
      if(r==2){cname <- substr(cname, 3, nchar(cname)) 
      }else{
        cname <- paste(substr(cname, 1, r-3-2), substr(cname, r+1-2, nchar(cname)), sep='///')}
    }
    cname <- strsplit(cname,"///")[[1]]
    colnames(faa) <- nc$var[["faa"]]$dim[[1+2]]$vals
    rownames(faa) <- cname
    
    ###creat output VPA object from netcdf
    
    outputs_VPA_nc <- list(obj=obj,residuals=residuals,main=main,faa=faa)
    return(outputs_VPA_nc)
  }
  linksfile <- read.csv('https://goo.gl/lcuqZO')
  
#   index <- reactive({which(linksfile$runNumb_retros==unlist(strsplit(test,' SEED:'))[1] && linksfile$seedNumb_retros==unlist(strsplit(test,' SEED:'))[2] )
#   })
  retrosdata <- reactive({
    
    ind <- which(linksfile$runNumb_retros==unlist(strsplit(input$Run,' SEED:'))[1] & linksfile$seedNumb_retros==unlist(strsplit(input$Run,' SEED:'))[2]) 
    OpenDap_URL <- as.character(linksfile$linktofile2[ind])
   
    
#   ind <- index()
#   OpenDap_URL <- as.character(linksfile$linktofile2[ind])
  zip_file=paste(getwd(),"/ncResult.zip",sep="")
  download.file(OpenDap_URL, destfile = zip_file, method='auto', quiet = FALSE, mode = "w",cacheOK = TRUE,extra = getOption("download.file.extra"))
  unzip(zip_file, overwrite = T, exdir = getwd())
  # RunSeed <- reactive({unlist(strsplit(input$Run,' SEED:'))})
    NetCDFtoRdata(nc_name=paste(getwd(),'/ICCAT_BFTE/2014/RETROS/',unlist(strsplit(input$Run,' SEED:'))[1],'/data_retro_',unlist(strsplit(input$Run,' SEED:'))[1],'_',abs(as.numeric(unlist(strsplit(input$Run,' SEED:'))[2])),'.nc',sep=''),nc_path = "")
   
     })
  
  
  ## for ncdump
  ncdump <- reactive({
#     ind <- which(linksfile$runNumb_retros==unlist(strsplit(input$Run,' SEED:'))[1] & linksfile$seedNumb_retros==unlist(strsplit(input$Run,' SEED:'))[2]) 
#     OpenDap_URL <- as.character(linksfile$linktofile2[ind])
#     
#     
#     #   ind <- index()
#     #   OpenDap_URL <- as.character(linksfile$linktofile2[ind])
#     zip_file=paste(getwd(),"/ncResult.zip",sep="")
#     download.file(OpenDap_URL, destfile = zip_file, method='auto', quiet = FALSE, mode = "w",cacheOK = TRUE,extra = getOption("download.file.extra"))
#     unzip(zip_file, overwrite = T, exdir = getwd())
    ncdump <- (nc_name=paste(getwd(),'/ICCAT_BFTE/2014/RETROS/',unlist(strsplit(input$Run,' SEED:'))[1],'/data_retro_',unlist(strsplit(input$Run,' SEED:'))[1],'_',abs(as.numeric(unlist(strsplit(input$Run,' SEED:'))[2])),'.nc',sep=''))
  })
#   main <- reactive({
#     retdata <- retrosdata()
#     retdata$main })
  
  output$ncdump <- renderPrint({
    ncop <- nc_open(ncdump())
    ncop
  })
 
  
  # ncfile <- paste(getwd(),'/ICCAT_BFTE/2014/RETROS/',RunSeed[1],'/data_retro_',RunSeed[1],'_',abs(as.numeric(RunSeed[2])),'.nc',sep='')
  
  
#   retrosdata <- NetCDFtoRdata(nc_name = ncfile(),nc_path = "")
  
#   obj <- as.matrix(retrosdata$obj)
#   colnames(obj) <- 'value'
#   
#   residuals <- retrosdata$residuals
#   
#   
#   main <- retrosdata$main
#   
#   
#   faa <- retrosdata$faa
    
   
 
  output$tab1 <- DT::renderDataTable({
    # DT::datatable(main, options = list(pageLength = 25))
    datata <- retrosdata()
    main <- datata$main
    
    DT::datatable(
      main, options = list(
        lengthMenu = list(c(10, 100, -1), c('10', '100', 'All')),
        pageLength = 10
      )
    )
  })
  
  # -1 means no pagination; the 2nd element contains menu labels
  output$tab2 <- DT::renderDataTable({
    datata <- retrosdata()
    residuals <- datata$residuals
    DT::datatable(
      residuals, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
      )
    )
  })
  
  # you can also use paging = FALSE to disable pagination
  
  # turn off filtering (no searching boxes)
  output$tab3 <- DT::renderDataTable({
    datata <- retrosdata()
    faa <- datata$faa
    DT::datatable(t(faa), options = list(searching = FALSE))
  })
  
  # write literal JS code in JS()
  output$tab4 <- DT::renderDataTable({
    datata <- retrosdata()
    obj <- datata$obj
    DT::datatable(
    as.data.frame(obj),
    options = list(paging = FALSE)
    # options = list(rowCallback = DT::JS(
#       'function(row, data) {
#         // Bold cells for those >= 5 in the first column
#         if (parseFloat(data[1]) >= 5.0)
#           $("td:eq(1)", row).css("font-weight", "bold");
#       }'
    # ))
  )})
  

#   output$downloadData <- downloadHandler(
#     filename = function() { 
#       paste(input$dataset, '.csv', sep='') 
#     },
#     content = function(file) {
#       write.csv(datasetInput(), file)
#     })

}