rm(list=ls())
options(java.parameters = "-Xmx16384m")
library("shiny")
library("shinyjs")
          
ui<-fluidPage(
  shinyjs::useShinyjs(),
  singleton(tags$head(HTML(
    '<title>Data-Independent Separation Application</title>

   	<!-- Web Fonts -->
	  <link rel="stylesheet" type="text/css" href="assets/css/font.css">
    
    <!-- CSS Global Compulsory -->
    <link rel="stylesheet" href="assets/plugins/bootstrap/css/bootstrap.min.css">
    <link rel="stylesheet" href="assets/css/style.css">

    <!-- CSS Header and Footer -->
    <link rel="stylesheet" href="assets/css/headers/header-default.css">
    <link rel="stylesheet" href="assets/css/footers/footer-v1.css">
    
    <!-- CSS Implementing Plugins -->
    <link rel="stylesheet" href="assets/plugins/animate.css">
    <link rel="stylesheet" href="assets/plugins/line-icons/line-icons.css">
    <link rel="stylesheet" href="assets/plugins/font-awesome/css/font-awesome.min.css">
    <link rel="stylesheet" href="assets/plugins/sky-forms-pro/skyforms/css/sky-forms.css">
    <link rel="stylesheet" href="assets/plugins/sky-forms-pro/skyforms/custom/custom-sky-forms.css">
    <!--[if lt IE 9]><link rel="stylesheet" href="assets/plugins/sky-forms-pro/skyforms/css/sky-forms-ie8.css"><![endif]-->
    
    <!-- CSS Theme -->
    <link rel="stylesheet" href="assets/css/theme-colors/default.css">
    <!--<link rel="stylesheet" href="assets/css/theme-colors/blue.css" id="style_color">-->
    <link rel="stylesheet" href="assets/css/theme-colors/green.css" id="style_color">
    	
    <!-- CSS DataTables -->
    <link rel="stylesheet" href="assets/css/jquery.dataTables.css">
    <link rel="stylesheet" href="assets/css/buttons.dataTables.min.css">
    
    <!-- CSS Customization -->
    <link rel="stylesheet" href="assets/css/custom.css">

    <div class="wrapper">
    <!--=== Header ===-->
    <div class="header">
    <div class="container">
    
    <!-- Logo -->
    <a class="logo">
    <img src="assets/img/logo-norman.png" alt="Logo" style="height: 40px">
    </a>
    <!-- End Logo -->
    <!-- Topbar -->
    
    </div><!--/end wrapper-->
    </div><!--/header-->
    </div><!--/container-->
    <!--=== End Header ===-->

	  <!--=== Breadcrumbs ===-->
  	<!--<div class="breadcrumbs">
		<div class="container">
			<h1 class="pull-left">Digital Sample Freezing Platform</h1>
	    	</div>
      	</div>-->
  	<!--=== End Breadcrumbs ===-->
    '
  ))),
  
  tags$style(type = 'text/css', '
             .navbar { 
              background-color: #fafafa;
             }
              
             .shiny-notification {
              color: black;
              font-size: 18px;
              background-color: #fafafa;
              text-align: center;
             }
            
             .progress-bar{background-color:#6FBE27;}
             
             #controls {background-color: rgba(115,181,229,0.3);;}
             #Resizefactor {background-color: rgba(0,0,255,0.00001);}

             '      
  ),
  
  navbarPage(title="", id="navbartabs", selected="Main Page",
             
             navbarMenu("Split Data-Independent mzML files",
                        tabPanel("Split Data-Independent data",
                                 fluidPage(
                                   fileInput('Sampletobeprocessed', 'Choose your full scan mzML file',accept=c('.mzML','.mzXML'), width = '100%'),
                                   uiOutput('gofurtherwithprocessing'),
                                   uiOutput('insertnumberofdifferentchannels'),
                                   uiOutput('insertmanuallythescansofdifferentchannels'),
                                   uiOutput('warningtextORdownload'),
                                   uiOutput('downloadprocessedfullscanfiles')
                                 )),
                        
             ),
             
             
             #  footer=HTML('</div><footer><center><img src="european.union.logo.png" width="300" alt="This project is funded by the European Union" /></center></footer>') #<a href="http://www.answer-itn.eu/" target="_blank">
             collapsible = TRUE,position="static-top"
  ), #End navbar
  
  HTML(paste0('	<!--=== Footer Version 1 ===-->
       <div class="footer-v1">
       <div class="copyright">
       <div class="container">
       <div class="row">
       <div class="col-md-6">
       <p>2018-',format(as.Date(Sys.time(), format="%d/%m/%Y"),"%Y"), ' &copy; All Rights Reserved.</p>
       </div><!--/footer-v1-->
       </div><!--/copyright-->
       </div><!--/container-->
       </div><!--/row-->
       </div><!--/col-md-6-->
       <!--=== End Footer Version 1 ===-->
       <!-- JS Global Compulsory -->
      <script src="assets/plugins/jquery/jquery-migrate.min.js"></script>
      <script src="assets/plugins/bootstrap/js/bootstrap.min.js"></script>

       <!-- JS DataTables -->
       <script src="assets/js/jquery.dataTables.min.js"></script>

       <!-- JS Customization -->
       <script src="assets/js/jquery.blockUI.js"></script>
       <script src="assets/js/jquery.redirect.js"></script>
       <script src="assets/js/custom.js"></script>

                         
 <!-- JS Implementing Plugins -->
 <script src="assets/plugins/back-to-top.js"></script>
 <script src="assets/plugins/smoothScroll.js"></script>
 <script src="assets/plugins/sky-forms-pro/skyforms/js/jquery.maskedinput.min.js"></script>
 <script src="assets/plugins/sky-forms-pro/skyforms/js/jquery-ui.min.js"></script>
 <script src="assets/plugins/sky-forms-pro/skyforms/js/jquery.validate.min.js"></script>
                                                         
 <!-- JS Page Level -->
 <script src="assets/js/app.js"></script>
 <script src="assets/js/plugins/style-switcher.js"></script>
 <script src="assets/js/plugins/masking.js"></script>
 <script src="assets/js/plugins/datepicker.js"></script>
 <script src="assets/js/plugins/validation.js"></script>


       </body>
       </html>
       '))
  
)


server<-shinyServer(function(input, output, session){
  options(warn = -1) #suppress warnings
  options(shiny.maxRequestSize = 4800*1024^2)  

  observeEvent(input$Sampletobeprocessed, {
    output$gofurtherwithprocessing <- renderUI({})
    output$gofurtherwithprocessing <- renderUI({})
    output$insertnumberofdifferentchannels <- renderUI({})
    output$insertmanuallythescansofdifferentchannels <- renderUI({})
    output$warningtextORdownload <- renderUI({})
    output$downloadprocessedfullscanfiles <- renderUI({})
    output$cutoffandsubmit <- renderUI({})
    output$download_Datadependent <- renderUI({})
    
    library("peakTrAMS")
    
    pathtofile<-strsplit(gsub(x=input$Sampletobeprocessed$datapath,pattern="\\\\", replacement="/"),"0.m")[[1]][1]
    assign("pathtofile",pathtofile,.GlobalEnv)
    
    if(grepl(x=sessionInfo()$running,pattern="Windows")) command<-paste("C:","\n","cd",pathtofile,"\n","msconvert.exe * --mzXML --filter \"peakPicking true 1-\" --filter \"threshold absolute 10000000000 most-intense\"")
    else if(grepl(x=sessionInfo()$running,pattern="Ubuntu")) command<-paste("cd",pathtofile,"\n","msconvert * --mzXML --filter \"peakPicking true 1-\" --filter \"threshold absolute 10000000000 most-intense\"")
    else command<-paste("C:","\n","cd",pathtofile,"\n","msconvert.exe * --mzXML --filter \"peakPicking true 1-\" --filter \"threshold absolute 10000000000 most-intense\"")
    
    write(command,file=paste0(pathtofile,"/convertANDcutnoise.bat"))
    
    if(grepl(x=sessionInfo()$running,pattern="Windows")) system(paste0(pathtofile,"/convertANDcutnoise.bat"), intern = FALSE)
    else if(grepl(x=sessionInfo()$running,pattern="Ubuntu")) system(command, intern = FALSE)
    else system(paste0(pathtofile,"/convertANDcutnoise.bat"), intern = FALSE)
    
    
    
    
    manyfiles<-dir(pathtofile, full.names=TRUE, pattern=".mz")
    sizeofmanyfiles<-tail(file.info(manyfiles)$size)
    skeleton<-manyfiles[which.min(sizeofmanyfiles)]
    print(skeleton)
    
    Sampleinmemory<-read.mzXML(skeleton)
    assign("Sampleinmemory",Sampleinmemory,.GlobalEnv)
    
    informationforacceptance<-getinfo(Sampleinmemory)
    #assign("informationforacceptance",informationforacceptance,.GlobalEnv)
    if(length(unique(informationforacceptance$CE))<=10 & #Here it says that max 10 collision energy channels for data-independent files
       c(sum(is.na(informationforacceptance$precursor))==length(informationforacceptance$precursor) || 
         length(unique(round(as.numeric(informationforacceptance$precursor[!is.na(informationforacceptance$precursor)]),0)))<=1 ||
         min(as.numeric(informationforacceptance$precursor[!is.na(informationforacceptance$precursor)]))>500)){
      #Here it says that if precursor is NA then all scans should be NA, OR if precursors exist in data-independent (not normal) then should be more or less the same number OR all precursor masses> 500. The last two cases happens in Agilent files
      
      
      
      output$gofurtherwithprocessing <- renderUI({
        informationsample<-getinfo(Sampleinmemory)
        if(length(unique(informationsample$CE))>1){
          div(id="a",
              helpText("Please, specify intensity cutoff below. (average cutoffs for TOFs for 250 counts in (+)-ESI and 150 counts for (-)-ESI. For Orbitraps cut of is 88000 for (+)-ESI and 50000 for (-)-ESI"),
              numericInput(inputId="intensitycutoffforscans",label="Intensity cutoff", value=250, min=0, max=100000, step=1),
              textAreaInput(inputId='vec1', label='Remove lock-mass calibrant peaks. Enter for example "1,2,3,4" (comma delimited)',placeholder = "1,2,3,4"),
              # verbatimTextOutput("vec1_output"),
              actionButton(inputId="startprocessing", label="Submit Processing"))
        } else {
          div(id="beta",
              helpText("Unfortunately, your file does not contain collision energy information. You should specify which scans belong to which layer. Follow the instructions below"),
              helpText("Please, specify intensity cutoff below. (average cutoffs for TOFs for 250 counts in (+)-ESI and 150 counts for (-)-ESI. For Orbitraps cut of is 88000 for (+)-ESI and 50000 for (-)-ESI"),
              numericInput(inputId="intensitycutoffforscans",label="Intensity cutoff", value=250, min=0, max=100000, step=1),
              textAreaInput(inputId='vec2', label='Remove lock-mass or calibrant peaks. Enter for example "1,2,3,4" (comma delimited)',placeholder = "1,2,3,4"),
              # verbatimTextOutput("vec2_output"),
              numericInput(inputId="howmanycollisionenergiesarecontainedinthefile",label="How many collision energy channel's are contained in the chromatogram", min=2, max=10, value=2, step=1)
          )
        }
      })
      
      output$downloadprocessedfullscanfiles <- renderUI({})
      output$warningtextORdownload <- renderUI({})
      output$insertnumberofdifferentchannels <- renderUI({})
      output$insertmanuallythescansofdifferentchannels <- renderUI({})
      output$warningtextORdownload <- renderUI({})
      output$downloadprocessedfullscanfiles <- renderUI({})
    } else {
      output$gofurtherwithprocessing <- renderUI({
        div(id="alfabetaerror",
            HTML(paste("<font color=\"#FF0000\"><b>", "Seems like your data is Data-Dependent!", "</b></font>"))
        )
      })
    }
    
    
  }, ignoreInit=TRUE)
  
  #Seperate "Data-Independent" layers if NO collision energies are included in the file
  observeEvent(input$howmanycollisionenergiesarecontainedinthefile,{
    output$insertnumberofdifferentchannels <- renderUI({ 
      div(id="gamma",
          lapply(1:input$howmanycollisionenergiesarecontainedinthefile, function(i) {
            numericInput(inputId=paste0("whichcollisionenergiesarecontainedinthefile",i),label="Which nominal collision energy channel's are contained in the chromatogram (i.e. 4eV, 20eV, 40eV)",
                         min=1, max=100, value=c(4,25,30,40,50,60,70,80,90,100)[i], step=1)
            
          }),
          actionButton(inputId="gaveinformationisdone", label="Ι finished with inputing the information")
      )
    })
  }, ignoreInit=TRUE)
  observeEvent(input$gaveinformationisdone,{
    shinyjs::disable("gaveinformationisdone")
    shinyjs::disable("howmanycollisionenergiesarecontainedinthefile") 
    
    lapply(1:input$howmanycollisionenergiesarecontainedinthefile, function(i) {
      shinyjs::disable(paste0("whichcollisionenergiesarecontainedinthefile",i))
    })
    
    output$insertmanuallythescansofdifferentchannels <- renderUI({ 
      div(id="delta",
          lapply(1:input$howmanycollisionenergiesarecontainedinthefile, function(i) {
            textAreaInput(inputId=paste0("channelnumber",i), 
                          placeholder = "1,2,3,4",
                          #label='Remove lock-mass or calibrant peaks. Enter for example "1,2,3,4" (comma delimited)',
                          label=paste0("Write down the scans of channel of collision energy: ",input[[paste0("whichcollisionenergiesarecontainedinthefile",i)]])
            )
          }),
          actionButton(inputId="startprocessing2", label="I finished with inputting the information")
      )
    })
  }, ignoreInit=TRUE)

  observeEvent(input$startprocessing2, {
    i<-1; channels<-list(); lengthscansofchannels<-c(); tempscanlist<-c()
    for(i in 1:input$howmanycollisionenergiesarecontainedinthefile){
      tempscanlist <- input[[paste0("channelnumber",i)]]
      tempscanlist <- suppressWarnings(as.numeric(unlist(strsplit(tempscanlist,","))))
      tempscanlist <- tempscanlist[!is.na(tempscanlist)]
      tempscanlist <- tempscanlist[tempscanlist>0] #positive number of scans
      tempscanlist <- round(tempscanlist,0) #decimal numbers that user may input
      tempscanlist <- unique(tempscanlist)
      
      channels[[i]]<-tempscanlist
      lengthscansofchannels[i]<-length(tempscanlist)
    }
    
    assign("channels",channels,.GlobalEnv)
    assign("lengthscansofchannels",lengthscansofchannels,.GlobalEnv)
    
    if(sum(lengthscansofchannels==0)!=0){ #if nothing has been entered to scans
      output$warningtextORdownload <- renderUI({ 
        HTML(paste("<font color=\"#FF0000\"><b>", "You let the scans empty! Note that characters and sybmols other than numbers and comma are ignored. Dublicate numbers are regarded only once.", "</b></font>"))
      })
    } else {
      if(length(unique(unlist(channels)))!=length(unlist(channels))){
        output$warningtextORdownload <- renderUI({ 
          HTML(paste("<font color=\"#FF0000\"><b>", "A scan can belong only in 1 collision energy channel. Note that characters and sybmols other than numbers and comma are ignored. Dublicate numbers are regarded only once", "</b></font>"))
        })
      } else {
        p<-1; matchingcalibrantswithothers<-c()
        removecalibrant<-as.numeric(unlist(strsplit(input$vec2,",")))
        removecalibrant<-removecalibrant[!is.na(removecalibrant)]
        for(p in 1:length(removecalibrant)){
          matchingcalibrantswithothers[p]<-match(removecalibrant[p], unique(unlist(channels)))
        }
        matchingcalibrantswithothers<-matchingcalibrantswithothers[!is.na(matchingcalibrantswithothers)]
        print(matchingcalibrantswithothers)
        if((length(matchingcalibrantswithothers)!=0)){
          # print(unique(unlist(channels)) )
          # print(as.numeric(unlist(strsplit(input$vec2,","))))
          output$warningtextORdownload <- renderUI({ 
            HTML(paste("<font color=\"#FF0000\"><b>", "!Removed scans can not belong to any channels. Note that characters and sybmols other than numbers and comma are ignored. Dublicate numbers are regarded only once", "</b></font>"))
          })
        } else {
          # print(input[[paste0("channelnumber",1)]])
          # print(input$howmanycollisionenergiesarecontainedinthefile)
          # length(Sampleinmemory)
          # print(input$Sampletobeprocessed$datapath)
          # output$warningtextORdownload <- renderUI({
          #   HTML(paste("<font color=\"FF0000\"><b>", "Good boy", "</b></font>"))
          # })        
          #      
          
          withProgress(message = 'Processing', value = 0, {
            #Step 1
            incProgress(0.14, detail = paste("Step", 1))
            
            pathtofile<-strsplit(gsub(x=input$Sampletobeprocessed$datapath,pattern="\\\\", replacement="/"),"0.m")[[1]][1]
            
            if(grepl(x=sessionInfo()$running,pattern="Windows")) command<-paste("C:","\n","cd",pathtofile,"\n","msconvert.exe * --mzXML --filter \"peakPicking true 1-\" --filter \"threshold absolute ",input$intensitycutoffforscans," most-intense\"")
            else if(grepl(x=sessionInfo()$running,pattern="Ubuntu")) command<-paste("cd",pathtofile,"\n","msconvert * --mzXML --filter \"peakPicking true 1-\" --filter \"threshold absolute ",input$intensitycutoffforscans," most-intense\"")
            else command<-paste("C:","\n","cd",pathtofile,"\n","msconvert.exe * --mzXML --filter \"peakPicking true 1-\" --filter \"threshold absolute ",input$intensitycutoffforscans," most-intense\"")
            
            write(command,file=paste0(pathtofile,"/convertANDcutnoise.bat"))
            
            if(grepl(x=sessionInfo()$running,pattern="Windows")) system(paste0(pathtofile,"/convertANDcutnoise.bat"), intern = FALSE)
            else if(grepl(x=sessionInfo()$running,pattern="Ubuntu")) system(command, intern = FALSE)
            else system(paste0(pathtofile,"/convertANDcutnoise.bat"), intern = FALSE)
            
            
            
            
            print(list.files(pathtofile,pattern=".mzXML",full.names = TRUE)[which.max(nchar(list.files(pathtofile,pattern=".mzXML",full.names = TRUE)))])
            
            #Step 2
            incProgress(0.14, detail = paste("Step", 2))
            
            #Read the file
            Sampleinmemory<-read.mzXML(list.files(pathtofile,pattern=".mzXML",full.names = TRUE)[which.max(nchar(list.files(pathtofile,pattern=".mzXML",full.names = TRUE)))])
            
            #Step 3
            incProgress(0.14, detail = paste("Step", 3))
            
            
            ##Fix problems 1. sequencial scan numbers and 2.round intensities and masses
            i<-1
            for(i in 1:length(getinfo(Sampleinmemory)$scan)){
              
              Sampleinmemory$scan[[i]]$num<-i
              Sampleinmemory$scan[[i]]$parentNum<-i
              Sampleinmemory$scan[[i]]$scanOrigin<-paste(strsplit(Sampleinmemory$scan[[1]]$scanOrigin,"num=")[[1]][1],
                                                         "num='",i,"'/>\n", sep="")
              
              if(length(Sampleinmemory$scan[[i]]$mass)>0){ #if scan is not empty 
                Sampleinmemory$scan[[i]]$mass<-round(Sampleinmemory$scan[[i]]$mass,5)
                Sampleinmemory$scan[[i]]$peaks<-round(Sampleinmemory$scan[[i]]$peaks,0)
              }
            }
            
            #Get information
            informationsample<-getinfo(Sampleinmemory)
            assign("informationsample",informationsample,.GlobalEnv)
            assign("Sampleinmemory_before",Sampleinmemory,.GlobalEnv)
            
            #Step 4
            incProgress(0.14, detail = paste("Step", 4))
            
            # Sampleinmemory<-removenoise(Sampleinmemory,noisethreshold = input$intensitycutoffforscans)
            #Remove calibrants
            removecalibrantpeaks <- suppressWarnings(as.numeric(unlist(strsplit(input$vec2,","))))
            removecalibrantpeaks <- removecalibrantpeaks[!is.na(removecalibrantpeaks)]
            removecalibrantpeaks <- removecalibrantpeaks[removecalibrantpeaks>0]
            removecalibrantpeaks <- round(removecalibrantpeaks,0)
            removecalibrantpeaks <- unique(removecalibrantpeaks)
            assign("removecalibrantpeaks",removecalibrantpeaks,.GlobalEnv)
            assign("Sampleinmemory",Sampleinmemory,.GlobalEnv)
            
            
            if(length(removecalibrantpeaks)>0){
              Sampleinmemory<-removescans(mzXML=Sampleinmemory_before, #Should be Sampleinmemory instead of Sampleinmemory_before
                                          scansORtime=removecalibrantpeaks,
                                          time=FALSE)
              
              #Fix numbers and convert mslevel to 1 for all scans
              i<-1
              for(i in 1:length(Sampleinmemory$scan)){
                Sampleinmemory$scan[[i]]$num<-
                  as.numeric(
                    strsplit(Sampleinmemory$scan[[i]]$scanOrigin,split="\\D+")[[1]]
                    
                    [length(strsplit(Sampleinmemory$scan[[i]]$scanOrigin,split="\\D+")[[1]])]
                    
                    )
                Sampleinmemory$scan[[i]]$msLevel<-1
              }
            }
            
            
            
            assign("Sampleinmemory",Sampleinmemory,.GlobalEnv)
            
            
            #Step 5
            incProgress(0.14, detail = paste("Step", 5))
            
            
            if(length(removecalibrantpeaks)>0) scansremained<-setdiff(informationsample$scan,removecalibrantpeaks)
            else scansremained<-informationsample$scan
            assign("scansremained",scansremained,.GlobalEnv)
            
            
            i<-1; ms<-list();
            if(length(removecalibrantpeaks)>0){
              removescans_local<-function(mzXML=blank_HILIC,scansORtime=c(17,25),time=TRUE){
                getinfo_local <- function(sample){
                  numscan<-sample$scan[[1]]$num
                  
                  info<-data.frame(scan=numscan:c(length(sample[[5]])+numscan-1),timeofscan=0)
                  for(numscan in 1:c(length(sample[[5]])+numscan-1)){ 
                    if(length(strsplit(try(sample[[5]][[numscan]][[6]], silent=T),"Error")[[1]])!=2){
                      info$timeofscan[numscan]<-sample[[5]][[numscan]][[6]]
                      info[numscan,2]<-as.numeric(strsplit(strsplit(info[numscan,2],split="S")[[1]][1],split="PT")[[1]][2])
                      #  info$basePeakMz[numscan]<-sprintf("%.5f",sample$scan[[i]]$mass[which.max(sample$scan[[numscan]]$mass)])
                      #  info$basePeakIntensity[numscan]<-as.numeric(sprintf("%.0f",max(sample$scan[[numscan]]$peaks)))
                    }
                  }
                  info$timeofscan<-as.numeric(info$timeofscan)
                  
                  numscan<-sample$scan[[1]]$num
                  for(numscan in 1:c(length(sample[[5]])+numscan-1)){ 
                    if(length(strsplit(try(sample[[5]][[numscan]][[6]], silent=T),"Error")[[1]])!=2){
                      info$mslevel[numscan]<-(sample[[5]][[numscan]][[5]])
                      info$numofpeaks[numscan]<-length(sample[[5]][[numscan]][[1]])
                      info$CE[numscan]<-strsplit(strsplit(sample$scan[[numscan]]$scanAttr, "collisionEnergy=[\"]")[[1]][2], "[\"]")[[1]][1]
                    }
                  }
                  info$precursor<-NA
                  info$precursorIntensity<-NA
                  
                  i<-1
                  for(i in 1:length(info[,1])){
                    if(length(strsplit(try(sample[[5]][[numscan]][[6]], silent=T),"Error")[[1]])!=2){
                      if(info$mslevel[i]!=1){
                        info$precursor[i]<-as.numeric(strsplit(strsplit(sample$scan[[i]]$precursorMz," </precursorMz>\n")[[1]][1],">   ")[[1]][2])
                        info$precursorIntensity[i]<-as.numeric(strsplit(sample$scan[[i]]$precursorMz,"[\"]")[[1]][2])
                      }
                    }
                  }
                  sprintf("Done")
                  
                  info<-info[info$timeofscan!=0,]
                  return(info)
                }
                if(scansORtime[2]=="end" & time==FALSE) scansORtime[2]<-max(getinfo_local(mzXML)$scan)
                if(scansORtime[2]=="end" & time==TRUE) scansORtime[2]<-max(getinfo_local(mzXML)$timeofscan)/60
                if(scansORtime[1]=="beginning" & time==FALSE) scansORtime[1]<-min(getinfo_local(mzXML)$scan)
                if(scansORtime[1]=="beginning" & time==TRUE) scansORtime[1]<-min(getinfo_local(mzXML)$timeofscan)/60
                scansORtime<-as.numeric(scansORtime)
                scansORtime2<-scansORtime
                
                info<-getinfo_local(sample=mzXML)
                
                if(time==TRUE){
                  k<-which.min(abs(info$timeofscan-scansORtime[2]*60))
                  if(info$mslevel[which.min(abs(info$timeofscan-scansORtime[2]*60))]!=1 & k!=length(info[,1])){
                    while(info$mslevel[k]!=1) {
                      k <- k+1 
                      scansORtime[2]<-info$timeofscan[k]/60
                    }
                    k<-k-1
                    cat("Ending point was set at", paste(round(c(info$timeofscan[k]/60),4)), "because given ending retention time", scansORtime2[2] ,"corresponds to scan at MS2 level","\n")
                  }
                }
                
                
                if(time==TRUE){
                  u<-which.min(abs(info$timeofscan-scansORtime[1]*60))
                  if(info$mslevel[which.min(abs(info$timeofscan-scansORtime[1]*60))]!=1){
                    info<-getinfo_local(mzXML)
                    while(info$mslevel[u]!=1) {
                      u <- u-1 
                      scansORtime[1]<-info$timeofscan[u]/60
                    }
                    u<-u-1
                    cat("Beginning point was set at", paste(round(c(info$timeofscan[u]/60),4)), "because given ending retention time", scansORtime2[1] ,"corresponds to scan at MS2 level","\n")
                  }
                }
                
                if(time==TRUE){
                  if(!is.na(info$mslevel[which.min(abs(info$timeofscan-scansORtime[2]*60))+1]!=1)){
                    if(info$mslevel[which.min(abs(info$timeofscan-scansORtime[2]*60))]==1 & info$mslevel[which.min(abs(info$timeofscan-scansORtime[2]*60))+1]!=1) k<-which.min(abs(info$timeofscan-scansORtime[2]*60))-1
                  }
                  if(!is.na(info$mslevel[which.min(abs(info$timeofscan-scansORtime[1]*60))+1]!=1)){
                    if(info$mslevel[which.min(abs(info$timeofscan-scansORtime[1]*60))]==1 & info$mslevel[which.min(abs(info$timeofscan-scansORtime[1]*60))+1]!=1) u<-which.min(abs(info$timeofscan-scansORtime[1]*60))-1
                  }
                }
                
                if(time==TRUE){
                  info<-getinfo_local(mzXML)[u:k,]
                  stayordelete<-c(rep(TRUE,length(mzXML[[5]])))
                  stayordelete[info$scan]<-FALSE
                } else {
                  stayordelete<-c(rep(TRUE,length(mzXML[[5]])))
                  
                  numscanfrommzXML<-c(); k<-1
                  for(k in 1:length(mzXML[[5]])){
                    numscanfrommzXML[k]<-as.numeric(strsplit(mzXML$scan[[k]]$scanOrigin,split="\\D+")[[1]][length(strsplit(mzXML$scan[[k]]$scanOrigin,split="\\D+")[[1]])])
                  }
                  # cbind(stayordelete,numscanfrommzXML,scansORtime)
                  
                  t<-1; p<-1
                  for(p in 1:length(scansORtime)){
                    if(length(which(numscanfrommzXML==scansORtime[p]))>0){ #Avoid the case in which scansOtime is not included in numscanfrommzXML
                      scansORtime2[t]<-which(numscanfrommzXML==scansORtime[p])
                      t<-t+1
                    }
                  }
                  scansORtime2
                  
                  
                  stayordelete[scansORtime2]<-FALSE
                }
                
                new_sample<-list()
                new_sample<-mzXML[1:4]
                new_sample$scan<-mzXML[[5]][c(stayordelete)]
                attr(new_sample, "class") = "mzXML"
                
                
                i<-1
                for(i in 1:length(new_sample$scan)) new_sample[[5]][[i]]$num<-i
                
                return(new_sample)
              }
              
              for(i in 1:length(channels)){
                ms[[i]]<-removescans_local(mzXML=Sampleinmemory,
                                           scansORtime=setdiff(scansremained, channels[[i]]),
                                           time=FALSE)
              }
              
            } else {
              for(i in 1:length(channels)){
                ms[[i]]<-removescans(mzXML=Sampleinmemory,
                                     scansORtime=setdiff(scansremained, channels[[i]]),
                                     time=FALSE)
            }
            }
            
            assign("ms",ms,.GlobalEnv)
            
            #Step 6
            incProgress(0.14, detail = paste("Step", 6))
            # print(input[[paste0("whichcollisionenergiesarecontainedinthefile",1)]])
            # print(input[[paste0("whichcollisionenergiesarecontainedinthefile",2)]])
            
            
            i<-1; j<-1
            for(i in 1:length(ms)){
              
              if(grepl(x=ms[[i]]$scan[[j]]$scanAttr, pattern="collisionEnergy=\"NA\"")){
                
              for(j in 1:length(ms[[i]]$scan)){
                ms[[i]]$scan[[j]]$scanAttr<-gsub(x=ms[[i]]$scan[[j]]$scanAttr,
                                                 pattern="collisionEnergy=\"NA\"",
                                                 replacement=paste0("collisionEnergy=\"",input[[paste0("whichcollisionenergiesarecontainedinthefile",i)]],
                                                                    "\""))
              }
              
                #The idea is to include collisionEnergy value if it is not there.
              # } else {
              #   if(!grepl(x=ms[[i]]$scan[[j]]$scanAttr, pattern="collisionEnergy")){
              #     for(j in 1:length(ms[[i]]$scan)){
              #       tmp<-strsplit(ms[[i]]$scan[[j]]$scanAttr, "\"")
              # 
              #       ms[[i]]$scan[[j]]$scanAttr<-
              #     }
              #   }
              # }
              
            }
            }
            
            
            
            
            
            
            
            #Step 7
            incProgress(0.16, detail = paste("Step", 7))
            
            
            
            output$warningtextORdownload <- renderUI({
              lapply(1:length(ms), function(i) {
                downloadButton(paste0("processedfile",i), paste0("Download channel of energy: ",input[[paste0("whichcollisionenergiesarecontainedinthefile",i)]]) )
              })
            })
            
            
            ####################
            pathtofile<-strsplit(gsub(x=input$Sampletobeprocessed$datapath,pattern="\\\\", replacement="/"),"0.m")[[1]][1]
            print(pathtofile)
            
            i<-1; myfile<-c()
            for(i in 1:length(ms)){
              newfile<-c(paste0(pathtofile,"/",input[[paste0("whichcollisionenergiesarecontainedinthefile",i)]],"_",paste(strsplit(input$Sampletobeprocessed$name,"\\.")[[1]][1:c(length(strsplit(input$Sampletobeprocessed$name,"\\.")[[1]])-1)],collapse="."),".mzXML"))
              print(newfile)
              
              write.mzXML(ms[[i]], file=newfile, precision=c('64'))
              
              if(grepl(x=sessionInfo()$running,pattern="Windows")) command<-paste0("C:","\n","cd ",pathtofile,"\n","msconvert.exe \"", newfile, "\" ",
                                                                                   "--mzML --filter \"peakPicking true 1-\" --outfile \"", 
                                                                                   paste0(strsplit(strsplit(newfile,"/")[[1]][length(strsplit(newfile,"/")[[1]])],".mzXML")[[1]][1],
                                                                                          ".mzML"), "\"")
              else if(grepl(x=sessionInfo()$running,pattern="Ubuntu")) command<-paste0("cd ",pathtofile,"\n","msconvert \"", newfile, "\" ",
                                                                                       "--mzML --filter \"peakPicking true 1-\" --outfile \"", 
                                                                                       paste0(strsplit(strsplit(newfile,"/")[[1]][length(strsplit(newfile,"/")[[1]])],".mzXML")[[1]][1],
                                                                                              ".mzML"), "\"")
              else command<-paste0("C:","\n","cd ",pathtofile,"\n","msconvert.exe \"", newfile, "\" ",
                                   "--mzML --filter \"peakPicking true 1-\" --outfile \"", 
                                   paste0(strsplit(strsplit(newfile,"/")[[1]][length(strsplit(newfile,"/")[[1]])],".mzXML")[[1]][1],
                                          ".mzML"), "\"")
              
              
              print(44)
              print(command)
              write(command,file=paste0(pathtofile,"/converttomzML.bat"))
              
              if(grepl(x=sessionInfo()$running,pattern="Windows")) system(paste0(pathtofile,"/converttomzML.bat"), intern = FALSE)
              else if(grepl(x=sessionInfo()$running,pattern="Ubuntu")) system(command, intern = FALSE)
              else system(paste0(pathtofile,"/converttomzML.bat"), intern = FALSE)
              
              
              myfile[i]<-paste0(pathtofile,"/",paste0(strsplit(strsplit(newfile,"/")[[1]][length(strsplit(newfile,"/")[[1]])],".mzXML")[[1]][1],".mzML"))
              assign("myfile",myfile,.GlobalEnv)      
            }
            ####################
            
            
            lapply(1:length(ms), function(i) {
              output[[paste0("processedfile",i)]] <- downloadHandler(
                filename = function() { 
                  paste0(input[[paste0("whichcollisionenergiesarecontainedinthefile",i)]],"_",paste(strsplit(input$Sampletobeprocessed$name,"\\.")[[1]][1:c(length(strsplit(input$Sampletobeprocessed$name,"\\.")[[1]])-1)],collapse="."),".mzML")
                },
                content = function(file) {
                  file.copy(myfile[i], file)
                  #write.mzXML(ms[[i]], file, precision=c('64'))
                }
              )
              
            })
            
            
          
            })
        }
      }
    }
    
    
    
    
    
    
    
  }, ignoreInit=TRUE)

  
  #Automatic recognition of CE channles
  observeEvent(input$startprocessing, {
    # pathtofile<-substr(x=gsub(x=input$Sampletobeprocessed$datapath,pattern="\\\\", replacement="/"),
    #                    start=1,
    #                    stop=c(nchar(gsub(x=input$Sampletobeprocessed$datapath,pattern="\\\\", replacement="/"))-2)
    # )
    pathtofile<-paste(strsplit(gsub(x=input$Sampletobeprocessed$datapath,pattern="\\\\", replacement="/"),"/")[[1]][1:c(length(strsplit(gsub(x=input$Sampletobeprocessed$datapath,pattern="\\\\", replacement="/"),"/")[[1]])-1)],collapse="/")
    
    # print(pathtofile)
    
    withProgress(message = 'Processing', value = 0, {
      #Step 1
      incProgress(0.14, detail = paste("Step", 1))
      
      if(grepl(x=sessionInfo()$running,pattern="Windows")) command<-paste("C:","\n","cd",pathtofile,"\n","msconvert.exe ",
                                                                          strsplit(input$Sampletobeprocessed$datapath,"/")[[1]][length(strsplit(input$Sampletobeprocessed$datapath,"/")[[1]])],
                                                                          " --mzXML --filter \"peakPicking true 1-\" --filter \"threshold absolute ",input$intensitycutoffforscans," most-intense\"")
      else if(grepl(x=sessionInfo()$running,pattern="Ubuntu")) command<-paste("cd",pathtofile,"\n","msconvert ",
                                                                              strsplit(input$Sampletobeprocessed$datapath,"/")[[1]][length(strsplit(input$Sampletobeprocessed$datapath,"/")[[1]])],
                                                                              " --mzXML --filter \"peakPicking true 1-\" --filter \"threshold absolute ",input$intensitycutoffforscans," most-intense\"")
      else command<-paste("C:","\n","cd",pathtofile,"\n","msconvert.exe ",
                          strsplit(input$Sampletobeprocessed$datapath,"/")[[1]][length(strsplit(input$Sampletobeprocessed$datapath,"/")[[1]])],
                          " --mzXML --filter \"peakPicking true 1-\" --filter \"threshold absolute ",input$intensitycutoffforscans," most-intense\"")
      
      
      
      write(command,file=paste0(pathtofile,"convertANDcutnoise.bat"))
      
      
      
      if(grepl(x=sessionInfo()$running,pattern="Windows")) system(paste0(pathtofile,"convertANDcutnoise.bat"), intern = FALSE)
      else if(grepl(x=sessionInfo()$running,pattern="Ubuntu")) system(command, intern = FALSE)
      else system(paste0(pathtofile,"convertANDcutnoise.bat"), intern = FALSE)
      
      #Convert 0.mzML/0.mzXML to original_file_name.mzXML using correct intesity cutoff
      
      
      allfilesinthedirectory<-list.files(pathtofile, pattern=".mz", full.names = TRUE) #Exclude the .bat file
      allfilesinthedirectory<-allfilesinthedirectory[!grepl(allfilesinthedirectory, pattern="/0.mz")] #exclude the originally uploaded file
      
      #Step 2
      incProgress(0.14, detail = paste("Step", 2))
      
      Sampleinmemory<-read.mzXML(allfilesinthedirectory)
      ##fix problems 1. sequencial scan numbers and 2.round intensities and masses
      i<-1
      for(i in 1:length(getinfo(Sampleinmemory)$scan)){ #
        Sampleinmemory$scan[[i]]$num<-i
        Sampleinmemory$scan[[i]]$parentNum<-i
        Sampleinmemory$scan[[i]]$scanOrigin<-paste(strsplit(Sampleinmemory$scan[[1]]$scanOrigin,"num=")[[1]][1],
                                                   "num='",i,"'/>\n", sep="")
        
        if(length(Sampleinmemory$scan[[i]]$mass)>0){
        Sampleinmemory$scan[[i]]$mass<-round(Sampleinmemory$scan[[i]]$mass,5)
        Sampleinmemory$scan[[i]]$peaks<-round(Sampleinmemory$scan[[i]]$peaks,0)
        }
      }
      
      
      #Step 3
      incProgress(0.14, detail = paste("Step", 3))
      
      #Sampleinmemory<-removenoise(Sampleinmemory,noisethreshold = input$intensitycutoffforscans)
      
      informationsample<-getinfo(Sampleinmemory)
      removecalibrantpeaks <- suppressWarnings(as.numeric(unlist(strsplit(input$vec1,","))))
      removecalibrantpeaks <- removecalibrantpeaks[!is.na(removecalibrantpeaks)]
      removecalibrantpeaks <- removecalibrantpeaks[removecalibrantpeaks>0]
      removecalibrantpeaks <- round(removecalibrantpeaks,0)
      removecalibrantpeaks <- unique(removecalibrantpeaks)
      if(length(removecalibrantpeaks)>0){
        Sampleinmemory<-removescans(Sampleinmemory,
                                    scansORtime=removecalibrantpeaks,
                                    time=FALSE)
      }
      
      informationsample<-getinfo(Sampleinmemory)
      if(sum(is.na(unique(suppressWarnings(as.numeric(informationsample$CE)))))>0){
        i<-1
        for(i in 1:length(Sampleinmemory$scan)){
          if(grepl(x=Sampleinmemory$scan[[i]]$scanAttr,pattern="collisionEnergy=\"NA\"")){
            Sampleinmemory$scan[[i]]$scanAttr<-gsub(x=Sampleinmemory$scan[[i]]$scanAttr,pattern="collisionEnergy=\"NA\"",
                                                    replacement="collisionEnergy=\"0\"")
          }
        }
      }
      
      if(sum(grepl(x=unique(informationsample$mslevel),pattern=2))>0){
        i<-1
        for(i in 1:length(Sampleinmemory$scan)){
          Sampleinmemory$scan[[i]]$msLevel<-1
        }
      }
      
      
      #Step 4
      incProgress(0.14, detail = paste("Step", 4))
      
      assign("Sampleinmemory",Sampleinmemory,.GlobalEnv)
      informationsample<-getinfo(Sampleinmemory)
      print(unique(informationsample$mslevel))
      
      i<-1; ms<-list();
      ce<-suppressWarnings(as.numeric(informationsample$CE))
      differentcollisionenergychannels<-unique(suppressWarnings(as.numeric(informationsample$CE)))
      
      if(sum(is.na(differentcollisionenergychannels))>0){
        differentcollisionenergychannels[is.na(differentcollisionenergychannels)]<-0
        ce[is.na(ce)]<-0
        informationsample$CE[is.na(as.numeric(informationsample$CE))]<-0
      }
      
      
      
      
      #Step 5
      incProgress(0.14, detail = paste("Step", 5))
      
      # pathtofile<-substr(x=gsub(x=input$Sampletobeprocessed$datapath,pattern="\\\\", replacement="/"),
      #                    start=1,
      #                    stop=c(nchar(gsub(x=input$Sampletobeprocessed$datapath,pattern="\\\\", replacement="/"))-2))
      pathtofile<-paste(strsplit(gsub(x=input$Sampletobeprocessed$datapath,pattern="\\\\", replacement="/"),"/")[[1]][1:c(length(strsplit(gsub(x=input$Sampletobeprocessed$datapath,pattern="\\\\", replacement="/"),"/")[[1]])-1)],collapse="/")
      
      i<-1; newfile<-c(); myfile<-c(); command2<-c()
      
      # if(grepl(x=sessionInfo()$running,pattern="Windows")) command2[1]<-paste0("C: ","\n ","cd ",pathtofile,"\n")
      # else if(grepl(x=sessionInfo()$running,pattern="Ubuntu")) command2[1]<-paste0("cd ",pathtofile,"/","\n")
      # else command2[1]<-paste0("C: ","\n ","cd ",pathtofile,"\n")
          
      for(i in 1:length(differentcollisionenergychannels)){
        ms[[i]]<-removescans(Sampleinmemory,
                             scansORtime=informationsample$scan[!(ce==sort(differentcollisionenergychannels,decreasing = FALSE,na.last =FALSE)[i])],
                             time=FALSE)
        write.mzXML(ms[[i]], file= paste0(pathtofile,"/",sort(unique(as.numeric(informationsample$CE)),decreasing = FALSE)[i],"_",paste(strsplit(input$Sampletobeprocessed$name,"\\.")[[1]][1:c(length(strsplit(input$Sampletobeprocessed$name,"\\.")[[1]])-1)],collapse="."),"!.mzXML"), precision=c('64'))
        print(1000)
        newfile[i]<-paste0(pathtofile,"/",sort(unique(as.numeric(informationsample$CE)),decreasing = FALSE)[i],"_",paste(strsplit(input$Sampletobeprocessed$name,"\\.")[[1]][1:c(length(strsplit(input$Sampletobeprocessed$name,"\\.")[[1]])-1)],collapse="."),"!.mzXML")
        
        if(grepl(x=sessionInfo()$running,pattern="Windows")) command2[c(i)]<-paste0("C: ","\n ","cd ",pathtofile,"\n",pathtofile,"\n", " msconvert.exe \"", newfile[i], "\" ",
                                                                                      # strsplit(newfile[i],"/")[[1]][length(strsplit(newfile[i],"/")[[1]])],"\"",
                                                                                      "--mzML --outfile \"",
                                                                                      paste0(strsplit(strsplit(newfile,"/")[[i]][length(strsplit(newfile,"/")[[1]])],".mzXML")[[1]][1],".mzML"),"\"")
        else if(grepl(x=sessionInfo()$running,pattern="Ubuntu")) command2[c(i)]<-paste0("cd ",pathtofile,"/","\n", " msconvert \"", newfile[i], "\" ",
                                                                                          #strsplit(newfile[i],"/")[[1]][length(strsplit(newfile[i],"/")[[1]])],"\"",
                                                                                          "--mzML --outfile \"",
                                                                                          paste0(strsplit(strsplit(newfile,"/")[[i]][length(strsplit(newfile,"/")[[1]])],".mzXML")[[1]][1],".mzML"),"\"")
        else command2[c(i)]<-paste0("C: ","\n ","cd ",pathtofile,"\n"," msconvert.exe \"", newfile, "\" ",
                                     # strsplit(newfile[i],"/")[[1]][length(strsplit(newfile[i],"/")[[1]])],"\"",
                                      "--mzML --outfile \"",
                                      paste0(strsplit(strsplit(newfile,"/")[[i]][length(strsplit(newfile,"/")[[1]])],".mzXML")[[1]][1],".mzML"),"\"")
        
        
         write(command2,file=paste0(pathtofile,"/converttomzML.bat"))
         
         if(grepl(x=sessionInfo()$running,pattern="Windows")){
           system(paste0(pathtofile,"/converttomzML.bat"), intern = FALSE)
         } else if(grepl(x=sessionInfo()$running,pattern="Ubuntu")){
           system(command2[i], intern = FALSE)
           write(system(command2, intern = TRUE),file=paste0(pathtofile,"/convertaaaaaaaaaaaatomzML.bat"))
         } else { 
           system(paste0(pathtofile,"/converttomzML.bat"), intern = FALSE)
         }
        
        
        # myfile[i]<-paste0(pathtofile,"/",paste0(strsplit(strsplit(newfile[i],"/")[[1]][length(strsplit(newfile[i],"/")[[1]])],".mzXML")[[1]][1],".mzML"))
        myfile[i]<-paste0(paste0(strsplit(newfile,"/")[[i]][1:c(length(strsplit(newfile,"/")[[1]])-1)],collapse="/"),"/",
                          gsub(strsplit(newfile,"/")[[i]][length(strsplit(newfile,"/")[[1]])],pattern=".mzXML", replacement=".mzML"))
      }
      assign("newfile",newfile,.GlobalEnv)
      assign("command2",command2,.GlobalEnv)
      assign("myfile",myfile,.GlobalEnv)      
      print(myfile)
      
      #Step 6
      incProgress(c(0.14+0.16), detail = paste("Step", 6))
      
      ## msconvert to mzXML file poy dhmiourghthhke me onoma to idio
      
      # command<-paste0("C: ","\n ","cd ",pathtofile," \n"," msconvert.exe *!.mzXML --mzML --outfile \"", 
      #                paste0(strsplit(strsplit(newfile,"/")[[1]][length(strsplit(newfile,"/")[[1]])],".mzXML")[[1]][1]
      #                       ,".mzML"),"\"")
      #command<-paste("C:","\n","cd",pathtofile,"\n","msconvert.exe", newfile, "--mzML --outfile", paste0(strsplit(strsplit(newfile,"/")[[1]][length(strsplit(newfile,"/")[[1]])],".mzXML")[[1]][1],".mzML")) 
      
      
      # write(command2,file=paste0(pathtofile,"/converttomzML.bat"))
      # 
      # if(grepl(x=sessionInfo()$running,pattern="Windows")){
      #   system(paste0(pathtofile,"/converttomzML.bat"), intern = FALSE)
      # } else if(grepl(x=sessionInfo()$running,pattern="Ubuntu")){
      #   system(command2, intern = FALSE)
      #   write(system(command2, intern = TRUE),file=paste0(pathtofile,"/convertaaaaaaaaaaaatomzML.bat"))
      # } else { 
      #   system(paste0(pathtofile,"/converttomzML.bat"), intern = FALSE)
      # }
      
      
    })
    
    output$downloadprocessedfullscanfiles <- renderUI({
      lapply(1:length(ms), function(i) {
        downloadButton(paste0("processedfile",i), paste0("Download channel of energy: ",sort(unique(as.numeric(informationsample$CE)),decreasing = FALSE)[i]))
      })
    })
    
    
    lapply(1:length(ms), function(i) {
      output[[paste0("processedfile",i)]] <- downloadHandler(
        filename = function() { 
          #paste0(sort(unique(as.numeric(informationsample$CE)),decreasing = FALSE)[i],"_",paste(strsplit(input$Sampletobeprocessed$name,"\\.")[[1]][1:c(length(strsplit(input$Sampletobeprocessed$name,"\\.")[[1]])-1)],collapse="."),".mzML")
          gsub(strsplit(newfile,"/")[[i]][length(strsplit(newfile,"/")[[1]])],pattern=".mzXML", replacement=".mzML")
        },
        content = function(file) {
          file.copy(myfile[i], file)
          # write.mzXML(ms[[i]], file, precision=c('64'))
        }
      )
      
    })
  }, ignoreInit=TRUE)

  
  })

shinyApp(ui, server)