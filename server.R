library(shiny)
library(readxl)
library(tools)
library(ggplot2)
library(dplyr)
library(VIM)
shinyServer(function(input, output,session) {
  
  #Value for the fliplab
  v <- reactiveValues(data = NULL)
  v1=reactiveValues(data=NULL)
  #reading the file (xlsx,txt,csv)
  f <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    #include xlsx files
    ext=file_ext(inFile)[1]
    if(identical("xlsx",ext)==F) return(f=read.csv(inFile$datapath, header=input$header, sep=input$sep,na.strings = input$na))
    else {file.rename(inFile$datapath,paste(inFile$datapath, ext, sep="."))
      return(f=readxl::read_excel(paste(inFile$datapath, ext, sep="."), 1,col_names = input$header,na = input$na))
    }       
  })
  
  #Tableur   --------------------------------------------------------------------------
  output$table <- renderDataTable({
    
    h=head(f(),100)
    h
  })
  
  #NA's treatement ---------------------------------------------------------------------
  sum_na=function(x){
    s=sum(is.na(x))
    return(s)}
  missingness=reactive({t(data.frame(sapply(f(), sum_na)*100/nrow(f())))})
  
  output$na_table1=renderTable({
    #Apply this function to data set 
    temp=missingness()
    row.names(temp)=" "
    temp
  })
  #missingess tolerance 
  output$na_text1=renderText({
    
    if(sum(missingness()[1,])==0){"Continuer l'analyse, la base de donées ne contient aucune valeur manquante"}
    else{  
      if(input$missinrate>max(missingness()[1,])) {"Vous pouvez ommettre les valeurs (observations) manquantes des variables suivantes :"}
      else {if(input$missinrate!=0 && input$missinrate>missingness()[1,]){"Votre anlayse requiert un traitement des valeurs manquantes. Pour se faire, il existe une multitude de méthodes pour traiter les valeurs manquantes (amputation), depuis la plus simple: amputation par la moyenne, à l'amputation par regression linéaire. Les variables conernées par ce traitement sont:\n"}
        else ""}
    }})
  
  output$plot_na=renderPlot({   withProgress(message = 'Traitement..', value = 0, {
    plot=aggr(f(), col=c("#619CFF","#F8766D"),lwd=0.1,
              numbers=TRUE, sortVars=TRUE, labels=names(f()), cex.axis=1, gap=3,
              ylab=c("Pourcentage des valeurs manquantes",""))
    Sys.sleep(0.1)
  })
  })
  
  #text of variables concerned by the missing values treatement 
  output$na_text2=renderPrint({
    numcol=ncol(missingness())
    
    for( i in 1:numcol){
      if(missingness()[1,i]<input$missinrate){
        if(missingness()[1,i]>0)
          HTML(paste0(cat('-',colnames(missingness())[i],'<br/>')))
       }
      }
    
  })
  
  output$na_text3=renderPrint({
    if(input$missinrate>max(missingness()[1,])){HTML("")}
    else{
    numcol=ncol(missingness())
    for( i in 1:numcol){
      if(missingness()[1,i]>input$missinrate){
        HTML(paste0(cat('-',colnames(missingness())[i],'<br/>')))
      }
    }
    }
  })
  
  #NA.ACTION
  #Mean imputation 
  imput_mean=function(vector){
    m <- mean(vector,na.rm = TRUE)
    return (ifelse (is.na(vector) == TRUE , round(m,digits = 3), vector)) 
  }
  
  #Na_treatement procedure
  na_treatement=function(df){
    numcol=ncol(df)
    for (i in 1:numcol){
      if(missingness()[1,i]>input$missinrate)
      {df=df[,-i]}
      else{
        if(missingness()[1,i]!=0 && any(class(df[i,])==c("integer","numeric"))==TRUE) {df[,i]=imput_mean(df[,i])}
        else {df=na.omit(df)}
      }
    }
    return(df)
  }
  
  
  new_f=reactive({na_treatement(f())})
  
  observeEvent(input$treat, {
    input$missinrate
    v$print="Traitement Effectué, Continuer l'analyse.."
  })
  
  output$warning_na=renderPrint({HTML(cat(v$print))
    
  })
  
  output$na_table2=renderTable({
    #Apply this function to data set 
    temp=t(data.frame(sapply(new_f(), sum_na)*100/nrow(new_f())))
    row.names(temp)=NULL
    temp
  })
  
  #Resume  ----------------------------------------------------------------------------
  
  output$summary=renderTable({
  
    summary(new_f())
  })
  
  #Select vars
  observe({ 
    updateSelectInput(session, "columname",
                      choices = colnames(f()))
  })
  
  # output$text_summary=renderText({
  #   
  # })
  output$slider <- renderUI({
    var=input$columname
    varuse=unlist(new_f()[,var])
    cls=class(varuse)
    if (any(cls==c("integer","numeric"))==TRUE) {
      Min=round(min(varuse),digits = 2)
      Max=round(max(varuse),digits = 2)
      sliderInput("inslider","Domaine de variation", min  =Min , 
                  max   = Max,
                  value = c(Min,Max))
    }
  })
  
  
  #Visualiser --------------------------------------------------------------------------
  output$hist=renderPlot({
    var=input$columname
    varuse=unlist(new_f()[,var])
    cls=class(varuse)
    input$inslider
    Date=new_f()[,1]
    
    withProgress(message = 'Visualisation..', value = 0, {Sys.sleep(0.5)
      if (any(colnames(Date)==c("date","Date"))==TRUE) {
        if(input$fliplab==TRUE){v$flip=theme(axis.text.x=element_text(angle=90))}
        else {v$flip=NULL}
              Date=unlist(Date)
              Date<- as.POSIXct(Date, tz = "GMT",origin="1970-1-1") 
               ggplot(NULL,aes(y=varuse,x=Date))+geom_line()+theme_classic()+v$flip+ylab(var)+xlab("Temps")
      }
      else{
        if(input$fliplab==TRUE){v$flip=theme(axis.text.x=element_text(angle=90))}
        else {v$flip=NULL}
      if (any(cls==c("integer","numeric"))==TRUE) {
        validate(need(input$inslider != "","Création du graphique ..."))
        v$Xlim=xlim(c(input$inslider[1],input$inslider[2]))
        ggplot(NULL,aes(x=varuse))+xlab(var)+ylab("Fréquence")+
          geom_histogram(bins=input$bins)+theme_classic()+v$flip+v$Xlim}
      
      else {
        ggplot(NULL,aes(x=as.factor(varuse)))+xlab(var)+geom_bar()+theme_classic()+ylab("Fréquence")+v$flip}}
   }
   )
  })

  #Render report (main)--------------------------------------------------------------------------------------
 
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('mon-rapport', sep = '.', switch(
        input$format, PDF = 'pdf', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
  
})