##################################################### PREPROCESADO DE LOS DATOS 
# Datos
df_raw <- read_excel("BaseDatosFinal.xlsx", 
                 col_types = c("text", "text", "text", "date", "text", "numeric", "text", "text", "text", "numeric"))

# Función que compara lo escrito en el sistema de búsqueda con la columna nombre de nuestros datos
BuscaMatch <- function(datos, palabra, recommend){
    nombre <- c()
    filas <- c()
    encontrado <- c()
    
    for (fila in (1:nrow(datos))) {
        if (!(datos[fila,"NOMBRE"] %in% nombre)){
            nombre<- c(nombre, datos[fila,"NOMBRE"])
            filas<- c(filas, fila) # guardamos los índices de las filas con nombre de plan distinto
            if ((str_detect(datos[fila,"NOMBRE"], palabra)) == TRUE){
                encontrado <- c(encontrado, fila)  # guardamos los índices de las filas 
                # que han hecho match con el texto del usuario
            }}}
    
    # Creamos el df de salida:
    df_match <- datos %>% # dataframe original
        slice(encontrado) %>% # seleccionamos filas que han hecho match con el texto del usuario
        select(9,1,6,10,11) # seleccionamos las columnas que nos interesa mostrar
    
    # Mostramos el df:
    if (recommend == TRUE){
        
        # Ordenamos descendentemente por valoración y seleccionamos las columnas que nos interesen:
        df_match   %>% arrange(desc(VALORACION), PRECIO)  %>% slice(1)
    }
    
    
    else{
        # Ordenamos descendentemente por valoración y seleccionamos las columnas que nos interesen:
        df_match    %>% arrange(desc(VALORACION), PRECIO) %>% slice(2:nrow(df_match))
    }
    
}





# Función que saca la pastaña (interfaz)
modal_dialog <- function(df) {
    
    x <- "Submit Edits"
    
    shiny::modalDialog(
        title = "Más información",
        div(
            #class = "text-center",
            div(
                print("NOMBRE")
            ),
            div(
                print(df[1])
            ),
            div(
                print("---------------------------------------------------------------------------------------------------------------------------")
            ),
            div(
                print("DESCRIPCION:")
            ),
            div(
                print(df[2])
            ),
            div(
                print("---------------------------------------------------------------------------------------------------------------------------")
            ),
            div(
                print("LOCALIZACION:")
            ),
            div(
                print(df[3])
            ),
            div(
                print("---------------------------------------------------------------------------------------------------------------------------")
            ),
            div(
                print('HORARIO:')
            ),
            div(
                print(df[5])
            ),
            
            
            div(
                #class = "container",
                div(
                    style = "margin-top: 50px;",
                    
                    
                    shiny::actionButton(
                        inputId = "add_car",
                        label = "Introduce tu valoracion",
                        icon = shiny::icon("plus"),
                        class = "btn-success",
                        style="color: #fff; background-color: #85afcc; border-color: #2e6da4"
                    )
                )
            )
        ),
        size = 'm',
        easyClose = TRUE,
        footer = div(
            shiny::actionButton(inputId = "dismiss_modal",
                                label   = "Close",
                                class   = "btn-danger",
                                style="color: #fff; background-color: #85afcc; border-color: #2e6da4")
        )
        
        
    ) %>% shiny::showModal()
    
}



modal_dialog_IRI <- function(df,mpg) {
    
    x <- "Subir valoracion"
    
    
    shiny::modalDialog(
        title = "Danos tu opinion",
        div(
            shiny::numericInput(inputId = "mpg",
                                label = "Dale estrellas del 0 al 5 al plan!",
                                value = mpg, 
                                width = "200px")
            
            
            
        ),
        size = 'm',
        easyClose = TRUE,
        footer = div(
            shiny::actionButton(inputId = "final_edit",
                                label   = x,
                                icon = shiny::icon("edit"),
                                class = "btn-info",
                                style="color: #fff; background-color: #85afcc; border-color: #2e6da4"),
            shiny::actionButton(inputId = "dismiss_modal",
                                label   = "Close",
                                class   = "btn-danger",
                                style="color: #fff; background-color: #85afcc; border-color: #2e6da4")
        )
        
    ) %>% shiny::showModal()
    
}


# # Función que crea los botones de cada actividad

create_btns <- function(x) {
    x %>%
        purrr::map_chr(~
                           paste0(
                               '<div class = "btn-group" >
                   <button class="btn btn-default action-button btn-info action_button"  style="color: #fff; background-color: #85afcc; border-color: #2e6da4" id="edit_',
                               .x,  '" type="button" onclick=get_id(this.id)><i class="fa fa-eye"></i></button></div>'
                           ))
}

x <- create_btns(1:828)
df <- df_raw %>% dplyr::bind_cols(tibble("Buttons" = x))



##################################################################### SERVIDOR 
shinyServer(function(input, output, session) {
    
    toggleModal(session, "startupModal", toggle = "open")
    
    # Pulsar boton anyadir valoracion
    shiny::observeEvent(input$add_car, {
        modal_dialog_IRI(df = distinct(df),mpg="")
    })
    
    # when final edit button is clicked, table will be changed
    shiny::observeEvent(input$final_edit, {
        shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "edit"))
        
        for (i in (1:nrow(rv$df))){
            
            if (rv$df[rv$dt_row, ][1]==rv$df[i,][1]){
                
                
                rv$edited_row <- dplyr::tibble(
                    rv$df[i, ][1],
                    rv$df[i, ][2],
                    rv$df[i, ][3],
                    rv$df[i, ][4],
                    rv$df[i, ][5],
                    rv$df[i, ][6],
                    rv$df[i, ][7],
                    rv$df[i, ][8],
                    rv$df[i, ][9],
                    VALORACION = as.double((as.double(rv$df[i, ][10]) + as.double(input$mpg))/2), 
                    rv$df[i, ][11])
                
                rv$df[i,] <- rv$edited_row
            }
            
        }
        
        
        
        
    })
    
    #Cambia datos en mytable
    proxy <- DT::dataTableProxy("mytable")
    shiny::observe({
        DT::replaceData(proxy, rv$df, resetPaging = FALSE, rownames = FALSE)
        
    })
    
    #Cambia datos en recommended
    proxy <- DT::dataTableProxy("recommended")
    shiny::observe({
        DT::replaceData(proxy, rv$df, resetPaging = FALSE, rownames = FALSE)
        
    })

    
    rv <- shiny::reactiveValues(
        df = distinct(df),
        dt_row = NULL,
        #add_or_edit = NULL,
        edit_button = NULL,
        keep_track_id = nrow(distinct(df)) + 1
    )
    
    # Sistema de filtrado ------------------------------------------------------
    
    # Cambiamos tipo de actividad si cambia Categorias popus
    observeEvent(
        input$changeAvatar,{
            if (input$changeAvatar!="Todas las categorias"){
                updateSelectInput(session, "changeAvatar1", "Tipo de actividad:", 
                                  choices = unique(rv$df[rv$df$`CATEGORIAS POPULARES`==input$changeAvatar,]$`TIPO DE ACTIVIDAD`))
            }
            else{
                updateSelectInput(session, "changeAvatar1", "Tipo de actividad:", 
                                  choices = "-")
            }
        }
    )
    
    
    # Mostramos la tabla en funcion de TODOS los filtros
    observeEvent(input$filtrox,{
        if (input$changeAvatar=="Todas las categorias"){
            print(rv$df %>% filter(FECHA==input$rango & `CATEGORIAS POPULARES`== input$changeAvatar & PRECIO >= input$RangoPrecios[1] & df$PRECIO <= input$RangoPrecios[2]) 
                  %>% select(9,1,6,10,11) %>%  arrange(desc(VALORACION), PRECIO) %>% slice(1)
                  )
            
            output$recommended <- DT::renderDT(
                {

                    shiny::isolate(rv$df %>% filter(FECHA==input$rango & `CATEGORIAS POPULARES`== input$changeAvatar & PRECIO >= input$RangoPrecios[1] & df$PRECIO <= input$RangoPrecios[2]) 
                                   %>% select(9,1,6,10,11) %>%  arrange(desc(VALORACION), PRECIO) %>% slice(1)
                    )
                },
                escape = F,
                rownames = FALSE,
                options = list(
                    lengthChange = FALSE,
                    initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().node()).css({'background-color': '#CDEED3'});",
                        "}"),
                    autowidth = TRUE,
                    columnDefs = list(list(width = '70%', targets = 1))
                )
            ) 
            
            output$mytable <- DT::renderDT(
                {
                    shiny::isolate(rv$df 
                                   %>% filter(`CATEGORIAS POPULARES`== input$changeAvatar & FECHA==input$rango & PRECIO >= input$RangoPrecios[1] & df$PRECIO <= input$RangoPrecios[2]) 
                                   %>%  select(9,1,6,10,11) %>%  arrange(desc(VALORACION), PRECIO) %>% slice(-1)
                                   )
                },
                escape = F,
                rownames = FALSE,
                options = list(processing = FALSE) 
            )                                                                                                                                                  
        }
        
        else {
            output$recommended <- DT::renderDT(
                {
                    shiny::isolate(rv$df %>% 
                                       filter(`TIPO DE ACTIVIDAD`==input$changeAvatar1 &
                                                  `CATEGORIAS POPULARES`== input$changeAvatar &
                                                  FECHA==input$rango & 
                                                  PRECIO >= input$RangoPrecios[1] & 
                                                  PRECIO <= input$RangoPrecios[2]) %>%
                                       select(9,1,6,10,11) %>%  
                                       arrange(desc(VALORACION), PRECIO) %>% 
                                       slice(1))
                },
                escape = F,
                rownames = FALSE,
                options = list(
                    lengthChange = FALSE,
                    initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().node()).css({'background-color': '#CDEED3'});",
                        "}"),
                    autowidth = TRUE,
                    columnDefs = list(list(width = '70%', targets = 1))
                )
            )
            
            output$mytable <- DT::renderDT(
                {
                    shiny::isolate(rv$df %>% 
                                       filter(`TIPO DE ACTIVIDAD`==input$changeAvatar1 & 
                                                  `CATEGORIAS POPULARES`== input$changeAvatar &
                                                  FECHA==input$rango & 
                                                  PRECIO >= input$RangoPrecios[1] &
                                                  PRECIO <= input$RangoPrecios[2]) %>% 
                                       select(9,1,6,10,11) %>% 
                                       arrange(desc(VALORACION), PRECIO) %>% 
                                       slice(-1))
                },
                escape = F,
                rownames = FALSE,
                options = list(processing = FALSE)
            )    
            
        }
    })
    
    # when edit button is clicked, modal dialog shows current editable row filled out
    observeEvent(input$current_id, {
        print(input$current_id)
        shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "edit"))
        rv$dt_row <- which(stringr::str_detect(rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
        df <- rv$df[rv$dt_row, ]
        
        modal_dialog(df=df)
        #rv$add_or_edit <- NULL
    })
    
    # Sistema de búsqueda -----------------------------------------------------
    #Esto es para la tabla principal sin haber usado el boton de la lupa
    output$recommended <- DT::renderDataTable({
        shiny::isolate(BuscaMatch(rv$df, input$userName,  TRUE) )
    },
    escape = F,
    rownames = FALSE,
    options = list(
        lengthChange = FALSE,
        initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().node()).css({'background-color': '#CDEED3'});",
            "}"),
        autowidth = TRUE,
        columnDefs = list(list(width = '70%', targets = 1))
    )
    ) 
    
    output$mytable <- DT::renderDataTable(
        {
            shiny::isolate(BuscaMatch(rv$df, input$userName, FALSE))
        }, 
        escape = F,
        rownames = FALSE,
        options = list(processing = FALSE)
    ) 
    
    # Esto es para mostrar las tablas si el botón ha sido usado
    observeEvent(input$searchButton,{
        output$recommended <- DT::renderDataTable({
            shiny::isolate(BuscaMatch(rv$df, input$userName,  TRUE) )
        },
        escape = F,
        rownames = FALSE,
        options = list(
            lengthChange = FALSE,
            initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().node()).css({'background-color': '#CDEED3'});",
                "}"),
            autowidth = TRUE,
            columnDefs = list(list(width = '70%', targets = 1))
        )
        ) 
        
        output$mytable <- DT::renderDataTable(
            {
                shiny::isolate(BuscaMatch(rv$df, input$userName,  FALSE))
            }, 
            escape = F,
            rownames = FALSE,
            options = list(processing = FALSE)
        ) 
    })
    # Navbar ------------------------------------------------------------------
    shinyjs::addClass(id = "navBar", class = "navbar-right")
    
    # DT Options --------------------------------------------------------------
    options(DT.options = list( lengthMenu = c(10, 20),
                               dom = 'tl'
    ))  # table and lengthMenu options
    
    # Intro JS ----------------------------------------------------------------
    observeEvent(input$help,
                 introjs(session, options = list("nextLabel"="Next",
                                                 "prevLabel"="Back",
                                                 "skipLabel"="Exit"))
    )
    
    
    
    # Initialize a variable to count how many times "btn1" is clicked.
    values <- reactiveValues(data = 1) 
    
    # Btn1 ---------------------------------------------------------------------
    # React to "btn1" being pressed by adding 1 to the values$data variable
    observeEvent( input$btn1, {
        if ( input$item_name == "" ) {
            showModal(modalDialog(title = "Pick a starting job first.",
                                  "It looks like you forgot to select a starting job. Please select a job from the drop-down
                                  menu to begin your career path.",
                                  easyClose = FALSE, size = "s" ))
        } else { 
            values$data = values$data + 1 }
        
    })
    
    # Go Back Button -----------------------------------------------------------
    
    observeEvent( input$goBack, {
        
        if (values$data <= 5) {
            enable("btn1")
        }
        
        if( values$data == 5 & !is.null(input$select5_rows_selected) ) {
            showModal(
                modalDialog("Please remove your selection before going back.", size = "s",
                            title = "Oops!",
                            footer = modalButton(label = "", icon = icon("close")))
            )
        } else if ( values$data == 4 & !is.null(input$select4_rows_selected) ) {
            showModal(
                modalDialog("Please remove your selection before going back.", size = "s",
                            title = "Oops!",
                            footer = modalButton(label = "", icon = icon("close")))
            )
        } else if ( values$data == 3 & !is.null(input$select3_rows_selected) ) {
            showModal(
                modalDialog("Please remove your selection before going back.", size = "s",
                            title = "Oops!",
                            footer = modalButton(label = "", icon = icon("close")))
            )
        } else if ( values$data == 2 & !is.null(input$select2_rows_selected) ) {
            showModal(
                modalDialog("Please remove your selection before going back.", size = "s",
                            title = "Oops!",
                            footer = modalButton(label = "", icon = icon("close")))
            )
        } else {
            values$data = values$data - 1
        }
    })
    
    # Disable btn1 when step 5 is reached
    useShinyjs()
    observeEvent( input$btn1, {
        if( values$data == 5 )
            shinyjs::disable("btn1")
    })
    
    # Disable goBack button at start of session
    observe( 
        if(values$data == 1){
            disable("goBack")
        } else {
            enable("goBack")    
        }
    )
    
    # Show/Hide Settings -------------------------------------------------------
    # Hide settings at start of new Shiny session
    observe(c(hide("changeAvatar1"),
              hide("changeAvatar"),
              hide("download"),
              hide("rango"),
              hide("RangoPrecios"),
              hide("filtrox")
              
    ))
    
    # Toggle visibility of settings
    observeEvent(input$settings, {
        shinyjs::toggle("changeAvatar", anim = TRUE)
        shinyjs::toggle("changeAvatar1", anim = TRUE)
        shinyjs::toggle("rango", anim = TRUE)
        shinyjs::toggle("RangoPrecios", anim = TRUE)
        shinyjs::toggle("filtrox", anim = TRUE)
    })
    
    # Preguntas página web -----------------------------------------------------
    # Escondemos las respuestas para que se muestren luego cuando se clique 
    # sobre su respectiva pregunta
    observe(c(hide("respuesta1.1"),
              hide("respuesta1.2"),
              hide("respuesta1.3"),
              hide("respuesta2.1"),
              hide("respuesta2.2"),
              hide("respuesta2.3")
    ))
    
    # Mostramos las respuestas sobre página web 
    observeEvent(input$pregunta1.1, {
        shinyjs::toggle("respuesta1.1", anim = TRUE)
    })
    observeEvent(input$pregunta1.2, {
        shinyjs::toggle("respuesta1.2", anim = TRUE)
    })
    observeEvent(input$pregunta1.3, {
        shinyjs::toggle("respuesta1.3", anim = TRUE)
    })
    observeEvent(input$pregunta2.1, {
        shinyjs::toggle("respuesta2.1", anim = TRUE)
    })
    observeEvent(input$pregunta2.2, {
        shinyjs::toggle("respuesta2.2", anim = TRUE)
    })
    observeEvent(input$pregunta2.3, {
        shinyjs::toggle("respuesta2.3", anim = TRUE)
    })
    
    # Determine which 'select' options to display (Input choices)
    output$btns <- renderUI({
        if (values$data == 0) {
            return()
        } else if (values$data == 1) {
            uiOutput("select1")
        } else if (values$data == 2) {
            dataTableOutput("select2")
        } else if (values$data == 3) {
            dataTableOutput("select3")
        } else if (values$data == 4) {
            dataTableOutput("select4")
        } else if (values$data >= 5) {
            dataTableOutput("select5")
        } 
    })
    
    
    # Search NeoGov ------------------------------------------------------------
    link <- eventReactive(input$searchTerm, {
        
        addr <- getLink(input$searchTerm)  # create a hyperlink based on the text input
        
        paste0( "window.open('", addr, "', '_blank')" )  # this code opens the link in a separate window
    })
    
    output$searchNeo <- renderUI({
        actionButton("srchNeo", 
                     label = "Search", 
                     onclick = link(),
                     icon = icon("external-link"))  # when clicked, the link() code executes
    })
    
    # Start Button -------------------------------------------------------------
    observeEvent(input$startBtn, {
        updateNavbarPage(session, "navBar",
                         selected = "careerPF"
        )
    })
    
    
    # Table Inputs (Next 2-5 Selections) ---------------------------------------
    
    # Table 1 (Step 2)
    top1 <- reactive({
        
        top <- dplyr::filter(item_pairs(), Item1Name == input$item_name) %>%
            select(Item2Name, Item2, Prob, Salary2Min, SalaryDiff, Incumbents, Hyperlink)
    })
    
    output$select2 <- DT::renderDataTable({
        datatable( top1(), escape = FALSE,
                   extensions = 'Responsive',
                   selection = list(mode = 'single', target = 'row'),
                   colnames = c("Title", "Job Code", "Popularity %", "Starting Salary", "Max Salary Difference", "Incumbents", "Job Description"),
                   rownames = FALSE, style = "bootstrap", 
                   callback = JS("
                                 var tips = ['Classification Title', 'Title Code', 'Percent of employees that moved into that job from your last selected job',
                                 'Starting salary', 'Difference between the highest possible salaries for the selected jobs',
                                 'Number of employees currently holding the title', 'Link to requirements and description'],
                                 header = table.columns().header();
                                 for (var i = 0; i < tips.length; i++) {
                                 $(header[i]).attr('title', tips[i]);
                                 }
                                 ")
        ) %>%
            formatCurrency('SalaryDiff') %>% 
            formatPercentage('Prob', 1) %>%
            formatCurrency("Salary2Min")
    })
    
    
    proxy1 = dataTableProxy('select2')
    
    top2 <- reactive({
        
        itemName <- top1()[ input$select2_rows_selected,  "Item2Name"]
        
        top <- dplyr::filter(item_pairs(), Item1Name == itemName) %>%
            select(Item2Name, Item2, Prob, Salary2Min, SalaryDiff, Incumbents, Hyperlink)
        top
    })
    
    output$select3 <- DT::renderDataTable({
        datatable( top2(), escape = FALSE, 
                   extensions = 'Responsive',
                   selection = list(mode = 'single', target = 'row'),
                   colnames = c("Title", "Job Code", "Popularity %", "Starting Salary", "Max Salary Difference", "Incumbents", "Job Description"),
                   rownames = FALSE, style = "bootstrap", 
                   callback = JS("
                                 var tips = ['Classification Title', 'Title Code', 'Percent of employees that moved into that job from your last selected job',
                                 'Starting salary', 'Difference between the highest possible salaries for the selected jobs',
                                 'Number of employees currently holding the title', 'Link to requirements and description'],
                                 header = table.columns().header();
                                 for (var i = 0; i < tips.length; i++) {
                                 $(header[i]).attr('title', tips[i]);
                                 }
                                 ")
        ) %>%
            formatCurrency('SalaryDiff') %>% 
            formatPercentage('Prob', 1) %>%
            formatCurrency("Salary2Min")
    })
    
    proxy2 = dataTableProxy('select3')
    
    # Table 3 (Step 4)
    top3 <- reactive({
        
        itemName <- top2()[ input$select3_rows_selected,  "Item2Name"]
        
        top <- dplyr::filter(item_pairs(), Item1Name == itemName) %>%
            select(Item2Name, Item2, Prob, Salary2Min, SalaryDiff, Incumbents, Hyperlink)
        top
    })
    
    output$select4 <- DT::renderDataTable({
        datatable( top3(), escape = FALSE, 
                   extensions = 'Responsive',
                   selection = list(mode = 'single', target = 'row'),
                   colnames = c("Title", "Job Code", "Popularity %", "Starting Salary", "Max Salary Difference", "Incumbents", "Job Description"),
                   rownames = FALSE, style = "bootstrap", 
                   callback = JS("
                                 var tips = ['Classification Title', 'Title Code', 'Percent of employees that moved into that job from your last selected job',
                                 'Starting salary', 'Difference between the highest possible salaries for the selected jobs',
                                 'Number of employees currently holding the title', 'Link to requirements and description'],
                                 header = table.columns().header();
                                 for (var i = 0; i < tips.length; i++) {
                                 $(header[i]).attr('title', tips[i]);
                                 }
                                 ")
        ) %>%
            formatCurrency('SalaryDiff') %>% 
            formatPercentage('Prob', 1) %>%
            formatCurrency("Salary2Min")
    })
    
    proxy3 = dataTableProxy('select4')
    
    top4 <- reactive({
        
        itemName <- top3()[ input$select4_rows_selected,  "Item2Name"]
        
        top <- dplyr::filter(item_pairs(), Item1Name == itemName) %>%
            select(Item2Name, Item2, Prob, Salary2Min, SalaryDiff, Incumbents, Hyperlink)
        top
    })
    
    output$select5 <- DT::renderDataTable({
        datatable( top4(), escape = FALSE, 
                   extensions = 'Responsive',
                   selection = list(mode = 'single', target = 'row'),
                   colnames = c("Title", "Job Code", "Popularity %", "Starting Salary", "Max Salary Difference", "Incumbents", "Job Description"),
                   rownames = FALSE, style = "bootstrap", 
                   callback = JS("
                                 var tips = ['Classification Title', 'Title Code', 'Percent of employees that moved into that job from your last selected job',
                                 'Starting salary', 'Difference between the highest possible salaries for the selected jobs',
                                 'Number of employees currently holding the title', 'Link to requirements and description'],
                                 header = table.columns().header();
                                 for (var i = 0; i < tips.length; i++) {
                                 $(header[i]).attr('title', tips[i]);
                                 }
                                 ")
        ) %>%
            formatCurrency('SalaryDiff') %>% 
            formatPercentage('Prob', 1) %>%
            formatCurrency("Salary2Min")
    })
    
    
    proxy4 = dataTableProxy('select5')
    
    # User name ----------------------------------------------------------------
    plotTitle <- reactive({
        
        if(input$userName == "") {
            paste("¿Cuál es tu plan?")
        } else {
            paste(input$userName, sep = "")
        }
        
    })
    
    
    output$displayName <- renderUI({
        tags$h4( plotTitle() )
        
    })
    
    
    # Get selection data for printing, etc. -----------------------------------
    
    job_1_data <- reactive({
        # Obtain stats
        itemNo <- item_ref[ item_ref$TitleLong == input$item_name, "TitleCode"]
        salaryMin <- item_ref[ item_ref$TitleLong == input$item_name, "SalaryMin"]
        salaryMax <- item_ref[ item_ref$TitleLong == input$item_name, "SalaryMax"]
        incumb <- item_ref[ item_ref$TitleLong == input$item_name, "Incumbents"]
        
        salaryMax <- format(salaryMax, big.mark = ",")
        salaryMax <- paste0("$", salaryMax)
        
        salaryMin <- format(salaryMin, big.mark = ",")
        salaryMin <- paste0("$", salaryMin)
        
        v <- c(input$item_name, itemNo, salaryMin, salaryMax, incumb)
        
        v
        
    })
    
    # Print each selection to a panel in sidebar
    output$printInput1 <- renderUI({
        # Display if item is selected
        if(input$item_name == ""){
            return()
        } else {
            div(class="panel panel-default",
                div(class="panel-body",
                    div(tags$img(src = "one.png", width = "25px", height = "25px"), tags$h6( paste0(input$item_name, " (", job_1_data()[2], ")") ),
                        paste0( job_1_data()[3], " - ", job_1_data()[4], " /month"), 
                        div(paste0(job_1_data()[5], " incumbents"))
                    )
                ))
        }
    })
    
    # Create label for output report
    label_1 <- reactive({
        
        lab <- paste0( input$item_name, "\n",
                       job_1_data()[3], " - ", job_1_data()[4], " Monthly",
                       " | ", job_1_data()[5], " Incumbents")
        
        lab
    })
    
    job_2_data <- reactive({
        # Obtain stats
        itemName <- top1()[ input$select2_rows_selected,  "Item2Name"]
        itemNo <- top1()[ input$select2_rows_selected,  "Item2"]
        salaryMin <- top1()[ input$select2_rows_selected,  "Salary2Min"] 
        salaryMax <- item_ref[ which( itemName == item_ref$TitleLong ), "SalaryMax" ]
        incumb <- top1()[ input$select2_rows_selected,  "Incumbents"]
        prob <- top1()[ input$select2_rows_selected,  "Prob"]
        
        salaryMax <- format(salaryMax, big.mark = ",")
        salaryMax <- paste0("$", salaryMax)
        
        salaryMin <- format(salaryMin, big.mark = ",")
        salaryMin <- paste0("$", salaryMin)
        
        prob <- paste0( round( prob*100, 1 ), "%" )
        
        v <- c(itemName, itemNo, salaryMin, salaryMax, incumb, prob)
        
        v
    })
    
    output$printInput2 <- renderUI({
        
        # Display if item is selected
        if( is.null(input$select2_rows_selected) ){
            return()
        } else {
            div(class="panel panel-default",
                div(class="panel-body",
                    div(tags$img(src = "two.png", width = "25px", height = "25px"), tags$h6( paste0(job_2_data()[1], " (", job_2_data()[2], ")") ),
                        paste0( job_2_data()[3], " - ", job_2_data()[4], " /month"), 
                        div(paste0(job_2_data()[5], " incumbents"))
                    )
                ))
        }
    })
    
    label_2 <- reactive({
        
        try(
            paste0( job_2_data()[1], "\n",
                    job_2_data()[3], " - ", job_2_data()[4], " Monthly", "\n",
                    job_2_data()[6], " Popularity", " | ", job_2_data()[5], " Incumbents"),
            
            TRUE
        )
        
    })
    
    job_3_data <- reactive({
        # Obtain stats
        itemName <- top2()[ input$select3_rows_selected,  "Item2Name"]
        itemNo <- top2()[ input$select3_rows_selected,  "Item2"]
        salaryMin <- top2()[ input$select3_rows_selected,  "Salary2Min"] 
        salaryMax <- item_ref[ which( itemName == item_ref$TitleLong ), "SalaryMax" ]
        incumb <- top2()[ input$select3_rows_selected,  "Incumbents"]
        prob <- top2()[ input$select3_rows_selected,  "Prob"]
        
        salaryMax <- format(salaryMax, big.mark = ",")
        salaryMax <- paste0("$", salaryMax)
        
        salaryMin <- format(salaryMin, big.mark = ",")
        salaryMin <- paste0("$", salaryMin)
        
        prob <- paste0( round( prob*100, 1 ), "%" )
        
        v <- c(itemName, itemNo, salaryMin, salaryMax, incumb, prob)
        
        v
    })
    
    output$printInput3 <- renderUI({
        
        # Display if item is selected
        if( is.null(input$select3_rows_selected) ){
            return()
        } else {
            div(class="panel panel-default",
                div(class="panel-body",
                    div(tags$img(src = "three.png", width = "25px", height = "25px"), tags$h6( paste0(job_3_data()[1], " (", job_3_data()[2], ")") ),
                        paste0( job_3_data()[3], " - ", job_3_data()[4], " /month"), 
                        div(paste0(job_3_data()[5], " incumbents"))
                    )
                ))
        }
    })
    
    label_3 <- reactive({
        
        try(
            paste0( job_3_data()[1], "\n",
                    job_3_data()[3], " - ", job_3_data()[4], " Monthly", "\n",
                    job_3_data()[6], " Popularity", " | ", job_3_data()[5], " Incumbents"),
            TRUE
        )
        
    })
    
    job_4_data <- reactive({
        # Obtain stats
        itemName <- top3()[ input$select4_rows_selected,  "Item2Name"]
        itemNo <- top3()[ input$select4_rows_selected,  "Item2"]
        salaryMin <- top3()[ input$select4_rows_selected,  "Salary2Min"] 
        salaryMax <- item_ref[ which( itemName == item_ref$TitleLong ), "SalaryMax" ]
        incumb <- top3()[ input$select4_rows_selected,  "Incumbents"]
        prob <- top3()[ input$select4_rows_selected,  "Prob"]
        
        salaryMax <- format(salaryMax, big.mark = ",")
        salaryMax <- paste0("$", salaryMax)
        
        salaryMin <- format(salaryMin, big.mark = ",")
        salaryMin <- paste0("$", salaryMin)
        
        prob <- paste0( round( prob*100, 1 ), "%" )
        
        v <- c(itemName, itemNo, salaryMin, salaryMax, incumb, prob)
        
        v
    })
    
    
    tip1 <- reactive({
        paste0( "<h6>", job_1_data()[1], "</h6>")
        
    })
    
    visNode <- reactive({
        
        item_name1 <- input$item_name  
        item_name2 <- try( top1()[ input$select2_rows_selected,  "Item2Name"], TRUE ) 
        item_name3 <- try( top2()[ input$select3_rows_selected,  "Item2Name"], TRUE ) 
        item_name4 <- try( top3()[ input$select4_rows_selected,  "Item2Name"], TRUE ) 
        item_name5 <- try( top4()[ input$select5_rows_selected,  "Item2Name"], TRUE ) 
        
        # Collect user selections
        selections <- append(selections,
                             c(item_name1, item_name2, item_name3,
                               item_name4, item_name5))
        
        
        # Insert line breaks where there's more than 2 words in a title
        selections <- sapply(selections, make_breaks, simplify = "array", USE.NAMES = FALSE)
        
        # Add selections to data.frame
        nodes[1:length(selections),2] <- selections
        
        
        # Add id
        nodes$id <- 1:length(selections)
        
        # Add icons, which requires defining 3 properties
        nodes$shape <- rep("icon", length(selections))
        nodes$icon.face <- rep('fontAwesome', length(selections))
        nodes$icon.code <- rep(avatar(), length(selections))
        
        
        # Keep only the rows that don't have errors
        nodes <- nodes[grep("Error", nodes$label, invert = TRUE),]
        
        # Keep rows that are not NA in Label column
        nodes <- nodes[ !is.na(nodes$label), ]  
        
    })
    
    
    # Under Development - Adding popularity percentage to edge label 
    edgeLab <- reactive({
        prob1 <- try( top1()[ input$select2_rows_selected,  "Prob"], TRUE ) 
        prob2 <- try( top2()[ input$select3_rows_selected,  "Prob"], TRUE ) 
        prob3 <- try( top3()[ input$select4_rows_selected,  "Prob"], TRUE ) 
        prob4 <- try( top4()[ input$select5_rows_selected,  "Prob"], TRUE ) 
        
        # Collect user selections
        edgeLabels <- c(prob1, prob2, prob3, prob4)
        
        # Keep only the rows that don't have errors
        edgeLabels <- edgeLabels[grep("Error", edgeLabels, invert = TRUE)]
    })
    
    # Set the seed (layout) for the graph based on number of nodes in graph
    visSeed <- reactive({
        if( nrow(visNode()) == 1 ) {
            1
        } else if ( nrow(visNode()) == 2 ) {
            6
        } else if ( nrow(visNode()) == 3 ) {
            21
        } else if ( nrow(visNode()) == 4 ) {
            30
        } else if ( nrow(visNode()) == 5 ) {
            5432
        }
    })
    
    # Creating the dynamic graph
    output$visTest <- visNetwork::renderVisNetwork({
        
        # The below uses a different random seed to determine layout based on num of nodes
        
        visNetwork::visNetwork(visNode(), visEdge(), height = "275px", width = "100%") %>%
            addFontAwesome() %>%
            visNetwork::visEdges(dashes = TRUE, shadow = TRUE,
                                 arrows = list(to = list(enabled = TRUE, scaleFactor = 2)),
                                 color = list(color = "#587fb4", highlight = "red")) %>%
            visNodes(shadow = list(enabled = TRUE, size = 15),
                     icon = list( color = colorIcon() )) %>%
            visLayout(randomSeed = visSeed() ) %>%
            visPhysics(solver = "barnesHut", stabilization = list(enabled = FALSE))
    })
    
    # Output Report -----------------------------------------------------------
    # Report template is chosen based on the number of job selections made
    template <- reactive({
        if( nrow(visNode()) == 1 ) {
            ""
        } else if ( nrow(visNode()) == 2 ) {
            png::readPNG("./www/pathImage_2.png")
        } else if ( nrow(visNode()) == 3 ) {
            png::readPNG("./www/pathImage_3.png")
        } else if ( nrow(visNode()) == 4 ) {
            png::readPNG("./www/pathImage_4.png")
        } else if ( nrow(visNode()) == 5 ) {
            png::readPNG("./www/pathImage_5.png")
        }
    })
    
    # Determine how many plot labels are needed for the output report
    plot_labels <- reactive({
        if( nrow(visNode()) == 1 ) {
            # Display job title 1
            text(25, 105, labels = label_1(), col = "white", pos = 2, cex = 0.75)
            
        } else if ( nrow(visNode()) == 2 ) {
            # Display job title 1
            text(25, 105, labels = label_1(), col = "white", pos = 2, cex = 0.75)
            
            # Display job title 2
            text(5.5, 42, labels = label_2(), col = "white", pos = 4, cex = 0.75)
        } else if ( nrow(visNode()) == 3 ) {
            # Display job title 1
            text(25, 105, labels = label_1(), col = "white", pos = 2, cex = 0.75)
            
            # Display job title 2
            text(5.5, 42, labels = label_2(), col = "white", pos = 4, cex = 0.75)
            
            # Display job title 3
            text(5.5, 33, labels = label_3(), col = "white", pos = 4, cex = 0.75)
        } else if ( nrow(visNode()) == 4 ) {
            # Display job title 1
            text(25, 105, labels = label_1(), col = "white", pos = 2, cex = 0.75)
            
            # Display job title 2
            text(5.5, 42, labels = label_2(), col = "white", pos = 4, cex = 0.75)
            
            # Display job title 3
            text(5.5, 33, labels = label_3(), col = "white", pos = 4, cex = 0.75)
            
            # Display job title 4
            text(5.5, 24.5, labels = label_4(), col = "white", pos = 4, cex = 0.75)
        } else if ( nrow(visNode()) == 5 ) {
            # Display job title 1
            text(25, 105, labels = label_1(), col = "white", pos = 2, cex = 0.75)
            
            # Display job title 2
            text(5.5, 42, labels = label_2(), col = "white", pos = 4, cex = 0.75)
            
            # Display job title 3
            text(5.5, 33, labels = label_3(), col = "white", pos = 4, cex = 0.75)
            
            # Display job title 4
            text(5.5, 24.5, labels = label_4(), col = "white", pos = 4, cex = 0.75)
            
            # Display job title 5
            text(5.5, 15, labels = label_5(), col = "white", pos = 4, cex = 0.75)
            
        }
    })
    
    
    
    # This plots the blank 'decoy' plot
    # Progress bar is based on the computation time of this plot
    output$myplot <- renderPlot({ 
        withProgress(message = 'Building your report', {
            
            plotInput()
            
            incProgress(detail = "Putting on the finishing touches...", amount = 0.5)
            
        })
    })
    
    output$myplot2 <- renderPlot({ 
        withProgress(message = 'Building your report', {
            
            plotInput()
            
            incProgress(detail = "Putting on the finishing touches...", amount = 0.5)
            
        })
    })
    
    output$myplot3 <- renderPlot({ 
        withProgress(message = 'Building your report', {
            
            plotInput()
            
            incProgress(detail = "Putting on the finishing touches...", amount = 0.5)
            
        })
    })
    output$pdflink <- downloadHandler(
        filename <- "my-career.pdf",
        content <- function(file) {
            # old file that was copied was my-career-path.pdf
            file.copy("./www/Career_Path_Report.pdf", file)
        })
    
    # Generate the download link for UI only if the checkbox is checked
    output$download <- renderUI({
        if(input$returnpdf == FALSE){
            return()
        } else{
            downloadLink('pdflink')
        }
    })
    
    # Delay showing the download link to buy time to generate the PDF
    observeEvent(input$returnpdf,{
        
        delay(3000, shinyjs::show("download") )
        
    })
    
    # Survey link after the download button is clicked
    # AND reset the checkbox to the download button
    
    onclick("download",
            delay(5000, 
                  c(
                      showModal(
                          modalDialog(
                              title = "Tell us what you think!",
                              "We want to hear your thoughts on this career planning tool. Please take this 2 minute survey and your feedback will enhance this resource for years to come!", 
                              tags$a(href="https://survey.lacounty.gov/LACounty/se.ashx?s=645F266E52CB09F0", 
                                     target = "_blank", 
                                     "Click here for the survey"), 
                              size = "m", 
                              footer = modalButton(label = " ", icon = icon("close") )
                          )
                      ),
                      delay(1000, shinyjs::reset("returnpdf") )
                  )
            )
    )
    
    shiny::observeEvent(input$dismiss_modal,
                        {
                            shiny::removeModal()
                        })
    
    shiny::observeEvent(input$final_edit, {
        shiny::removeModal()
        
    })
    
    
    responses1 <- interviewer::buildResponses(
        id = c("a", "b", "c"),
        label = c("Culturales o que aporten conocimiento", 
                  "En las que estás en contacto con la naturaleza", 
                  "Cualquier cosa alocada y divertida")
    )
    
    responses2 <- interviewer::buildResponses(
        id = c("a", "b", "c"),
        label = c("Solo", 
                  "En pareja", 
                  "Con mi grupo de amigos")
    )
    
    responses3 <- interviewer::buildResponses(
        id = c("a", "b", "c"),
        label = c("Suelo hacer actividades gratuitas", 
                  "Como máximo, suelo gastar alededor de 10€", 
                  "No me importa cuánto gastarme")
    )
    
    responses4 <- interviewer::buildResponses(
        id = c("a", "b", "c"),
        label = c("Prefiero actividades moviditas", 
                  "Quiero algo que me de que pensar pero sin cansarme físicamente",
                  "No quiero esfuerzo físico ni mental, solo evadirme y pasarlo bien.")
    )
    
    
    output$Personalizado <-
        interviewer::questionnaire(
            label = actionLink("chatbot", shiny::HTML("<h7>Chatbot</h7>"), width = 100,icon = icon("robot", class = "fa-2x")),
            
            welcome = list(
                shiny::HTML("<h4>¡Bienvenido!</h4>"),
                shiny::HTML("<h5>Aquí te ayudaremos a elegir las mejores actividades para ti según tus preferencias a partir de un cuestionario.</h5>"),
                # Espacio
                shiny::HTML("<h5>Para comenzar, pulsa 'START'</h5>")
            ),
            
            interviewer::question.single(
                id = "LoopSource1",
                label = "Te suelen interesar las actividades...",
                responses = responses1
            ),
            
            interviewer::pageBreak(),
            
            interviewer::question.single(
                id = "LoopSource2",
                label = "Cuando realizas actividades de ocio, sueles hacelo...",
                responses = responses2
            ),
            
            interviewer::pageBreak(),
            
            interviewer::question.single(
                id = "LoopSource3",
                label = "En cuanto al dinero que gastas en las actividades de ocio que realizas...",
                responses = responses3
            ),
            
            interviewer::pageBreak(),
            
            interviewer::question.single(
                id = "LoopSource4",
                label = "En cuanto al tipo de actividades que te gustan...",
                responses = responses4
            ),
            
            exit = function(data) {
                cat("Done:\n")
                
                
                if ((data$LoopSource1 == 'a' & data$LoopSource3 == 'a' & data$LoopSource4 == 'a') |
                    (data$LoopSource1 == 'a' & data$LoopSource3 == 'a' & data$LoopSource4 == 'b') |
                    (data$LoopSource1 == 'a' & data$LoopSource3 == 'a' & data$LoopSource4 == 'c') ) 
                {
                    output$tabla_chatbot <- DT::renderDT(
                        {
                            # Museo militar, Museo BA
                            shiny::isolate( rbind(BuscaMatch(df, "Museo Histórico Militar de Valencia", FALSE),
                                                  BuscaMatch(df, "Museo de Bellas Artes de Valencia", FALSE))%>% 
                                                arrange(desc(VALORACION), PRECIO)) 
                        },
                        escape = F,
                        rownames = FALSE,
                        options = list(processing = FALSE) 
                    )}
                
                
                
                else if ((data$LoopSource1 == 'a' & data$LoopSource3 == 'b' & data$LoopSource4 == 'a') | 
                         (data$LoopSource1 == 'a' & data$LoopSource3 == 'b' & data$LoopSource4 == 'b') |
                         (data$LoopSource1 == 'a' & data$LoopSource3 == 'b' & data$LoopSource4 == 'c')) 
                {
                    
                    
                    output$tabla_chatbot <- DT::renderDT(
                        {
                            # Museo militar, Museo Fallero, museo ceramica, catedral, museo BA
                            shiny::isolate( rbind(BuscaMatch(df, "Museo Histórico Militar de Valencia", FALSE),
                                                  BuscaMatch(df, "Museo de Bellas Artes de Valencia", FALSE),
                                                  BuscaMatch(df, "Museo Fallero", FALSE),
                                                  BuscaMatch(df, "Museo Nacional de Cerámica y de las Artes Suntuarias González Martí", FALSE),
                                                  BuscaMatch(df, "Catedral de Valencia", FALSE))%>% 
                                                arrange(desc(VALORACION), PRECIO)) 
                        },
                        escape = F,
                        rownames = FALSE,
                        options = list(processing = FALSE) 
                    ) 
                }
                else if ((data$LoopSource1 == 'a' & data$LoopSource3 == 'c' & data$LoopSource4 == 'a')) 
                {
                    output$tabla_chatbot <- DT::renderDT(
                        {
                            
                            # Museo militar, Museo Fallero, museo ceramica, museo BA, palacio, recorrido
                            shiny::isolate( rbind(BuscaMatch(df, "Museo Histórico Militar de Valencia", FALSE),
                                                  BuscaMatch(df, "Museo de Bellas Artes de Valencia", FALSE),
                                                  BuscaMatch(df, "Museo Fallero", FALSE),
                                                  BuscaMatch(df, "Museo Nacional de Cerámica y de las Artes Suntuarias González Martí", FALSE),
                                                  BuscaMatch(df, "Palacio de las Artes Reina Sofía", FALSE),
                                                  BuscaMatch(df, "Recorrido privado a pie por el casco antiguo de Valencia", FALSE)) %>% 
                                                arrange(desc(VALORACION), PRECIO)) 
                        },
                        escape = F,
                        rownames = FALSE,
                        options = list(processing = FALSE) 
                    ) 
                }
                else if ((data$LoopSource1 == 'a' & data$LoopSource3 == 'c' & data$LoopSource4 == 'b') | 
                         (data$LoopSource1 == 'a' & data$LoopSource3 == 'c' & data$LoopSource4 == 'c')) 
                {
                    output$tabla_chatbot <- DT::renderDT(
                        {
                            # Museo militar, Museo Fallero, museo ceramica, catedral, museo BA, palacio
                            shiny::isolate( rbind(BuscaMatch(df, "Museo Histórico Militar de Valencia", FALSE),
                                                  BuscaMatch(df, "Museo de Bellas Artes de Valencia", FALSE),
                                                  BuscaMatch(df, "Museo Fallero", FALSE),
                                                  BuscaMatch(df, "Museo Nacional de Cerámica y de las Artes Suntuarias González Martí", FALSE),
                                                  BuscaMatch(df, "Palacio de las Artes Reina Sofía", FALSE),
                                                  BuscaMatch(df, "Catedral de Valencia", FALSE)) %>% 
                                                arrange(desc(VALORACION), PRECIO)) 
                        },
                        escape = F,
                        rownames = FALSE,
                        options = list(processing = FALSE) 
                    ) 
                }
                else if ((data$LoopSource1 == 'b' & data$LoopSource3 == 'a' & data$LoopSource4 == 'a') | 
                         (data$LoopSource1 == 'b' & data$LoopSource3 == 'a' & data$LoopSource4 == 'b') |
                         (data$LoopSource1 == 'b' & data$LoopSource3 == 'a' & data$LoopSource4 == 'c') |
                         (data$LoopSource1 == 'b' & data$LoopSource3 == 'b' & data$LoopSource4 == 'a') |
                         (data$LoopSource1 == 'b' & data$LoopSource3 == 'b' & data$LoopSource4 == 'b') |
                         (data$LoopSource1 == 'b' & data$LoopSource3 == 'b' & data$LoopSource4 == 'c') ) 
                {
                    output$tabla_chatbot <- DT::renderDT(
                        {
                            
                            # Parque, embalse, jardines
                            shiny::isolate( rbind(BuscaMatch(df, "Parque Gulliver", FALSE),
                                                  BuscaMatch(df, "Parque Reina Sofia", FALSE),
                                                  BuscaMatch(df, "Parque Natural de la Murta", FALSE),
                                                  BuscaMatch(df, "Embalse de Tous", FALSE),
                                                  BuscaMatch(df, "Parque de Cabacera", FALSE),
                                                  BuscaMatch(df, "Jardines del Real", FALSE)) %>% 
                                                arrange(desc(VALORACION), PRECIO)) 
                        },
                        escape = F,
                        rownames = FALSE,
                        options = list(processing = FALSE) 
                    ) 
                }
                
                else if ((data$LoopSource1 == 'b' & data$LoopSource3 == 'c' & data$LoopSource4 == 'a')  ) 
                {
                    output$tabla_chatbot <- DT::renderDT(
                        {
                            # Parque, embalse, jardines, bioparc, oceanografico, albufera
                            shiny::isolate( rbind(BuscaMatch(df, "Parque Gulliver", FALSE),
                                                  BuscaMatch(df, "Parque Reina Sofia", FALSE),
                                                  BuscaMatch(df, "Parque Natural de la Murta", FALSE),
                                                  BuscaMatch(df, "Embalse de Tous", FALSE),
                                                  BuscaMatch(df, "Parque de Cabacera", FALSE),
                                                  BuscaMatch(df, "Jardines del Real", FALSE),
                                                  BuscaMatch(df, "Bioparc Valencia", FALSE),
                                                  BuscaMatch(df, "Oceanogràfic", FALSE),
                                                  BuscaMatch(df, "Excursión a La Albufera", FALSE)) %>% 
                                                arrange(desc(VALORACION), PRECIO))
                        },
                        escape = F,
                        rownames = FALSE,
                        options = list(processing = FALSE) 
                    ) 
                }
                
                else if ((data$LoopSource1 == 'b' & data$LoopSource3 == 'c' & data$LoopSource4 == 'b') |
                         (data$LoopSource1 == 'b' & data$LoopSource3 == 'c' & data$LoopSource4 == 'c') ) 
                {
                    output$tabla_chatbot <- DT::renderDT(
                        {
                            
                            # Parque, embalse, jardines, albufera
                            shiny::isolate( rbind(BuscaMatch(df, "Parque Gulliver", FALSE),
                                                  BuscaMatch(df, "Parque Reina Sofia", FALSE),
                                                  BuscaMatch(df, "Parque Natural de la Murta", FALSE),
                                                  BuscaMatch(df, "Embalse de Tous", FALSE),
                                                  BuscaMatch(df, "Parque de Cabacera", FALSE),
                                                  BuscaMatch(df, "Jardines del Real", FALSE),
                                                  BuscaMatch(df, "Excursión a La Albufera", FALSE)) %>% 
                                                arrange(desc(VALORACION), PRECIO))
                        },
                        escape = F,
                        rownames = FALSE,
                        options = list(processing = FALSE) 
                    ) 
                }
                else if ((data$LoopSource1 == 'c' & data$LoopSource3 == 'a' & data$LoopSource4 == 'a') ) 
                {
                    output$tabla_chatbot <- DT::renderDT(
                        {
                            
                            # Parque, embalse, jardines
                            shiny::isolate( rbind(BuscaMatch(df, "Parque Gulliver", FALSE),
                                                  BuscaMatch(df, "Parque Reina Sofia", FALSE),
                                                  BuscaMatch(df, "Parque Natural de la Murta", FALSE),
                                                  BuscaMatch(df, "Embalse de Tous", FALSE),
                                                  BuscaMatch(df, "Parque de Cabacera", FALSE),
                                                  BuscaMatch(df, "Jardines del Real", FALSE)) %>% 
                                                arrange(desc(VALORACION), PRECIO))
                        },
                        escape = F,
                        rownames = FALSE,
                        options = list(processing = FALSE) 
                    ) 
                }
                else if ((data$LoopSource1 == 'c' & data$LoopSource3 == 'a' & data$LoopSource4 == 'b') |
                         (data$LoopSource1 == 'c' & data$LoopSource3 == 'a' & data$LoopSource4 == 'c') |
                         (data$LoopSource1 == 'c' & data$LoopSource3 == 'b' & data$LoopSource4 == 'a')) 
                {
                    output$tabla_chatbot <- DT::renderDT(
                        {
                            
                            # Parque, embalse, jardines, museo BA
                            shiny::isolate( rbind(BuscaMatch(df, "Parque Gulliver", FALSE),
                                                  BuscaMatch(df, "Parque Reina Sofia", FALSE),
                                                  BuscaMatch(df, "Parque Natural de la Murta", FALSE),
                                                  BuscaMatch(df, "Embalse de Tous", FALSE),
                                                  BuscaMatch(df, "Parque de Cabacera", FALSE),
                                                  BuscaMatch(df, "Jardines del Real", FALSE),
                                                  BuscaMatch(df, "Museo de Bellas Artes de Valencia", FALSE)) %>% 
                                                arrange(desc(VALORACION), PRECIO))
                        },
                        escape = F,
                        rownames = FALSE,
                        options = list(processing = FALSE) 
                    ) 
                }
                else if ((data$LoopSource1 == 'c' & data$LoopSource3 == 'b' & data$LoopSource4 == 'b') |
                         (data$LoopSource1 == 'c' & data$LoopSource3 == 'b' & data$LoopSource4 == 'c')) 
                {
                    output$tabla_chatbot <- DT::renderDT(
                        {
                            # Parque, embalse, jardines, museo BA, Genious
                            shiny::isolate( rbind(BuscaMatch(df, "Parque Gulliver", FALSE),
                                                  BuscaMatch(df, "Parque Reina Sofia", FALSE),
                                                  BuscaMatch(df, "Parque Natural de la Murta", FALSE),
                                                  BuscaMatch(df, "Embalse de Tous", FALSE),
                                                  BuscaMatch(df, "Parque de Cabacera", FALSE),
                                                  BuscaMatch(df, "Jardines del Real", FALSE),
                                                  BuscaMatch(df, "Museo de Bellas Artes de Valencia", FALSE),
                                                  BuscaMatch(df, "Genius Escape Room", FALSE)) %>% 
                                                arrange(desc(VALORACION), PRECIO))
                        },
                        escape = F,
                        rownames = FALSE,
                        options = list(processing = FALSE) 
                    ) 
                }
                
                else if ((data$LoopSource1 == 'c' & data$LoopSource3 == 'c' & data$LoopSource4 == 'a')) 
                {
                    output$tabla_chatbot <- DT::renderDT(
                        {
                            
                            
                            # Parque, embalse, jardines, recorrido, bioparc, palacio, oceanografic
                            shiny::isolate( rbind(BuscaMatch(df, "Parque Gulliver", FALSE),
                                                  BuscaMatch(df, "Parque Reina Sofia", FALSE),
                                                  BuscaMatch(df, "Parque Natural de la Murta", FALSE),
                                                  BuscaMatch(df, "Embalse de Tous", FALSE),
                                                  BuscaMatch(df, "Parque de Cabacera", FALSE),
                                                  BuscaMatch(df, "Jardines del Real", FALSE),
                                                  BuscaMatch(df, "Recorrido privado a pie por el casco antiguo de Valencia", FALSE),
                                                  BuscaMatch(df, "Bioparc Valencia", FALSE),
                                                  BuscaMatch(df, "Palacio de las Artes Reina Sofía", FALSE),
                                                  BuscaMatch(df, "Oceanogràfic", FALSE)) %>% 
                                                arrange(desc(VALORACION), PRECIO))
                        },
                        escape = F,
                        rownames = FALSE,
                        options = list(processing = FALSE) 
                    ) 
                }
                
                else if ((data$LoopSource1 == 'c' & data$LoopSource3 == 'c' & data$LoopSource4 == 'b') |
                         (data$LoopSource1 == 'c' & data$LoopSource3 == 'c' & data$LoopSource4 == 'c')) 
                {
                    output$tabla_chatbot <- DT::renderDT(
                        {
                            
                            # Parque, embalse, jardines, palacio, albufera, museo BA, Escape
                            shiny::isolate( rbind(BuscaMatch(df, "Parque Gulliver", FALSE),
                                                  BuscaMatch(df, "Parque Reina Sofia", FALSE),
                                                  BuscaMatch(df, "Parque Natural de la Murta", FALSE),
                                                  BuscaMatch(df, "Embalse de Tous", FALSE),
                                                  BuscaMatch(df, "Parque de Cabacera", FALSE),
                                                  BuscaMatch(df, "Jardines del Real", FALSE),
                                                  BuscaMatch(df, "Palacio de las Artes Reina Sofía", FALSE),
                                                  BuscaMatch(df, "Excursión a La Albufera", FALSE),
                                                  BuscaMatch(df, "Museo de Bellas Artes de Valencia", FALSE),
                                                  BuscaMatch(df, "Genius Escape Room", FALSE),
                                                  BuscaMatch(df, "Coco Room Valencia Room Escape", FALSE)) %>% 
                                                arrange(desc(VALORACION), PRECIO))
                        },
                        escape = F,
                        rownames = FALSE,
                        options = list(processing = FALSE) 
                    ) 
                }
                
            },
            
            goodbye = "¡Estas son las mejores actividades para ti!"
            
            
        ) # cerramos cuestionario
})

