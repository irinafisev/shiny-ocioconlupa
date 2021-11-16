
# Intalación de librerías 

# install.packages("dplyr")
# install.packages("stringr")
# install.packages("png")
# install.packages("shinyjs")
# install.packages("DT")
# install.packages("visNetwork")
# install.packages("rintrojs")
# install.packages("readxl")
# install.packages("shinydashboard")
# install.packages("tidyverse")
# install.packages("scales")
# install.packages("dplyr")
# devtools::install_github('mtrybulec/interviewer')
# install.packages("bsModal")
# install.packages("installr")
# library(installr)
# updateR()
# install.packages("shinyBS")

# Librerías
library(dplyr)
library(stringr)
library(png)
library(shinyjs)
library(DT)
library(visNetwork)
library(rintrojs)
library(readxl)
library(shinydashboard)
library(tidyverse)
library(scales)
library(interviewer)
library(shinyBS)
library(shiny)


# Panel div for visualization
# override the currently broken definition in shinyLP version 1.1.0
panel_div <- function(class_type, content) {
    div(class = sprintf("panel panel-%s", class_type),
        div(class = "panel-body", content))}

# Desarga de los datos
df <- read_excel("BaseDatosFinal.xlsx", 
                 col_types = c("text", "text", "text", "date", "text", "numeric", "text", "text", "text", "numeric"))

shinyUI(navbarPage(title = img(src="HR.LOGOred3_cropped.png", height = "40px"), id = "navBar",
                   theme = "paper.css",
                   collapsible = TRUE,
                   inverse = TRUE,
                   windowTitle = "",
                   position = "fixed-top",
                   footer = includeHTML("./www/P.Rhtml"),
                   header = tags$style(
                       ".navbar-right {
                       float: right !important;
                       }",
                       "body {padding-top: 100px;}"),
                   
                   tabPanel("INICIO", value = "home",
                            
                            shinyjs::useShinyjs(),
                            
                            tags$head(tags$script(HTML('
                                                       var fakeClick = function(tabName) {
                                                       var dropdownList = document.getElementsByTagName("a");
                                                       for (var i = 0; i < dropdownList.length; i++) {
                                                       var link = dropdownList[i];
                                                       if(link.getAttribute("data-value") == tabName) {
                                                       link.click();
                                                       };
                                                       }
                                                       };
                                                       '))),
                            fluidRow(
                                HTML("<section class='banner'>
                                     <h2 class='parallax'>OCIO CON LUPA</h2>
                                     </section>
                                     ")
                                ),
                            
                            
                            # Qué encontrarás aquí
                            fluidRow(column(3),column(6,
                                                      shiny::HTML("<br><br><center><h1>¿Qué encontrarás aquí?</h1></center><br>"),
                                                      shiny::HTML("<br><br><center><h5> Somos una plataforma dedicada a comparar lugares turísticos y culturales con el objetivo de facilitar su proceso de compra.</h5></center><br>")),
                                     column(3)),fluidRow(style = "height:1px;"),
                            
                            fluidRow(style = "height:40px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            # Como se utiliza la página
                            fluidRow(
                                shiny::HTML("<br><br><center> <h1>Pasos a seguir</h1> </center>
                                            <br>")
                                ),
                            
                            fluidRow(
                                column(3),
                                
                                # Número uno 
                                column(2,div(class="panel panel-default", 
                                           div(class="panel-body",  width = "600px",align = "center",
                                               div(tags$img(src = "one.png", width = "55px", height = "50px")),
                                               div(h5("Busca tu plan favorito en nuestro buscador de planes o elige la mejor actividad con nuestros sistema de filtrado."))))),
                                # Número dos
                                column(2,div(class="panel panel-default",
                                           div(class="panel-body",  width = "600px",align = "center",
                                               div(tags$img(src = "two.png", width = "55px", height = "50px")),
                                               div(h5("Filtra la búsqueda en cuanto a fechas, precios, ¡y muchas más cosas! No te adaptes a los planes, que ellos se adapten a ti."))))),
                                # Número tres
                                column(2, div(class="panel panel-default",
                                           div(class="panel-body",  width = "600px", align = "center",
                                               div(tags$img(src = "three.png", width = "55px", height = "50px")),
                                               div(h5("Lee detenidamente la información completa de la actividad para asegurarte de que es el plan perfecto."))))),
                                column(3)),

                            fluidRow(style = "height:50px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            # Botón de empezar 
                            fluidRow(shiny::HTML("<br><br><center> <h1>¡Empieza la aventura!</h1> </center><br>")),
                            fluidRow(column(3),column(6,tags$div(align = "center",
                                                                 tags$a("Empecemos", onclick="fakeClick('careerPF')", class="btn btn-primary btn-lg"))),
                                column(3)
                            ),
                            fluidRow(
                              bsModal(id = "startupModal", trigger = "Empecemos",
                                      title="SUSCRIBETE Y OBTEN UN DESCUENTO", size='medium',
                                      p('Por la suscripcion a nuestra pagina web podras obtener un 15% de descuento'),
                                      textInput('username', 'Usuario'),
                                      passwordInput('pwInp', 'Password'),
                                      actionButton('butLogin', 'Login', class = 'btn action-button btn-success', icon = icon('sign-in-alt')),
                                      footer = h4(actionLink('create_account',
                                                             'Crear una cuenta',
                                                             easyClose = TRUE, 
                                                             style="color: #e63127; background-color: #fff; border-color: #fff"),
                                                  align='right'),
                                      
                                      tags$head(tags$style("#window .modal-footer{display:none} .modal-header .close{display:none}"),
                                                tags$script("$(document).ready(function(){$('#window').modal();});")
                                      )
                                      
                              )
                              
                            ),
                            
                            fluidRow(style = "height:25px;"
                            )
                            
                            ), # Closes the first tabPanel called "Home"
                   
                   tabPanel("PLANES", value = "careerPF",
                            
                            list(tags$head(tags$style("body {background-color: #EDF3F5; }"))),
                            sidebarLayout(
                                sidebarPanel( width = 3, introjsUI(),
                                              
                                              useShinyjs(),
                                              
                                              tags$div(
                                                  
                                                  
                                                  shiny::HTML("<br><br><center> <h5>Busca tu plan:</h5> </center><br>"),
                                                  sidebarSearchForm(textId = "userName", buttonId = "searchButton", label = "Search..."),
                                                  
                                                  # Espacio
                                                  shiny::HTML("<h5></br></h5>"),
                                                  
                                                  data.step = 1, 
                                                  data.intro = "Empieza buscando un plan",
                                                  
                                                  introBox(tags$div(style = "height:50px;",
                                                                    
                                                                    actionLink("settings", "Filtros",
                                                                               style="color: #e63127; background-color: #fff; border-color: #fff",
                                                                               icon = icon("sliders-h", class = "fa-2x"))),
                                                           data.step = 6,
                                                           data.intro = "Settings is where you can set options that affect the graph and career statistics."
                                                  ),
                                                  
                                                  
                                                  wellPanel(style = "background:#FFFFFF",
                                                            
                                                            # Categorías populares
                                                            selectizeInput("changeAvatar", "Categorías populares:",
                                                                           choices = c(
                                                                               "Todas las categorias",
                                                                               "Visitas guiadas", 
                                                                               "Actividades al aire libre", 
                                                                               "Monumentos historicos",
                                                                               "Diversion y entretenimiento",
                                                                               "Museos")),
                                                            
                                                            
                                                            # Tipo de actividad
                                                            selectizeInput("changeAvatar1", "Tipo de actividad:",
                                                                           choices = c(
                                                                               "Acuarios",
                                                                               "Catedrales",
                                                                               "Conciertos y espectaculos",
                                                                               "Escape Rooms",
                                                                               "Parques naturales", 
                                                                               "Parque turistico", 
                                                                               "Museos")),
                                                            
                                                            # Seleccionar fecha
                                                            dateInput("rango", "Seleccione fecha:", value = Sys.Date()),
                                                            
                                                            # Seleccionar rango de precios    
                                                            sliderInput(
                                                                inputId='RangoPrecios',
                                                                label='Seleccione rango de precio:',
                                                                min=min(df$PRECIO),
                                                                max=max(df$PRECIO), 
                                                                value = c(min(df$PRECIO),max(df$PRECIO))),
                                                                                                        
                                                            actionButton(inputId="filtrox","Start", style="color: #fff; background-color: #85afcc; border-color: #2e6da4") #Start filtros
                                                            
                                                  )# Cerramos wellPanel
                                              )
                                              
                                ),  # Cerramos sidebarPanel
                                
                                
                                mainPanel( width = 8,
                                           fluidRow(tags$style(type="text/css",
                                                               ".shiny-output-error { visibility: hidden; }",
                                                               ".shiny-output-error:before { visibility: hidden; }"),
                                                    
                                                    introBox(panel_div(class_type = "default",content = tags$div(uiOutput("displayName"),
                                                                                                                 visNetwork::visNetworkOutput("visTest", height = "10px"),
                                                                                                                 DT::dataTableOutput('recommended'),
                                                                                                                 DT::dataTableOutput('mytable')
                                                                                                                 
                                                    )),
                                                    data.step = 4,
                                                    data.intro = "Your selections will be displayed here in a graph."),
                                                    
                                                    # Panel con siguiente y anterior
                                                    fluidRow(div(class="panel panel-default",div(class="panel-body",  width = "600px",
                                                                                                 tags$div(class = "wrap",div(class = "left",
                                                                                                                             style="display: inline-block;vertical-align:top; width: 150px;",
                                                                                                                             uiOutput("stepNo")),

                                                                                                          # Botón de Anterior
                                                                                                          div(class = "center",style="display: inline-block;vertical-align:top; width: 150px;",
                                                                                                              introBox(actionButton("goBack", label = "Anterior",
                                                                                                                                    icon = icon("arrow-circle-left", class = "fa-2x"),
                                                                                                                                    width= "150px", height= "40px",
                                                                                                                                    style="color: #fff; background-color: #85afcc; border-color: #2e6da4"),
                                                                                                                       data.step = 3,
                                                                                                                       data.intro = "Go back a step to edit your selection anytime." )),

                                                                                                          # Botón de siguiente
                                                                                                          div(class = "center",style="display: inline-block;vertical-align:top; width: 150px;",
                                                                                                              introBox(actionButton("btn1", label = "Siguiente",
                                                                                                                                    icon = icon("arrow-circle-right", class = "fa-2x"),
                                                                                                                                    width= "150px", height= "40px",
                                                                                                                                    style="color: #fff; background-color: #85afcc; border-color: #2e6da4"),
                                                                                                                       data.step = 2,
                                                                                                                       data.intro = "Confirm your selection by clicking here."))))),


                                                             # Pone los identificadores para cada boton de cada plan
                                                             shiny::includeScript("script.js")
                                                    )
                                           ) # Cerramos panel con siguiente y anterior
                                )  # Cerramos el mainPanel
                            )  # Cerramos el sidebarLayout
                   ),  # Cerramos el segundo tabPanel de "Planes"
                   
                   tabPanel("FAQ", value = "careerPF1",sidebarLayout(column(2),
                                                                     
                                                                     mainPanel( width = 8,fluidRow(tags$style(type="text/css",
                                                                                                              ".shiny-output-error { visibility: hidden; }",
                                                                                                              ".shiny-output-error:before { visibility: hidden; }"),
                                                                                                   # Abrimos panel en blanco
                                                                                                   introBox(panel_div(class_type = "default",
                                                                                                                      
                                                                                                                      # Abrimos el contenido                  
                                                                                                                      content = tags$div(shiny::HTML("<br><br><center><h2>Foro de preguntas y respuestas</h2> </center><br>"),
                                                                                                                                         shiny::HTML("<br><br><h5>Queremos que tengas la mejor y más sencilla experiencia comprando con nosotros. Pero sabemos que puedes tener algunas dudas. Encuentra toda la información con relación al funcionamiento de nuestra página web, inicio de sesión, precios, compras y mucho más.</h5><br>"),
                                                                                                                                         visNetwork::visNetworkOutput("visTest2", height = "10px"),
                                                                                                                                         
                                                                                                                                         # Foto PÁGINA WEB
                                                                                                                                         div(class="panel-body",  width = "600px",
                                                                                                                                             align = "center",
                                                                                                                                             div(tags$img(src = "paginaweb.png", 
                                                                                                                                                          width = "900px", height = "50px"))),
                                                                                                                                         
                                                                                                                                         # Pregunta 1.1
                                                                                                                                         shiny::HTML("<h7></br></h7>"),
                                                                                                                                         actionLink("pregunta1.1", shiny::HTML("<h7>1. ¿Tengo que pagar algo para utilizar la página web?</h7>"), width = 100,icon = icon("question", class = "fa-2x")),
                                                                                                                                         shiny::HTML("<h7></br></h7>"),
                                                                                                                                         
                                                                                                                                         # Respuesta 1.1
                                                                                                                                         box(id = "respuesta1.1", width = 100,"No, es totalmente gratis."),
                                                                                                                                         shiny::HTML("<h7></br></h7>"),
                                                                                                                                         
                                                                                                                                         # Pregunta 1.2
                                                                                                                                         actionLink("pregunta1.2", "2. ¿Tengo que registrarme para utilizar la página web?", icon = icon("question", class = "fa-2x")),
                                                                                                                                         # Respuesta 1.2
                                                                                                                                         box(id = "respuesta1.2", width = 100,"No hace falta registrarse para ver los contenidos publicados. Para realizar algún comentario o responder a uno existente, deberá registrarse como usuario de OcioConLupa."),
                                                                                                                                         shiny::HTML("<h7></br></h7>"),
                                                                                                                                         
                                                                                                                                         # Pregunta 1.3
                                                                                                                                         actionLink("pregunta1.3", "3.	Tengo otra pregunta. ¿Dónde me pueden ayudar?",icon = icon("question", class = "fa-2x")),
                                                                                                                                         # Respuesta 1.3
                                                                                                                                         box(id = "respuesta1.3", width = 100,"Para realizar preguntas que no estén presentes en nuestras preguntas frecuentes puede contactar con nosotros mediante nuestro correo electrónico ocioconlupa@gmail.com o mediante cualquiera de nuestras redes sociales presentes en el pie de página."),
                                                                                                                                         shiny::HTML("<h7></br></h7>"),
                                                                                                                                         
                                                                                                                                         # Foto INICIO DE SESIÓN
                                                                                                                                         div(class="panel-body",  width = "600px",
                                                                                                                                             align = "center",
                                                                                                                                             div(tags$img(src = "inicio.png", 
                                                                                                                                                          width = "900px", height = "47px"))),
                                                                                                                                         # Pregunta 2.1
                                                                                                                                         actionLink("pregunta2.1", "1.	¿Por qué no consigo iniciar una nueva sesión?",icon = icon("question", class = "fa-2x")),
                                                                                                                                         # Respuesta 2.1
                                                                                                                                         box(id = "respuesta2.1", width = 100,"Para iniciar sesión en nuestra página web, deberá utilizar la misma dirección de correo electrónico y la misma contraseña que utilizó para registrarse como usuario. Para poder iniciar una sesión, primero deberá registrarse. Si ya se ha registrado y sigue sin poder iniciar una sesión, envíe un correo de verificación a su cuenta y cambie su contraseña por una nueva. Si sigue sin poder iniciar una sesión, escriba a ocioconlupa@gmail.com."),
                                                                                                                                         shiny::HTML("<h7></br></h7>"),
                                                                                                                                         
                                                                                                                                         # Pregunta 2.2
                                                                                                                                         actionLink("pregunta2.2", "2.	He perdido mi contraseña. ¿Cómo puedo iniciar sesión?",icon = icon("question", class = "fa-2x")),
                                                                                                                                         # Respuesta 2.2
                                                                                                                                         box(id = "respuesta2.2", width = 100,"Si ha perdido su contraseña, vaya a la página de inicio de cuenta de OcioConLupa y haga clic en el botón Iniciar sesión/Registrarse. Luego sólo tiene que seguir las instrucciones para recuperar la contraseña perdida."),
                                                                                                                                         shiny::HTML("<h7></br></h7>"),
                                                                                                                                         
                                                                                                                                         # Pregunta 2.3
                                                                                                                                         actionLink("pregunta2.3", "3.	Me registré en otra ocasión, pero ahora no consigo iniciar una sesión",icon = icon("question", class = "fa-2x")),
                                                                                                                                         # Respuesta 2.3
                                                                                                                                         box(id = "respuesta2.3", width = 100,"Si está utilizando la dirección de correo electrónico correcta pero no tiene la contraseña, consulte las instrucciones anteriores para la recuperación de contraseñas perdidas. Si está seguro de que la dirección de correo electrónico y la contraseña que está utilizando son las correctas, escriba a ocioconlupa@gmail.com."),
                                                                                                                                         shiny::HTML("<h7></br></h7>")
                                                                                                                                         
                                                                                                                                         
                                                                                                                      ) #  Cerramos el contenido
                                                                                                                      
                                                                                                   )) # Cerramos panel blanco
                                                                     ),  # Cerramos el fluidRow
                                                                     
                                                                     fluidRow(plotOutput("myplot2") # No quitar
                                                                              
                                                                     ) # Cerramos el fluidRow
                                                                     ) # Cerramos el mainPanel
                   ) # Cerramos el sidebarLayout
                   ),  # Cerramos el segundo tabPanel de "FAQ"
                   
                   tabPanel("CHATBOT", value = "chatbot_id",sidebarLayout(column(2),
                                                                          
                                                                          mainPanel( 
                                                                            
                                                                            wellPanel(
                                                                              interviewer::useInterviewer(),
                                                                              shiny::uiOutput(outputId = "Personalizado")
                                                                            ),
                                                                            wellPanel(
                                                                              DT::dataTableOutput("tabla_chatbot")
                                                                            ) 
                                                                          ) 
                   ),#  Cerramos el contenido
                   plotOutput("myplot3"),
                   ), # Cerramos chatbot
                   
                   tabPanel("SOBRE NOSOTROS", value = "about",

                             fluidRow(
                                 shiny::HTML("<br><br><center>
                                             <h1>OcioConLupa</h1>
                                             <h4>Más información sobre nosostros</h4>
                                             </center>
                                             <br>
                                             <br>"),
                                 style = "height:250px;"),
                            
                             fluidRow(
                                 column(2),
                                 column(8,
                                        # Panel for Background on Data
                                        div(class="panel panel-default",
                                            div(class="panel-body",
                                                tags$div( align = "center"
                                                ),
                                                tags$p(h5("MISIÓN",style="color: #e63127; background-color: #fff; border-color: #fff")),
                                                tags$p(h6("Generar el máximo valor para nuestros usuarios satisfaciendo en todo momento sus necesidades y expectativas en la busca de actividades turísticas en Valencia.")),
                                                # Espacio
                                                shiny::HTML("<h5></br></h5>"),
                                                tags$p(h5("VISIÓN",style="color: #e63127; background-color: #fff; border-color: #fff")),
                                                tags$p(h6("Ser el comprardor líder de precios de actividades de ocio preferido por su garantía y confianza, adaptándonos a las necesidades y gustos de cada persona.")),
                                                # Espacio
                                                shiny::HTML("<h5></br></h5>"),
                                                tags$p(h5("VALORES",style="color: #e63127; background-color: #fff; border-color: #fff")),
                                                tags$ul(
                                                    tags$li(h6("Colaboración y respeto con nuestros clientes.")),
                                                    tags$li(h6("Profesionalidad y claidad en el servicio.")),
                                                    tags$li(h6("Compromiso social y solidario.")),
                                                    tags$li(h6("Trabajo en equipo."))
                                                )
                                            )
                                        ) # Closes div panel
                                 ), # Closes column
                                 column(2)
                             ),
                            # TEAM BIO
                             fluidRow(
                                 column(3),
                                 column(6,
                                        shiny::HTML("<br><br><center><h4>Nuestro equipo</h4></center><br>"),
                                        shiny::HTML("<h5>Somos cuatro estudiantes del Grado en Ciencia de datos en la Universitat de València interesados en machine learning, análisis de series temporales, visualización y tratamiento de datos, Inteligencia Artificial, y sobre todo, en el emprendimiento.</h5>"),
                                        shiny::HTML("<h5>Juntos hemos creado Ocioconlupa, una startup que surge de la asignatura Gestión de la innovación, basada en la colaboración y trabajo en equipo. ¡Moiceleniri al poder!</h5>")
                                 ),
                                 column(3)
                                        ),

                             fluidRow(
                            
                                 style = "height:50px;"),
                            
                             fluidRow(
                                 column(2),
                            
                                # Irina
                                 column(2,
                                        div(class="panel panel-default",
                                            div(class="panel-body",  width = "600px",
                                                align = "center",
                                                div(
                                                    tags$img(src = "iri.svg",
                                                             width = "90px", height = "90px")
                                                ),
                                                div(
                                                    tags$h5("Irina Filimonova"),
                                                    tags$h6( tags$i("Data Scientist & Programmer"))
                                                ),
                                                div(
                                                    ""
                                                )
                                            )
                                        )
                                 ),
                                # Elena
                                 column(2,
                                        div(class="panel panel-default",
                                            div(class="panel-body",  width = "600px",
                                                align = "center",
                                                div(
                                                    tags$img(src = "helen.svg",
                                                             width = "90px", height = "90px")
                                                ),
                                                div(
                                                    tags$h5("Elena Marrero"),
                                                    tags$h6( tags$i("Data Scientist & Programmer"))
                                                ),
                                                div(
                                                    " "
                                                )
                                            )
                                        )
                                 ),
                                 # Ceci
                                 column(2,
                                        div(class="panel panel-default",
                                            div(class="panel-body",  width = "600px",
                                                align = "center",
                                                div(
                                                    tags$img(src = "ceci.svg",
                                                             width = "90px", height = "90px")),
                                                div(
                                                    tags$h5("Cecilia Diana"),
                                                    tags$h6( tags$i("Data Scientist & Programmer"))
                                                ),
                                                div(
                                                    " "
                                                )
                                            )
                                        )
                                ),
                                # Moi
                                column(2,
                                       div(class="panel panel-default",
                                           div(class="panel-body",  width = "600px",
                                               align = "center",
                                               div(
                                                 tags$img(src = "moi.svg",
                                                          width = "90px", height = "90px")),
                                               div(
                                                 tags$h5("Moisés Barrios"),
                                                 tags$h6( tags$i("Data Scientist & Programmer"))
                                               ),
                                               div(
                                                 " "
                                               )
                                           )
                                       )
                                ),
                            
                             ),
                            fluidRow(style = "height:150px;")
                                )  # Closes About tab
                   
                            )
        
                   )