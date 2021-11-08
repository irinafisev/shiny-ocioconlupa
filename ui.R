
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
library(DT)
library(bsModal)

# Panel div para la visualization
panel_div <- function(class_type, content) {
    div(class = sprintf("panel panel-%s", class_type),
        div(class = "panel-body", content))}

# Desarga de los datos
df <- read_excel("BaseDatosFinal.xlsx", 
                 col_types = c("text", "text", "text", "date", "text", "numeric", "text", "text", "text", "numeric"))


# Empezamos la interfaz de usuario
shinyUI(navbarPage(title = img(src="HR.LOGOred3_cropped.png", height = "40px"), id = "navBar",
                   theme = "paper.css",
                   collapsible = TRUE,
                   inverse = TRUE,
                   windowTitle = "Los Angeles County Career PathFinder",
                   position = "fixed-top",
                   footer = includeHTML("./www/P.Rhtml"),
                   #footer = includeHTML("./www/include_footer.html"),
                   header = tags$style(
                       ".navbar-right {
                       float: right !important;
                       }",
                       "body {padding-top: 75px;}"),
                   
                   
                   
                   
                    
                   
                   # tabPanel("INICIO", value = "home",
                   #          
                   #          shinyjs::useShinyjs(),
                   #          
                   #          tags$head(tags$script(HTML('
                   #                                     var fakeClick = function(tabName) {
                   #                                     var dropdownList = document.getElementsByTagName("a");
                   #                                     for (var i = 0; i < dropdownList.length; i++) {
                   #                                     var link = dropdownList[i];
                   #                                     if(link.getAttribute("data-value") == tabName) {
                   #                                     link.click();
                   #                                     };
                   #                                     }
                   #                                     };
                   #                                     '))),
                   #          fluidRow(
                   #              HTML("
                   #                   <section class='banner'>
                   #                   <h2 class='parallax'>OCIO CON LUPA</h2>
                   #                   </section>
                   #                   ")
                   #              ),
                   #          #<h2 class='parallax'>OCIO CON LUPA</h2>
                   #          
                   #          # WHAT
                   #          # Qué encontrarás aquí
                   #          fluidRow(column(3),column(6,
                   #                                    shiny::HTML("<br><br><center><h1>¿Qué encontrarás aquí?</h1></center><br>"),
                   #                                    shiny::HTML("<br><br><center><h5> Somos una plataforma dedicada a comparar lugares turísticos y culturales con el objetivo de facilitar su proceso de compra.</h5></center><br>")),
                   #                   column(3)),fluidRow(style = "height:50px;"),
                   #          
                   #          fluidRow(style = "height:50px;"),
                   #          
                   #          # PAGE BREAK
                   #          tags$hr(),
                   #          
                   #          # Como se utiliza la página
                   #          fluidRow(
                   #              shiny::HTML("<br><br><center> <h1>Pasos a seguir</h1> </center>
                   #                          <br>")
                   #              ),
                   #          
                   #          fluidRow(
                   #              column(3),
                   #              
                   #              # Número uno 
                   #              column(2,div(class="panel panel-default", 
                   #                         div(class="panel-body",  width = "600px",align = "center",
                   #                             div(tags$img(src = "one.png", width = "55px", height = "50px")),
                   #                             div(h5("Busca tu plan favorito en nuestro buscador de planes o elige la mejor actividad con nuestros sistema de filtrado."))))),
                   #              # Número dos
                   #              column(2,div(class="panel panel-default",
                   #                         div(class="panel-body",  width = "600px",align = "center",
                   #                             div(tags$img(src = "two.png", width = "55px", height = "50px")),
                   #                             div(h5("Filtra la búsqueda en cuanto a fechas, precios, ¡y muchas más cosas! No te adaptes a los planes, que ellos se adapten a ti."))))),
                   #              # Número tres
                   #              column(2, div(class="panel panel-default",
                   #                         div(class="panel-body",  width = "600px", align = "center",
                   #                             div(tags$img(src = "three.png", width = "55px", height = "50px")),
                   #                             div(h5("Lee detenidamente la información completa de la actividad para asegurarte de que es el plan perfecto."))))),
                   #              column(3)),
                   # 
                   #          fluidRow(style = "height:50px;"),
                   #          
                   #          # PAGE BREAK
                   #          tags$hr(),
                   #          
                   #          # Botón de empezar 
                   #          fluidRow(shiny::HTML("<br><br><center> <h1>¡Empieza la aventura!</h1> </center><br>")),
                   #          fluidRow(column(3),column(6,tags$div(align = "center",
                   #                                               tags$a("Empecemos", onclick="fakeClick('careerPF')", class="btn btn-primary btn-lg"))),
                   #              column(3)
                   #          ),
                   #          fluidRow(style = "height:25px;"
                   #          )
                   #          
                   #          ), # Closes the first tabPanel called "Home"
                   
                   tabPanel("PLANES", value = "careerPF",
                            
                            list(tags$head(tags$style("body {background-color: #EDF3F5; }"))),
                            
                            
                            sidebarLayout(
                                sidebarPanel( width = 3, introjsUI(),
                                              
                                              ##################################################################
                                              
                                              # tags$div(
                                              #     actionButton("help", "Take a Quick Tour"),
                                              #     style = "height:50px;"
                                              # ),
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
                                                                     icon = icon("sliders", class = "fa-2x"))),
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
                                                              #cc(min(titanic$Age, na.rm=T), max(titanic$Age, na.rm=T))
                                                              
                                                              
                                                              # tags$div(
                                                              #     style = "height:50px;",
                                                              #     uiOutput("printInput1"),
                                                              #     uiOutput("printInput2"),
                                                              #     uiOutput("printInput3"),
                                                              #     uiOutput("printInput4"),
                                                              #     uiOutput("printInput5"))                                              
                                                          actionButton(inputId="filtrox","Start", style="color: #fff; background-color: #85afcc; border-color: #2e6da4") #Start filtros
                                                  
                                                  )# Cerramos wellPanel
                                              )
                                                  
                                ),  # Cerramos sidebarPanel
                                
                                
                                
                                mainPanel( width = 8,
                                           
                                           fluidRow(
                                               bsModal(id = "startupModal", trigger = "mytable",
                                                       title="SUSCRIBETE Y OBTEN UN DESCUENTO",size='medium',
                                                       p('Por la suscripcion a nuestra pagina web podras obtener un 15% de descuento'),
                                                       textInput('username', 'Usuario'),
                                                       passwordInput('pwInp', 'Password'),
                                                       actionButton('butLogin', 'Login', class = 'btn action-button btn-success', icon = icon('sign-in')),
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
                                                    
                                            
                                               plotOutput("myplot"), # No quitar
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
                                                                 actionLink("pregunta1.1", shiny::HTML("<h7>1. ¿Tengo que pagar algo para utilizar la página web?</h7>"),icon = icon("question", class = "fa-2x",style="color: #e63127; background-color: #fff; border-color: #fff")),
                                                                 shiny::HTML("<h7></br></h7>"),
                                                                 
                                                                 # Respuesta 1.1
                                                                 box(id = "respuesta1.1", width = 100,"No, es totalmente gratis."),
                                                                 shiny::HTML("<h7></br></h7>"),
                                                                 
                                                                 # Pregunta 1.2
                                                                 actionLink("pregunta1.2", "2. ¿Tengo que registrarme para utilizar la página web?",icon = icon("question", class = "fa-2x")),
                                                                 # Respuesta 1.2
                                                                 box(id = "respuesta1.2", width = 100,"No hace falta registrarse para ver los contenidos publicados. Para realizar algún comentario o responder a uno existente, deberá registrarse como usuario de OcioConLupa. Tiene dos opciones: estar suscrito a nuestra página web o, si no, puede registrarse ahora."),
                                                                 shiny::HTML("<h7></br></h7>"),
                                                                 
                                                                 # Pregunta 1.3
                                                                 actionLink("pregunta1.3", "3.	Tengo otra pregunta. ¿Dónde me pueden ayudar?",icon = icon("question", class = "fa-2x")),
                                                                 # Respuesta 1.3
                                                                 box(id = "respuesta1.3", width = 100,"Para realizar preguntas que no estén presentes en nuestras preguntas frecuentes puede contactar con nosotros mediante nuestro correo electrónico ocioconlupa@gmail.com o meidantecualquiera de nuestras redes sociales presentes en el pie de página."),
                                                                 shiny::HTML("<h7></br></h7>"),
                                                                 
                                                                 # Pregunta 1.4
                                                                 actionLink("pregunta1.4", "4. ¿Tengo que registrarme para utilizar la página web?",icon = icon("question", class = "fa-2x")),
                                                                 # Respuesta 1.4
                                                                 box(id = "respuesta1.4", width = 100,"No hace falta registrarse para ver los contenidos publicados. Para realizar algún comentario o responder a uno existente, deberá registrarse como usuario de OcioConLupa. Tiene dos opciones: estar suscrito a nuestra página web o, si no, puede registrarse ahora."),
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
                                                                 box(id = "respuesta2.3", width = 100,"10.	Si está utilizando la dirección de correo electrónico correcta pero no tiene la contraseña, consulte las instrucciones anteriores para la recuperación de contraseñas perdidas. Si está seguro de que la dirección de correo electrónico y la contraseña que está utilizando son las correctas, escriba a ocioconlupa@gmail.com."),
                                                                 shiny::HTML("<h7></br></h7>")
                                                                 
                                                                 
                                                                 ) #  Cerramos el contenido
                                              
                                              )) # Cerramos panel blanco
                              ),  # Cerramos el fluidRow
                              
                              fluidRow(plotOutput("myplot2") # No quitar
                                       
                              ) # Cerramos el fluidRow
                            ) # Cerramos el mainPanel
                            ) # Cerramos el sidebarLayout
                    ),  # Cerramos el segundo tabPanel de "FAQ"
                   
                   
                   
                   
                   
                   # 
                   # tabPanel("SOBRE NOSOTROS", value = "about",
                   #          
                            # fluidRow(
                            #     shiny::HTML("<br><br><center> 
                            #                 <h1>About Career PathFinder</h1> 
                            #                 <h4>What's behind the data.</h4>
                            #                 </center>
                            #                 <br>
                            #                 <br>"),
                            #     style = "height:250px;"),
                            # fluidRow(
                            #     div(align = "center",
                            #         tags$span(h4("A Brief History of Los Angeles County's Career PathFinder"), 
                            #                   style = "font-weight:bold"
                            #         ))
                            # ),
                            # fluidRow(
                            #     column(3),
                            #     column(6,
                            #            tags$ul(
                            #                tags$li(h6("The need for the Career PathFinder grew out of the fact that it is simply difficult to navigate the classification structure if you do not already know it or know someone who has gone through it.")), 
                            #                tags$li(h6("The Workforce and Employee Development team wanted to help others help themselves by providing an online tool that sheds light on the otherwise invisible career paths in the County.")), 
                            #                tags$li(h6("In 2016, the Los Angeles County Quality and Productivity Commission granted the seed money that got the ball rolling.")),
                            #                tags$li(h6("We wanted to augment training provided through the Los Angeles County University by helping people see real career opportunities after taking a course that improved their skills.")),
                            #                tags$li(h6("Now, and into the future, we want to be a magnet for top talent and be an employer of choice."))
                            #            )
                            #     ),
                            #     column(3)
                            # ),
                            # fluidRow(
                            #     column(2),
                            #     column(8,
                            #            # Panel for Background on Data
                            #            div(class="panel panel-default",
                            #                div(class="panel-body",  
                            #                    tags$div( align = "center",
                            #                              icon("bar-chart", class = "fa-4x"),
                            #                              div( align = "center", 
                            #                                   h5("About the Data")
                            #                              )
                            #                    ),
                            #                    tags$p(h6("Over 30 years of data were collected, which resulted in nearly 500,000 records of career movement. Several business rules were developed to ensure the data reflected real opportunities in the current classification system.")),
                            #                    tags$ul(
                            #                        tags$li(h6("Any career movement within 30 days of a previous career movement was ignored. Although this represents a small percent of movement, these job transitions may have reflected data entry errors and may have skewed probabilities in jobs with a small number of incumbents.")),
                            #                        tags$li(h6("Multiple transfers within the same classification were ignored when there were 2 or more transfers to the same position. Although single transfers were counted, multiple transfers inflated the likelihood that the next step was a transfer.")),
                            #                        tags$li(h6("Expired classifications were removed from an individual's career path in the source data, because their path is no longer possible in the current system.")),
                            #                        tags$li(h6("Minor demotions were retained in the data to reflect deliberate career choices; however, demotions of a significant percent were excluded."))
                            #                    )
                            #                )
                            #            ) # Closes div panel
                            #     ), # Closes column
                            #     column(2)
                            # ),
                            # TEAM BIO
                            # fluidRow(
                            #     column(3),
                            #     column(6,
                            #            shiny::HTML("<br><br><center> <h5>About the team</h5> </center><br>"),
                            #            shiny::HTML("<h6>The Career PathFinder is sponsored by the Los Angeles County 
                            #                        Department of Human Resources, with financial support from the 
                            #                        Quality and Productivity Commission. And here is a little information 
                            #                        about the project team!</h6>")
                            #            ),
                            #     column(3)
                            #            ),
                            
                            # fluidRow(
                            #     
                            #     style = "height:50px;"),
                            # 
                            # fluidRow(
                            #     column(3),
                            #     
                                # Marc
                                # column(2,
                                #        div(class="panel panel-default", 
                                #            div(class="panel-body",  width = "600px",
                                #                align = "center",
                                #                div(
                                #                    tags$img(src = "man_beard_1.svg", 
                                #                             width = "60px", height = "50px")
                                #                ),
                                #                div(
                                #                    tags$h5("Marc"),
                                #                    tags$h6( tags$i("Visionary & Project Lead"))
                                #                ),
                                #                div(
                                #                    "My County career path started as a Human Resources Analyst."
                                #                )
                                #            )
                                #        )
                                # ),
                                # George
                                # column(2,
                                #        div(class="panel panel-default",
                                #            div(class="panel-body",  width = "600px", 
                                #                align = "center",
                                #                div(
                                #                    tags$img(src = "man.svg", 
                                #                             width = "50px", height = "50px")
                                #                ),
                                #                div(
                                #                    tags$h5("George"),
                                #                    tags$h6( tags$i("Data Scientist & Programmer"))
                                #                ),
                                #                div(
                                #                    "My County career path started as an Intermediate Typist Clerk."
                                #                )
                                #            )
                                #        )
                                # ),
                                # Angela
                                # column(2,
                                #        div(class="panel panel-default",
                                #            div(class="panel-body",  width = "600px", 
                                #                align = "center",
                                #                div(
                                #                    tags$img(src = "woman.svg", 
                                #                             width = "50px", height = "50px")),
                                #                div(
                                #                    tags$h5("Angela"),
                                #                    tags$h6( tags$i("Writer"))
                                #                ),
                                #                div(
                                #                    "My County career path started as an Administrative Assistant."
                                #                )
                                #            )
                                #        )
                                # ),
                            #     column(3)
                            #     
                            # ),
                            # fluidRow(style = "height:150px;")
                            #     )  # Closes About tab
                   
                   
                   
        ################################## CHATBOOOOOOT CECI
        
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
                                                               )#  Cerramos el contenido
                                                                                                           
                                  ) # Cerramos chatbot
     
                   )
)

              