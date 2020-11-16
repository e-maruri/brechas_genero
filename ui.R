library(shiny)
library(shinydashboard)
library(shinyWidgets)

## 1. Header --------------------------------
header <- 
  dashboardHeader( title = "Brecha de género en México", 
                   disable = FALSE, 
                   titleWidth  = 550)

## 2. siderbar ------------------------------
siderbar <- 
  dashboardSidebar( 
    width = 200,
    sidebarMenu(
      id = 'sidebar',
      style = "position: relative; overflow: visible;",
      ## ENOE -----------
      menuItem("ENOE", icon = icon("toolbox"),
               menuSubItem("Resumen", tabName = "enoe_intro"),
               menuSubItem("Nacional", tabName = "enoe"),
               menuSubItem("Estatal", tabName = "enoe_edos")),
      
      ## ENUT -----------
      menuItem( "Uso del Tiempo", tabName = 'enut', icon = icon('fist-raised'),
                menuSubItem("Resumen", tabName = "enut_res"),
                menuSubItem("Distribución del Tiempo", tabName = "enut_time"),
                menuSubItem("Estatal", tabName = "enut_est"),
                menuSubItem("Nacional", tabName = "enut_nac"))))

## 3. body --------------------------------
body<-dashboardBody(
  tabItems(
    ## ENOE -----------
    tabItem(tabName = "enoe_intro", 
            h1("Encuesta Nacional de Ocupación y Empleo", align="center"), 
            boxPlus(title="Descripción del contenido", closable=F,  collapsible=F, collapsed=T, status="danger", width=12, 
                    p("Esta sección muestra las brechas de género existentes en la Encuesta Nacional de Ocupación y Empleo (ENOE), 
                     que es la encuesta más importante a nivel nacional para conocer las condiciones laborales de l@s mexican@s. 
                     El periodo que abarca es desde el primer trimestre de 2005 hasta el primer trimestre de 2020. 
                     En todos los casos, se considera sólo a las personas entre 15 y 98 años de edad, que es el edad legal para trabajar, 
                     al igual que lo hace el INEGI para reportar los datos de empleo oficiales.")),
            valueBoxOutput("enoe_brecha_intro_xhora", width = 4),
            valueBoxOutput("enoe_brecha_intro_mensual", width = 4),
            box(width=4, 
                helpText("Seleccione un año para conocer la brecha salarial promedio."),
                sliderInput("enoe_brecha_intro_year", "Año:", min=2005, max=2020, value=2020, sep = "") ),
            fluidRow(
              box(width = 8, 
                  plotlyOutput("graph_enoe_brecha_sal_anual_barras")),
              box(width = 4,
                  selectInput("enoe_brecha_sal_anual_barras_var", "Variable:", 
                              choices = list("Ingreso por hora" = "ing_x_hrs", "Ingreso mensual" = "ingocup")),
                  sliderInput("enoe_brecha_sal_anual_barras_year", "Periodo:", min=2005, max=2020, value=c(2018, 2020), sep = ""))
            ) # Termina fluidRow
    ), # Termina sección ENOE_intro
    
    tabItem(tabName = "enoe", 
            h1("Brechas de género en la ENOE a nivel nacional", align="center"),
            fluidRow(
              tabBox(
                title = NULL, width = 12,
                id = "tabset1", height = "250px",
                tabPanel("Salarios", "", # Inicia primera pestaña 
                         box(width = 12, 
                             h3("Brecha salarial de género a nivel nacional", align="center"),
                             h5("(Promedio anual)", align="center"),
                             plotlyOutput("graph_enoe_brecha_sal_anual")),
                         br(),
                         box(width = 8,
                             helpText("Seleccione el periodo para mostrar"),
                             sliderInput("enoe_brecha_nacional_year", "Periodo", min=2005, max=2020, value=c(2005, 2020), sep = "")
                         ),
                         br(),
                         box(width = 10, 
                             h3("Brecha salarial de género por deciles de ingreso", align="center"),
                             h5("(ingreso por hora)", align="center"),
                             plotlyOutput("graph_enoe_brecha_sal_deciles")),
                         box(width = 2,
                             helpText("Haga doble click en el año que desee ver.")
                         ),
                         
                         box(width = 8, 
                             h3("Brecha salarial de género por sector económico y por nivel educativo", align="center"), 
                             h5("(ingreso por hora)", align="center"), 
                             plotlyOutput("graph_enoe_brecha_sal_anual_cruzada")), 
                         br(),
                         box(width = 4, 
                             helpText("Seleccione el sector económico y el periodo para mostrar, para elegir un nivel educativo haga doble click sobre este directamente en la gráfica."),
                             selectInput("enoe_brecha_sal_anual_cruzada_sector", "Sector económico", 
                                         choices = list("Primario" = "Primario", "Secundario" = "Secundario", "Terciario" = "Terciario")),
                             sliderInput("enoe_brecha_sal_anual_cruzada_year", "Periodo", min=2005, max=2020, value=c(2005, 2020), sep = "")
                         ),   
                         
                         box(width = 6, 
                             h3("Brecha salarial de género por sector económico", align="center"), 
                             h5("(ingreso por hora)", align="center"), 
                             plotlyOutput("graph_enoe_brecha_sal_anual_sector")), 
                         br(),
                         
                         box(width = 6, 
                             h3("Brecha salarial de género por nivel educativo", align="center"), 
                             h5("(ingreso por hora)", align="center"), 
                             plotlyOutput("graph_enoe_brecha_sal_anual_nivel_educ")), 
                         br(),
                         
                         box(width = 6, 
                             helpText("Seleccione el periodo para mostrar"),
                             sliderInput("enoe_brecha_sal_anual_sector_year", "Periodo", min=2005, max=2020, value=c(2005, 2020), sep = "")),
                         
                         box(width = 6,
                             helpText("Seleccione el periodo para mostrar"),
                             sliderInput("enoe_brecha_sal_anual_nivel_educ_year", "Periodo", min=2005, max=2020, value=c(2005, 2020), sep = "")), 
                         
                         box(width = 8, 
                             h3("Brecha salarial de género por condición laboral", align="center"), 
                             h5("(ingreso por hora)", align="center"), 
                             plotlyOutput("graph_enoe_brecha_sal_anual_informal")),
                         
                         box(width = 4,
                             helpText("Seleccione el periodo para mostrar"),
                             sliderInput("enoe_brecha_sal_anual_informal_year", "Periodo", min=2005, max=2020, value=c(2005, 2020), sep = ""))
                         
                ), # Termina primera pestaña
                
                tabPanel("Empleo", "", # Inicia segunda pestaña 
                         fluidRow(
                           box(width = 4, 
                               selectInput("enoe_var_emp", label = "Seleccione una variable:",
                                           choices = list("Tasa de participación" = "tp", 
                                                          "Tasa de desempleo" = "desempleo", 
                                                          "Tasa de ocupación en el sector informal" = "tosi", 
                                                          "Tasa de informalidad laboral" = "til"), 
                                           selected = "desempleo"), 
                               sliderInput("enoe_empleo_year", "Periodo", min=2005, max=2020, value=c(2005, 2020), sep = "")), 
                           box(width = 8, 
                               plotlyOutput("graph_enoe_empleo")), 
                           box(width = 8, 
                               plotlyOutput("graph_enoe_empleo_econ")),
                           box(width = 4, 
                               selectInput("enoe_empleo_econ_var", label = "Seleccione una variable:",
                                           choices = list("Tasa de participación" = "tp", 
                                                          "Tasa de desempleo" = "desempleo", 
                                                          "Tasa de ocupación en el sector informal" = "tosi", 
                                                          "Tasa de informalidad laboral" = "til"), 
                                           selected = "tp"), 
                               sliderInput("enoe_empleo_econ_year", "Año:", min=2005, max=2020, value=2020, sep = ""), 
                               radioButtons("enoe_empleo_econ_q", label = "Trimestre:",
                                            choices = list("Primero" = 1, "Segundo" = 2, 
                                                           "Tercero" = 3, "Cuarto" = 4) ), 
                               helpText("Nota: la combinación año 2020 con trimestre distinto al primero no está disponible.")
                           ), 
                           box(width = 8, 
                               plotlyOutput("graph_enoe_empleo_educ")),
                           box(width = 4, 
                               selectInput("enoe_empleo_educ_var", label = "Seleccione una variable:",
                                           choices = list("Tasa de participación" = "tp", 
                                                          "Tasa de desempleo" = "desempleo", 
                                                          "Tasa de ocupación en el sector informal" = "tosi", 
                                                          "Tasa de informalidad laboral" = "til"), 
                                           selected = "tp"), 
                               sliderInput("enoe_empleo_educ_year", "Año:", min=2005, max=2020, value=2020, sep = ""), 
                               radioButtons("enoe_empleo_educ_q", label = "Trimestre:",
                                            choices = list("Primero" = 1, "Segundo" = 2, 
                                                           "Tercero" = 3, "Cuarto" = 4) ), 
                               helpText("Nota: la combinación año 2020 con trimestre distinto al primero no está disponible.")
                           )   
                         ) # Termina fluid row
                ) # Termina segunda pestaña
              ), # Terminan pestañas
              
            ), # Termina fluid row
    ), # Termina sección ENOE_nacional
    
    tabItem(tabName = "enoe_edos", 
            h1("Brechas de género en la ENOE a nivel estatal", align="center"),
            fluidRow(
              box(width = 8, 
                  h3("Brecha salarial de género en el ingreso por hora", align="center"), 
                  h5("(Promedio anual)", align="center"), 
                  plotlyOutput("enoe_brecha_sal_edos_map")),
              box(width = 4,
                  sliderInput("enoe_brecha_sal_edos_year", "Año", min=2005, max=2020, value=2020, sep = "")), 
              
              box(width = 8, 
                  h3("Indicadores de empleo", align="center"), 
                  h5("(Promedio anual)", align="center"),  
                  plotlyOutput("enoe_empleo_edos_map")), 
              box(width = 4,
                  selectInput("enoe_empleo_edos_sex", label = "Seleccione un sexo:",
                              choices = list("Mujer"="Mujer", "Hombre"="Hombre"), 
                              selected = "Mujer"),
                  selectInput("enoe_empleo_edos_var", label = "Seleccione una variable:",
                              choices = list("Tasa de participación" = "tp", 
                                             "Tasa de desempleo" = "desempleo", 
                                             "Tasa de ocupación en el sector informal" = "tosi", 
                                             "Tasa de informalidad laboral" = "til"), 
                              selected = "tp"), 
                  sliderInput("enoe_empleo_edos_year", "Año", min=2005, max=2020, value=2020, sep = "")), 
            ) # Termina fluidRow
    ), # Termina sección ENOE_edos
    
    
    ## ENUT -----------
    tabItem("enut_res", h2("Encuesta Nacional sobre el Uso del Tiempo", align="center"), 
            widgetUserBox(title="Marilyin Waring", width=6, type=2, src="waring2.jpg", color="yellow",
                          HTML("<em>Los hombres no abandonarán fácilmente un sistema en el que la mitad del planeta trabaja para la otra mitad a cambio de nada. </em>" )),
            widgetUserBox(title="Federico Engels", width=6, type=2, src="engels2.jpg", color="red",
                          HTML("<em> El primer antagonismo de clases que apareció en la historia coincide 
                                                 con el desarrollo del antagonismo entre el hombre y la mujer en la monogamia; y la primera opresión de clases, con la del sexo femenino por el masculino. </em>" )),
    boxPlus(title="Descripción del contenido", closable = FALSE,  collapsible = TRUE, collapsed=TRUE, status="danger", 
            p("Esta sección denominada 'Uso del Tiempo' tiene el propósito de presentar otras dimensiones de la brecha de género, además de la brecha salarial. Para ello, se emplearon datos de la Encuesta Nacional de Uso del Tiempo (ENUT) tomando como población a los mexicanos y mexicanas mayores de 12 años.
              Es necesario aclarar que cuando se presenta el promedio de horas dedicadas a cierta actividad, en todos los casos se refiere al promedio de horas semanales tomando únicamente en cuenta a las personas que sí participaron en la actividad en cuestión durante la semana previa a la encuesta.
              Asimismo, las brechas que se presentan corresponden a la diferencia entre el promedio de las mujeres respecto del promedio de los hombres, por lo que una diferencia positiva significa que la media de las mujeres fue mayor."), width=12),
    valueBox(value="6.3 hrs", "Brecha promedio semanal entre mujeres y hombres en el Tiempo Total de Trabajo en 2019", color="aqua" , width=3),
    valueBox(value="-9.8 hrs", "Brecha promedio semanal entre mujeres y hombres en el Tiempo de Trabajo Remunerado en 2019", color="green", width=3),
    valueBox(value="24.5 hrs", "Brecha promedio semanal entre mujeres y hombres en el Tiempo de Trabajo No Remunerado en 2019", color="olive", width=3),
    valueBox(value="-4.1%", "Brecha promedio entre mujeres y hombres en el ingreso por hora en 2019", color="fuchsia", width=3),
    fluidRow(gradientBox(title="Seleccione un tipo de trabajo", icon="fa fa-jedi", gradientColor = "teal" ,footer=selectInput("mextrab", label=h4(strong('Variable')), choices=list("Log del ingreso por hora"=0, "Trabajo Total"=1, "Remunerado"=2,	"No Remunerado"=3,	"Producción de autoconsumo"=4, "Doméstico"=5, "De Cuidado"=6, "Comunitario"=7, "Preparación de alimentos"=8, "Limpieza de vivienda"=9, 
                                                                                   "Limpieza de ropa"=10, "Reparaciones del hogar"=11, "Cuidado de menores de 5"=12, "Cuidado de menores de 14"=13, "Cuidado de mayores de 60"=14, "Estudio"=15), selected=1), boxToolSize = "xs", width=12)),
    box(plotlyOutput("hrsmex"), width=6), box(plotlyOutput("partmex"), width=6)),
    
    tabItem("enut_time", h2("Distribución Porcentual del Tiempo Total de Trabajo 2019", align="center" ),
            fluidRow(gradientBox(title="Seleccione un grupo", icon="fa fa-venus-mars", gradientColor = "maroon",footer=radioButtons("sexo", label=h4(strong('Sexo')), choices=list("Hombres"=1, "Mujeres"=2), selected=1),
                         width=6, boxToolSize = "xs")),
            box(DT::dataTableOutput("RawData")), 
            infoBox("Trabajo no remunerado", value = "49.4%", subtitle = "Porcentaje Nacional del Tiempo Total de Trabajo incluyendo ambos sexos",
                    icon = shiny::icon("shopping-cart"), color = "aqua", width = 4,
                    href = NULL, fill = TRUE),
            infoBox("Trabajo remunerado", value = "47.9%", subtitle = "Porcentaje Nacional del Tiempo Total de Trabajo incluyendo ambos sexos",
                    icon = shiny::icon("coins"), color = "purple", width = 4,
                    href = NULL, fill = TRUE),
            infoBox("Producción de bienes para el hogar", value = "2.7%", subtitle = "Porcentaje Nacional del Tiempo Total de Trabajo incluyendo ambos sexos",
                    icon = shiny::icon("egg"), color = "light-blue", width = 4,
                    href = NULL, fill = TRUE),
            box(selectInput("ppie", label=h4(strong('Población')), choices=list("Indígenas"=1, "No Indígenas"=2), selected=1), 
                plotlyOutput("enutpie"), width=6)
            ),
    tabItem("enut_est", h2("Brecha de género en las horas de trabajo semanales y en las tasas de participación 2019", align="center"),
            fluidRow(box(selectInput("trabajo", label = h4(strong('Brecha en el Tiempo de Trabajo'), br(), 'Tipo de Trabajo'),
                                     choices = list( "Log del ingreso por hora"=0, "Trabajo Total"=1, "Remunerado"=2,	"No Remunerado"=3,	"Producción de autoconsumo"=4, "Doméstico"=5, "De Cuidado"=6, "Comunitario"=7, "Preparación alimentos"=8, "Limpieza vivienda"=9, "Limpieza ropa"=10, "Reparaciones del hogar"=11,
                                                     "Cuidado de menores de 5"=12, "Cuidado de menores de 14"=13, "Cuidado de mayores de 60"=14, "Estudio"=15),  
                                     selected = 2), width=6 ),
                     box(selectInput("trabajop", label = h4(strong('Brecha en la Tasa de Participación'), br(), 'Tipo de Trabajo'),
                                     choices = list("Remunerado"=1,	"No Remunerado"=2,	"Producción de autoconsumo"=3, "Doméstico"=4, "De Cuidado"=5, "Comunitario"=6, "Preparación alimentos"=7, "Limpieza vivienda"=8, "Limpieza ropa"=9, "Reparaciones del hogar"=10,
                                                    "Cuidado de menores de 5"=11, "Cuidado de menores de 14"=12, "Cuidado de mayores de 60"=13, "Estudio"=14),  
                                     selected = 1 ), width=6),
                     box(plotlyOutput("mapahrs"), width=6), box(plotlyOutput("mapapart"), width=6),
                     box(plotlyOutput("hrshombres"), plotlyOutput("hrsmujeres"), width=6),
                     box( plotlyOutput("parthombres"), plotlyOutput("partmujeres"), width=6 ))
            ),
    tabItem("enut_nac", h2("Análisis de la Brecha de Género a Nivel Nacional", align="center"), br(),
            h4("En las regresiones cuantiles se emplearon como variables explicativas la edad, la edad al cuadrado, las horas de estudio, las horas de trabajo remunerado, las horas de trabajo de cuidado, la escolaridad y dummies por vivir en una comunidad rural, por ser soltero, por ser mujer, por ser indígena y por ser mujer indígena"),
            fluidRow(box(radioButtons("enutcoef", label=h4(strong("Coeficiente Estimado de la Regresión Cuantil")), choices=list("Mujer"=1, "Indígena"=2, "Mujer Indígena"=3, "Soltero"=4), selected=1), plotlyOutput("enutregcuantil"), width=6),
                     box("Feminismo", blockQuote(HTML("<em>La realidad es que si nosotras no hacemos nada, pasarán 75 o 100 años antes de que las mujeres puedan esperar ser pagadas de la misma manera que los hombres</em><br><b>Emma Watson</b> ")), width=5),
                     box("Trabajo No Remunerado", blockQuote(HTML("<em>El término 'trabajo no remunerado por el mercado' se refiere a las contribuciones directas de miembros de la familia no remunerados al trabajo de mercado que oficialmente le pertenece a otro miembro de la familia</em><br><b>L. Phillips</b>")), width=5),
                     box("Declaración de los Derechos de la Mujer", blockQuote(HTML("<em> La ignorancia, el olvido o el desprecio de los derechos de la mujer son la únicas causas de los males públicos y de la corrupción de los gobiernos.</em><br><b>Olympe de Gouges</b>")), width=5),
                     box(selectInput("pactividad", label=h4(strong("Brecha por grupos")), choices=list("Log del ingreso por hora"=1, "Hrs de Trabajo Total"=2, "Hrs de Trabajo Remunerado"=3, "Hrs de Trabajo No Remunerado"=4, "Hrs de Producción de Autoconsumo"=5, "Hrs de Trabajo Doméstico"=6, "Hrs de Trabajo de Cuidado"=7, "% de Participación en Trabajo Remunerado"=8, 
                                                                                                      "% de Participación en Trabajo Doméstico"=9, "% de Participación en Trabajos de Cuidado"=10), selected=2),
                         plotlyOutput("pdem") , width=12))
            )
  )
)
      

## 4. Juntar ui --------------------
ui <- dashboardPage(header, siderbar, body, skin="purple" )   

