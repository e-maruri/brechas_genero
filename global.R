# EMMA
library(tidyverse)
library(sjlabelled)

library(haven)
library(plotly)
library(shiny)
library(shinydashboard)
library(dplyr) #collapse, crear nuevas variables
library(zoo)
library(ggplot2)
library(gridExtra)
library(grid)
library(statar) #permite usar comandos de stata
library(viridis)
library(maps)
library(forcats)
library(tidyr)
library(IRdisplay)
library(shinythemes)
library(highcharter)
library( directlabels)
library(ggmap)
library(maptools)
library(rgdal)
library(mapproj)
library(rgeos)
library(RColorBrewer)
library(sf)
library(leaflet)
library(rsconnect)
library(stringr)
library(withr)
library(treemap)
library(DT)
library(shinyBS)
library(shinyjs)
library(WDI)
library(geosphere)
library(magrittr)
library(shinycssloaders)
options(spinner.color="#006272")
library(timevis)
library(shinyWidgets)
library(shinydashboardPlus)
library(scales)
options(scipen=999) #desactiva notaciÃÂ³n cientÃ­fica



# COMUNES -----------------------------------------------------------------
mapa_est <- st_read("www/sin_islas.shp")

# ENUT --------------------------------------------------------------------
brechas<-read_dta("www/enut_brechas.dta")
qreg<-read_dta("www/enut_qreg.dta")
estados<-read_dta("www/enut_estados.dta")
nacional<-read_dta("www/enut_nac14.dta")


dist_hom<-cbind(estados$entidad, round(estados$hom_rem, 4), round(estados$hom_norem,4), round(estados$hom_prod,4))
dist_muj<-cbind(estados$entidad, round(estados$mujer_rem,4), round(estados$mujer_norem,4), round(estados$mujer_prod,4))
mapa2<-cbind(mapa_est, estados$br_linghr, estados$br_ttt2, estados$br_rem2, estados$br_norem2, estados$br_prod2, 
             estados$br_domestico2, estados$br_cuidado2, estados$br_comun2, estados$br_alim2, estados$br_limpviv2, estados$br_limprop2, 
             estados$br_rep2, estados$br_cui52, estados$br_cui142, estados$br_cui602, estados$br_estudio2, estados$br_parrem,
             estados$br_parnorem, estados$br_parprod, estados$br_pardomestico, estados$br_parcuidado, estados$br_parcomun, estados$br_paralim,
             estados$br_parlimpviv, estados$br_parlimprop, estados$br_parrep, estados$br_parcui5, estados$br_parcui14, estados$br_parcui60, estados$br_parestudio, estados$entidad)

nac_hom<-c("Nacional", 68.6507, 27.9365, 3.1507)
nac_muj<-c("Nacional", 30.8917, 66.5605, 2.5031)
dist_hom<-rbind(nac_hom, dist_hom)
dist_muj<-rbind(nac_muj, dist_muj)
nom<-1:33
rownames(dist_hom)<-nom
rownames(dist_muj)<-nom
colnames(dist_hom)<-c("Estado", "% Dedicado al Trabajo Remunerado", "% Dedicado al Trabajo No Remunerado", "% Dedicado a la Producción de bienes para el hogar")
colnames(dist_muj)<-c("Estado", "% Dedicado al Trabajo Remunerado", "% Dedicado al Trabajo No Remunerado", "% Dedicado a la Producción de bienes para el hogar")
piedata<-data.frame(trabajo=c("Trabajo Remunerado", "Trabajo No Remunerado", "Producción de Autoconsumo"),
                    noind=c(48.3, 49.3, 2.4), ind=c(40.5, 51.0, 8.5), total=c(47.9, 49.4, 2.7))


# ENOE  -------------------------------------------------------------------
# INTRO
enoe_brecha_sal_anual_barras <- read_dta("www/enoe_brecha_sal_anual_barras.dta")

enoe_brecha_sal_anual_barras <- enoe_brecha_sal_anual_barras %>% 
  mutate(sex = case_when(
    sex == 1 ~ "Hombre", 
    sex == 2 ~ "Mujer"
  ))


# SALARIOS ----------------------------------------------------------------
enoe_brecha_sal_anual <- read_dta("www/enoe_brecha_sal_anual.dta")

# SECTOR & NIVEL EDUC
enoe_brecha_sal_anual_cruzada <- read_dta("www/enoe_brecha_sal_anual_sec_educ_cruzada.dta")

enoe_brecha_sal_anual_cruzada <- enoe_brecha_sal_anual_cruzada %>% 
  mutate(sector = case_when(
    sector == 1 ~ "Primario", 
    sector == 2 ~ "Secundario", 
    sector == 3 ~ "Terciario"
  ), 
  nivel_educ = case_when(
    nivel_educ == 1 ~ "Menor a primaria", 
    nivel_educ == 2 ~ "Primaria", 
    nivel_educ == 3 ~ "Secundaria",
    nivel_educ == 4 ~ "Preparatoria",
    nivel_educ == 5 ~ "Universidad"
  ))

# SECTOR
enoe_brecha_sal_anual_sector <- read_dta("www/enoe_brecha_sal_anual_sector.dta")

enoe_brecha_sal_anual_sector <- enoe_brecha_sal_anual_sector %>% 
  mutate(sector = case_when(
    sector == 1 ~ "Primario", 
    sector == 2 ~ "Secundario", 
    sector == 3 ~ "Terciario"
  ))

# NIVEL EDUCATIVO 
enoe_brecha_sal_anual_nivel_educ <- read_dta("www/enoe_brecha_sal_anual_nivel_educ.dta")

enoe_brecha_sal_anual_nivel_educ <- enoe_brecha_sal_anual_nivel_educ %>% 
  mutate(nivel_educ = case_when(
    nivel_educ == 1 ~ "Menor a primaria", 
    nivel_educ == 2 ~ "Primaria", 
    nivel_educ == 3 ~ "Secundaria",
    nivel_educ == 4 ~ "Preparatoria",
    nivel_educ == 5 ~ "Universidad"
  ))

# FORMALES E INFORMALES 
enoe_brecha_sal_anual_informal <- read_dta("www/enoe_brecha_sal_anual_informal.dta")

enoe_brecha_sal_anual_informal <- enoe_brecha_sal_anual_informal %>% 
  mutate(emp_ppal = case_when(
    emp_ppal == 1 ~ "Empleo informal", 
    emp_ppal == 2 ~ "Empleo formal"
  ))

# BRECHA POR DECILES 
enoe_brecha_deciles <- read_dta("www/enoe_brecha_cuantiles_10.dta")

enoe_brecha_deciles <- enoe_brecha_deciles %>% 
  mutate(year = as.factor(year))

# ESTATAL 
enoe_brecha_sal_anual_edos <- read_dta("www/enoe_brecha_sal_anual_edos.dta")

enoe_brecha_sal_anual_edos <- enoe_brecha_sal_anual_edos %>% 
  mutate(wgap_ixh = wgap_ixh*100, 
         wgap_ing = wgap_ing*100) %>% 
  rename(CVE_EDO = ent)

# EMPLEO ------------------------------------------------------------------

enoe_empleo <- read_dta("www/enoe_empleo_agregado.dta")

enoe_empleo <- enoe_empleo %>%  
  mutate(sex = case_when(
    sex == 1 ~ "Hombre", 
    sex == 2 ~ "Mujer"), 
    date = paste(year, "-", "0", q, sep=""), 
    date = as.yearqtr(date)
  )

enoe_empleo_gpos <- read_dta("www/enoe_empleo_econ_educ.dta")

enoe_empleo_gpos <- enoe_empleo_gpos %>%  
  filter(q == 1) %>% 
  mutate(sex = case_when(
    sex == 1 ~ "Hombre", 
    sex == 2 ~ "Mujer"),
    
    e_con = case_when(
      e_con == 1 ~ "Unión libre", 
      e_con == 2 ~ "Separado(a)", 
      e_con == 3 ~ "Divorciado(a)", 
      e_con == 4 ~ "Viudo(a)", 
      e_con == 5 ~ "Casado(a)", 
      e_con == 6 ~ "Soltero(a)"), 
  )


# ESTADO CONYUGAL 
enoe_empleo_econ <- read_dta("www/enoe_empleo_econ.dta")

enoe_empleo_econ <- enoe_empleo_econ %>%
  mutate(sex = case_when(
    sex == 1 ~ "Hombre", 
    sex == 2 ~ "Mujer"),
    
    e_con = case_when(
      e_con == 1 ~ "Unión libre", 
      e_con == 2 ~ "Separado(a)", 
      e_con == 3 ~ "Divorciado(a)", 
      e_con == 4 ~ "Viudo(a)", 
      e_con == 5 ~ "Casado(a)", 
      e_con == 6 ~ "Soltero(a)") 
  )

enoe_empleo_econ %>%
  filter(year == 2020 & q == 1) %>%
  plot_ly() %>%
  add_trace(x = ~e_con, y = ~desempleo, type = 'bar', color=~sex) %>%
  layout(#title = paste(get_label(enoe_empleo[, "desempleo"])),
    xaxis = list(title = ""),
    yaxis = list(title = "", ticksuffix = "%"))

# NIVEL EDUCATIVO
enoe_empleo_educ <- read_dta("www/enoe_empleo_educ.dta")

enoe_empleo_educ <- enoe_empleo_educ %>%
  mutate(sex = case_when(
    sex == 1 ~ "Hombre", 
    sex == 2 ~ "Mujer"),
    
    nivel_educ = case_when(
      nivel_educ == 1 ~ "Menor a primaria", 
      nivel_educ == 2 ~ "Primaria", 
      nivel_educ == 3 ~ "Secundaria", 
      nivel_educ == 4 ~ "Preparatoria", 
      nivel_educ == 5 ~ "Superior") 
  )

xform <- list(title = "",
              categoryorder = "array",
              categoryarray = c("Menor a primaria", "Primaria",
                                "Secundaria", "Preparatoria", "Superior"))

# ESTATAL 
enoe_empleo_edos <- read_dta("www/enoe_empleo_edos.dta")

enoe_empleo_edos <- enoe_empleo_edos %>%
  mutate(sex = case_when(
    sex == 1 ~ "Hombre",
    sex == 2 ~ "Mujer")) %>%
  rename(CVE_EDO = ent)

