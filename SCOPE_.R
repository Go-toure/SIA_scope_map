setwd("C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/")
# install.packages("ggspatial")
# install.packages("geojsonsf")
library(pacman)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)
library(lubridate)

#map layout
  
  block<- c("ALGERIA","BURKINA FASO","COTE D IVOIRE","MAURITANIA","MALI","GUINEA","GHANA","TOGO","BENIN","SIERRA LEONE","LIBERIA","GUINEA-BISSAU","THE GAMBIA","SENEGAL","NIGERIA","NIGER","CAMEROON","CHAD","CENTRAL AFRICAN REPUBLIC","DEMOCRATIC REPUBLIC OF THE CONGO","CONGO","GABON","EQUATORIAL GUINEA","RWANDA","BURUNDI ","ANGOLA","KENYA","ERITREA","ETHIOPIA","SOUTH SUDAN","UGANDA","TANZANIA","MALAWI","ZAMBIA","MOZAMBIQUE","ZIMBABWE","BOTSWANA","NAMIBIA","ESWATINI (KINGDOM)","LESOTHO(KINGDOM","SOUTH AFRICA","MADAGASCAR","COMOROS","SEYCHELLES","MAURITIUS","CAPE VERDE","SAO TOME AND PRINCIPE","UNITED REPUBLIC OF TANZANIA")
  
  pays<- c("ALGERIA","BURKINA FASO","COTE D IVOIRE","MAURITANIA","MALI","GUINEA","GHANA","TOGO","BENIN","SIERRA LEONE","LIBERIA","GUINEA-BISSAU","THE GAMBIA","SENEGAL","NIGERIA","NIGER","CAMEROON","CHAD","CENTRAL AFRICAN REPUBLIC","DEMOCRATIC REPUBLIC OF THE CONGO","CONGO","GABON","EQUATORIAL GUINEA","RWANDA","BURUNDI ","ANGOLA","KENYA","ERITREA","ETHIOPIA","SOUTH SUDAN","UGANDA","TANZANIA","MALAWI","ZAMBIA","MOZAMBIQUE","ZIMBABWE","BOTSWANA","NAMIBIA","ESWATINI (KINGDOM)","LESOTHO(KINGDOM","SOUTH AFRICA","MADAGASCAR","COMOROS","SEYCHELLES","MAURITIUS","CAPE VERDE","SAO TOME AND PRINCIPE","UNITED REPUBLIC OF TANZANIA")
  
  
  data <- read_csv("C:/Users/TOURE/Mes documents/REPOSITORIES/SCOPE/scope.csv")
  c<-data |>
    mutate(month = month(round_start_date)) |>
    filter(month != "NA") |> 
    arrange(month)|>
    mutate(district = toupper(district)) |> 
    mutate(district = case_when(
      country	=="ANGOLA"&	district	=="N'HARÃŠA" ~	"NHAREA",
      country	=="ANGOLA"&	district	=="NGONGUEMBO" ~	"GONGUEMBO",
      country	=="ANGOLA"&	district	=="NÃ“QUI" ~	"NOQUI",
      country	=="ANGOLA"&	district	=="PANGO-ALUQUEM" ~	"PANGO ALUQUEM",
      country	=="ANGOLA"&	district	=="TÃ”MBUA (EX. PORTO ALEXANDRE)" ~	"TOMBUA",
      country	=="ANGOLA"&	district	=="UCUMA" ~	"UKUMA",
      country	=="ANGOLA"&	district	=="UÃGE" ~	"UIGE",
      country	=="ANGOLA"&	district	=="XÃ-MUTEBA" ~	"XA MUTEBA",
      country	=="ANGOLA"&	district	=="NZETU" ~	"NZETO",
      country	=="ANGOLA"&	district	=="CELA (EX. UACU-CUNGO)" ~	"CELA",
      country	=="ANGOLA"&	district	=="OMBADJA (EX. CUAMATO)" ~	"OMBADJA",
      country	=="ANGOLA"&	district	=="TCHICALA TCHOLOHANGA" ~	"TCHIKALA-TCHOLOHAN",
      country	=="ANGOLA"&	district	=="BUNDAS" ~	"LUMBALA NGUIMBO (BUNDAS)",
      country	=="ANGOLA"&	district	=="AMBOIM (EX. GABELA)" ~	"AMBOIM",
      country	=="ANGOLA"&	district	=="AMBUÃLA" ~	"AMBUILA",
      country	=="ANGOLA"&	district	=="BAÃA FARTA" ~	"BAIA FARTA",
      country	=="ANGOLA"&	district	=="BUENGAS (EX. NOVA ESPERANÃ‡A)" ~	"BUENGAS",
      country	=="ANGOLA"&	district	=="BULA-ATUMBA" ~	"BULA ATUMBA",
      country	=="ANGOLA"&	district	=="QUIUABA-N'ZOGI" ~	"KIWABA NZOGI",
      country	=="ANGOLA"&	district	=="SAMBA CAJÃš" ~	"SAMBA CAJU",
      country	=="ANGOLA"&	district	=="SELES (EX. UCU SELES)" ~	"SELES",
      country	=="ANGOLA"&	district	=="SUMBE (EX. NGUNZA)" ~	"SUMBE",
      country	=="ANGOLA"&	district	=="CAMEIA" ~	"LUMEJE (CAMEIA)",
      country	=="ANGOLA"&	district	=="CATABOLA (EX. NOVA SINTRA)" ~	"CATABOLA",
      country	=="ANGOLA"&	district	=="LÃ‰UA" ~	"LEUA",
      country	=="ANGOLA"&	district	=="LIBOLO (EX. CALULO)" ~	"LIBOLO",
      country	=="ANGOLA"&	district	=="LÃ“VUA" ~	"LOVUA",
      country	=="ANGOLA"&	district	=="BUNDAS-LUMBALA-NGUIMBO" ~	"LUMBALA NGUIMBO (BUNDAS)",
      country	=="ANGOLA"&	district	=="CAÃLA" ~	"CAALA",
      country	=="ANGOLA"&	district	=="CACONGO (EX. LÃ‚NDANA)" ~	"CACONGO",
      country	=="ANGOLA"&	district	=="DANDE (CAXITO)" ~	"DANDE",
      country	=="ANGOLA"&	district	=="DEMBOS-QUIBAXE" ~	"DEMBOS (QUIBAXE)",
      country	=="ANGOLA"&	district	=="GAMBOS (EX. CHIANGE)" ~	"GAMBOS",
      country	=="ANGOLA"&	district	=="CUNDA-DIA-BAZE" ~	"KUNDA-DIA-BAZE",
      country	=="ANGOLA"&	district	=="CUNHINGA (VOUGA)" ~	"CUNHINGA",
      country	=="ANGOLA"&	district	=="MUCABA (EX. QUINZALA)" ~	"MUCABA",
      country	=="ANGOLA"&	district	=="MUCARI" ~	"CACULAMA (MUCARI)",
      country	=="ANGOLA"&	district	=="TCHIKALA TCHOLOHANG" ~	"TCHIKALA-TCHOLOHAN",
      country	=="ANGOLA"&	district	=="CUROCA (EX. ONCOCUA)" ~	"CUROCA",
      country	=="ANGOLA"&	district	=="MILUNGA (SANTA CRUZ)" ~	"MILUNGA",
      country	=="ANGOLA"&	district	=="LUENA" ~	"MOXICO (LUENA)",
      country	=="NIGER"&	district	=="AGUIÃ‰" ~	"AGUIÉ",
      country	=="NIGE"&	district	=="TCHIROZÃ‰RINE" ~	"TCHIROZÉRINE",
      country	=="NIGER"&	district =="TÃ‰RA" ~	"TERA",
      country	=="NIGER"&	district	=="GOURÃ‰" ~	"GOURÉ",
      country	=="NIGER"&	district	=="IFÃ‰ROUANE" ~	"IFÉROUANE",
      country	=="NIGER"&	district	=="ILLÃ‰LA" ~	"ILLÉLA",
      country	=="NIGER"&	district	=="MATAMÃˆYE" ~	"MATAMEYE",
      country	=="NIGER"&	district	=="FILINGUÃ‰" ~	"FILINGUE",
      country	=="NIGER"&	district	=="KANTCHÃ‰" ~	"KANTCHÉ",
      country	=="NIGER"&	district	=="GOTHÃˆYE" ~	"GOTHÈYE",
      country	=="NIGER"&	district	=="JINJA CITY‰" ~	"JINJA",
      country	=="NIGER"&	district	=="MBALE CITY" ~	"MBALE",
      country	=="MAURITANIA"&	district	=="RIYADH" ~	"RIYAD",
      country	=="MAURITANIA"&	district	=="LEKSEIBE" ~	"LEXEIBA",
      country	=="MAURITANIA"&	district	=="WOMPOU" ~	"WOMPO",
      country	=="MAURITANIA"&	district	=="ADEL BAGHROU" ~	"ADEL BEGROU",
      country	=="MAURITANIA"&	district	=="AKJOUJET" ~	"AKJOUJT",
      country	=="MAURITANIA"&	district	=="BABABE" ~	"BABABÉ",
      country	=="MAURITANIA"&	district	=="BIR OUMGREINE" ~	"BIR MOGHREN",
      country	=="MAURITANIA"&	district	=="BOGHE" ~	"BOGHÉ",
      country	=="MAURITANIA"&	district	=="BARKEOL" ~	"BARKÉOLE",
      country	=="MAURITANIA"&	district	=="CHINGUITTI" ~	"CHINGUITTY",
      country	=="MAURITANIA"&	district	=="D_HAR" ~	"D'HAR",
      country	=="MAURITANIA"&	district	=="BOUTILIMITT" ~	"BOUTILIMIT",
      country	=="MAURITANIA"&	district	=="F_DERIK" ~	"F'DERICK",
      country	=="MAURITANIA"&	district	=="GUERROU" ~	"GUÉRROU",
      country	=="MAURITANIA"&	district	=="KANKOUSSA" ~	"KANKOSSA",
      country	=="MAURITANIA"&	district	=="KOBENNI" ~	"KOBENI",
      country	=="MAURITANIA"&	district	=="M_BAGNE" ~	"M'BAGNE",
      country	=="MAURITANIA"&	district	=="M_BOUT" ~	"M'BOUT",
      country	=="MAURITANIA"&	district	=="MAGHTA LEHJAR" ~	"MAGTA LAHJAR",
      country	=="MAURITANIA"&	district	=="MOUDJRIA" ~	"MOUDJÉRIA",
      country	=="MAURITANIA"&	district	=="NEMA" ~	"NÉMA",
      country	=="MAURITANIA"&	district	=="OUAD-NAGA" ~	"OUAD NAGA",
      country	=="MAURITANIA"&	district	=="R_KIZ" ~	"R'KIZ",
      country	=="MAURITANIA"&	district	=="SEILIBABY" ~	"SELIBABY",
      country	=="MAURITANIA"&	district	=="TAMCHEKETT" ~	"TAMCHAKET",
      country	=="MAURITANIA"&	district	=="TEVRAGH ZEINE" ~	"TEVRAGH ZEINA",
      country	=="MAURITANIA"&	district	=="TICHITT" ~	"TICHIT",
      country	=="MAURITANIA"&	district	=="TIMBEDRA" ~	"TIMBÉDRA",
      country	=="MAURITANIA"&	district	=="ZOUERATE" ~	"ZOUÉRAT",
      country	=="MOZAMBIQUE"&	district	=="CHIÃšRE" ~	"CHIÚRE",
      country	=="MOZAMBIQUE"&	district	=="MARÃVIA" ~	"MARÁVIA",
      country	=="MOZAMBIQUE"&	district	=="MAÃšA" ~	"MAUA",
      country	=="MOZAMBIQUE"&	district	=="ALTO MOLÃ“CUÃˆ" ~	"ALTO MOLOCUE",
      country	=="MOZAMBIQUE"&	district	=="ANGÃ“NIA" ~	"ANGONIA",
      country	=="MOZAMBIQUE"&	district	=="MOCÃMBOA DA PRAIA" ~	"MACIMBOA DA PRAI",
      country	=="MOZAMBIQUE"&	district	=="MÃGOÃˆ" ~	"MÁGOÈ",
      country	=="MOZAMBIQUE"&	district	=="GURUÃ‰" ~	"GURUE",
      country	=="MOZAMBIQUE"&	district	=="GILÃ‰" ~	"GILÉ",
      country	=="MOZAMBIQUE"&	district	=="NGAÃšMA" ~	"NGAÚMA",
      TRUE~district))
  
  
  writexl::write_xlsx(c,"C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/SIA_SCOPE_LAST_six_month.xlsx")
  
  d0<- ym("2024-04")
  d1<- ym("2024-05")
  d2<- ym("2024-06")
  d3<- ym("2024-07")
  d4<- ym("2024-08")
  d5<- ym("2024-09")
  d6<- ym("2024-10")
  
  # "2024-06", "2024-07", "2024-08", "2024-09", "2024-10"
  
  plots_list <- list()
  
  for (selected_month_yr in c(5, 6, 7, 8, 9, 10)){
    A<-c |>
      filter(month == selected_month_yr) |> 
      filter(country %in% pays) |>
      select(country, province, district, vaccine.type)
    
    
    
    
    all_countries <- read_rds("global.ctry.rds")
    all_provinces <- read_rds("global.prov.rds")
    all_districts <- read_rds("global.dist.rds")
    
    AFRO_layer<-all_countries |> 
      filter(WHO_REGION == "AFRO")
    Africa<-all_countries |> 
      filter(WORLD_CONTINENTS == "AFRICA")
    
    
    country_layer <-all_countries |>
      filter(ADM0_NAME %in% block)
    province_layer <-all_provinces |>
      filter(ADM0_NAME %in% block)
    district_layer <-all_districts |>
      filter(ADM0_NAME %in% block)
    #join the spatial layer of provinces
    LQAS_performance2 <- left_join(district_layer, A, relationship = "many-to-many",  
                                   by=c("ADM0_NAME" = "country",
                                        "ADM2_NAME" = "district")) |> 
      filter(vaccine.type != "NA")
    
    #plot the result of the analysis, 
    last_6 <- ggplot() + 
      geom_sf(data = Africa, linewidth = 0.5, color = "black", fill = "grey") +
      
      geom_sf(data = province_layer, color = NA, fill = "white") +
      geom_sf(data = LQAS_performance2, aes(fill = vaccine.type), color = NA) +
      # geom_text(data=district_layer, aes(x=CENTER_LON, y=CENTER_LAT, 
      #                                   label=ADM2_NAME, group=NULL), size=2)+
      geom_sf(data = country_layer, linewidth = 0.5, color = "black", fill = NA) +
      scale_fill_manual(values = c("nOPV2" = "cornflowerblue", "bOPV" = "yellow3"))+
      #scale_fill_(low = "red",high = "green")
      theme_void() +
      labs(fill = paste0("vaccine type"), 
           title = paste("scope in", month(selected_month_yr, label = TRUE), year(d0)),
           subtitle = ("")) +
      theme(plot.title = element_text(hjust=0.5), 
            plot.subtitle = element_text(hjust=0.5))
    last_6

    plots_list[[selected_month_yr - 4]] <- last_6
  }
  
 
  
  # Combine the plots
  multiplot <- wrap_plots(plots_list, ncol = 3)
  
  # Add source and production date at the bottom of the last plot
  multiplot_with_caption <- multiplot + labs(caption = paste("Source: AFR0 SIA calendar, Production date:", Sys.Date()))
  
  # Display the plots
  multiplot_with_caption



