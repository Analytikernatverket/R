# Välj län genom att ange lanskoden för det län du vill göra uttag för
Lanskod = 20

# För att mäta hur lång tid körningen tar
StartTid <- Sys.time()

#  Laddar in nödvändiga packages ----------------------------------------------

library(tidyverse)
library(RSelenium)
library(rvest)
library(stringr)

# Ladda funktioner för att navigera på AF:s statistiksida och ladda hem data
source("funktioner_veckostat.R", encoding = "UTF-8")

retry(RegList <- RegionLista())
#Sys.sleep(1)
retry(ValdRegion_df <- RegionKommunMatris(Lanskod))

# Ange vilket län som uttaget avses. Värdet tas från valet som är gjort
# på rad 2
lannr_meny = ValdRegion_df[1,5]

# Lägg antalet kommuner i vald region i varibeln ant_kommuner - värdet hämtas
# från val av region på rad 2 - alla kommuner tas med
ant_kommuner = nrow(ValdRegion_df)

# Ange slutvecka -------------------------------------------------------------------------------------
# Data laddas alltid hem från vecka 1 varje år till den vecka som anges i
# variabeln "veckonr". 1 rad i de returnerade resultaten innehåller text (rad
# 20) vill man ha resultat för 52 veckor måste därför antal veckor anges till
# 53. Rekommendationen är att alltid ladda hem all data till sista veckan på
# året, dvs att låta veckonr vara lika med 53

veckonr <- 54 # Låt stå kvar!


# Anslut till AFs QlikView-server -----------------------------------------------------------------

fanslut_till_server <- function() {
    remDr <<- remoteDriver$new(
        remoteServerAddr = "localhost",
        port = 4444,
        browserName = "firefox"
    )

# Kolla så att chromeversionen är rätt under Chrome -> Hjälp

    rd <<- rsDriver(port = 4567L,
                    chromever = "87.0.4280.20")

}


# Stoppa session och frigör portar och gör nytt anslutningsförsök
fstoppa_session_anslut <- function() {
    system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
    gc()
    Sys.sleep(1)
    fanslut_till_server()
}

# Fel vid anslutningen till servern beror nästan alltid på att man startat om och
# att porten därför är upptagen från en tidigare session. Startar man om java
# frigörs porten och det går att starta en ny session
tryCatch(fanslut_till_server(),
         error=function(e) fstoppa_session_anslut())


remDr <- rd[["client"]]

url <- "http://qvs12ext.ams.se/QvAJAXZfc/opendoc.htm?document=extern%5Cvstatplus_extern.qvw&host=QVS%40w001765&anonymous=true%20&select=StartTrigger,1"

#Felhantering - fungerar? vet ej
remDr$setTimeout(type = "page load", milliseconds = 10000)

remDr$navigate(url)

# Lång paus för att sidan ska hinna laddas
Sys.sleep(4)


################# Här har sidan laddats in och inhämtning av data börjar

# Klicka på sökande-knappen
retry(fplatser_byt_till_sokande())

# Välj öppent arbetslösa och sökande i program i rapportmenyn (item nr 3)
# Långtidsarbetslösa - dvs. arbetslösa längre än 6 månader (item nr 4)
# Endast öppet arbetslösa (item nr 8)
retry(fsokande_valj_rapport(4))

# Klicka på tabell-knappen
retry(fsokande_valj_tabell())

############# Hämta ner all data för hela riket #############################

# Välj kön
retry(fsokande_valj_kon())

#Ladda hem tabell för riket fördelat på kön
dfriket_langalosa1664_kon <- fsokande_skapa_tab(kon = TRUE)

# Ladda hem data för arbetslösa i alla län 16-64 år fördelat på kön
dflangalosa1664_alla_lan_kon <- fsokande_extr_data_lan(kon = TRUE)

#Avvälj kön
retry(fsokande_avvalj_kon())

# Ladda hem data för arbetslösa i riket i åldern 16-24
retry(fsokande_valj_alder(1))
dfriket_langalosa1624 <- fsokande_skapa_tab(kon = FALSE)

# Ladda hem data för arbetslösa i riket i åldern 60- år
retry(fsokande_valj_alder(6))
dfriket_langalosa60 <- fsokande_skapa_tab(kon = FALSE)

# Se till så att ingen ålder är vald
retry(fsokande_avvalj_alder())

# Välj födda i Sverige
retry(fsokande_valj_fodelseland(1))
dfriket_langalosa1664_sv <- fsokande_skapa_tab(kon = FALSE)

# Välj födda i övriga Europa
retry(fsokande_valj_fodelseland(2))
dfriket_langalosa1664_ovr_eur <- fsokande_skapa_tab(kon = FALSE)

# Välj födda i övriga Europa
retry(fsokande_valj_fodelseland(3))
dfriket_langalosa1664_ovr_varlden <- fsokande_skapa_tab(kon = FALSE)

# Avvälj födelseland
retry(fsokande_avvalj_fodelseland())


# Välj funktionshindrade
retry(fsokande_valj_funk())

# Ladda hem data för funktionshindrade arbetslösa i riket
dfriket_langalosa_funk <- fsokande_skapa_tab(kon = FALSE)

# Välj bort funktionshindrade
retry(fsokande_avvalj_funk())


############################# Ladda hem data för län #####################

# Välj aktuellt län i dropdown-menyn för län (item nr 10)
retry(fsokande_valj_lan(lannr_meny))

# Klicka på rutan för könsfördelad statistik
retry(fsokande_valj_kon())

# Ladda hem data för regionens alla kommuner för kön. Tar tid!
dfregion_kom_langalosa1664_kon <- fsokande_extr_data_kommuner(ant_kommuner, kon = TRUE)

# Avmarkera kön
retry(fsokande_avvalj_kon())

################# Funktionshindrade i länet och länets kommuner

# Välj aktuellt län i dropdown-menyn för län (item nr 10)
retry(fsokande_valj_lan(lannr_meny))

# Välj funktionshindrade
retry(fsokande_valj_funk())

# Ladda hem tabell för funktionshindrade i länet
dfregion_langalosa_funk <- fsokande_skapa_tab(kon = FALSE)

# Ladda hem tabell för funktionshindrade i länets kommuner 
dfregion_kom_langalosa_funk <- fsokande_extr_data_kommuner(ant_kommuner, kon = FALSE)

# Välj bort funktionshindrade
retry(fsokande_avvalj_funk())


############################# Unga i länet och länets kommuner

# Välj aktuellt län i dropdown-menyn för län (item nr 10)
retry(fsokande_valj_lan(lannr_meny))

# Byt ålder till 16-24 år (item nr 1 i Ålder-menyn)
retry(fsokande_valj_alder(1))

# Ladda hem data för arbetslösa i åldern 16-24 år
dfregion_langarlosa1624 <- fsokande_skapa_tab(kon = FALSE)

# Ladda hem data för regionens alla kommuner i åldern 16-24. Tar tid!
dfregion_kom_langalosa1624 <- fsokande_extr_data_kommuner(ant_kommuner, kon = FALSE)

# Avvälj ålder
retry(fsokande_avvalj_alder())

############################# Äldre i länet och länets kommuner

# Välj aktuellt län i dropdown-menyn för län (item nr 10)
retry(fsokande_valj_lan(lannr_meny))

# Byt ålder till 60- år (item nr 6 i Ålder-menyn)
retry(fsokande_valj_alder(6))

# Ladda hem data för arbetslösa i åldern 60- år
dfregion_langarlosa60 <- fsokande_skapa_tab(kon = FALSE)

# Ladda hem data för regionens alla kommuner i åldern 60- år. Tar tid!
dfregion_kom_langalosa60 <- fsokande_extr_data_kommuner(ant_kommuner, kon = FALSE)

# Avvälj ålder
retry(fsokande_avvalj_alder())

############################## Ladda hem data på födelseland ################

# Välj aktuellt län i dropdown-menyn för län (item nr 10)
retry(fsokande_valj_lan(lannr_meny))

# Avvälj ålder igen, den tycks missa ibland
retry(fsokande_avvalj_alder())

# Välj Sverige som födelseland
retry(fsokande_valj_fodelseland(1))

# Ladda hem data för regionen för personer födda i Sv. fsokande_skapa_tab

dfregion_langalosa1664_sv <- fsokande_skapa_tab(kon = FALSE)

# Välj födda i övriga Europa
retry(fsokande_valj_fodelseland(2))

# Ladda hem data för födda i övriga Europa i valt län
dfregion_langalosa1664_ovr_eur <- fsokande_skapa_tab(kon = FALSE)

# Välj födda utanför Europa
retry(fsokande_valj_fodelseland(3))

# Ladda hem data för födda utanför Europa i valt län
dfregion_langalosa1664_ovr_varlden <- fsokande_skapa_tab(kon = FALSE)

# Välj bort födelseland
retry(fsokande_avvalj_fodelseland())

# Stäng webbläsaren
remDr$closeWindow()

##### Lägg ihop Öppet arbetslösa fördelat på kön i en dataframe 

# Kontrollera att inga oönskade kolumner kommer med
dfriket_langalosa1664_kon <- dfriket_langalosa1664_kon[, !names(dfriket_langalosa1664_kon) %in% c("etab","fland")] 
dflangalosa1664_alla_lan_kon <- dflangalosa1664_alla_lan_kon [, !names(dflangalosa1664_alla_lan_kon ) %in% c("etab","fland")]
dfregion_kom_langalosa1664_kon <- dfregion_kom_langalosa1664_kon [, !names(dfregion_kom_langalosa1664_kon ) %in% c("etab","fland")]

dflangalosa1664_kon <- bind_rows(dfriket_langalosa1664_kon,
                                    dflangalosa1664_alla_lan_kon,
                                    dfregion_kom_langalosa1664_kon) %>%    
    separate(region,
             into = c("region_kod", "region"), sep = "\\s", extra = "merge") %>%
    pivot_longer(cols = 2:7, names_to = "kon", values_to = "antal") %>%
    filter(!is.na(antal)) %>%
    separate(kon,
             into = c("kon", "ar"), sep = " ", extra = "merge") %>%
    mutate(kon = str_replace(kon, "kvinor", "kvinnor")) %>%
    mutate(kon = str_replace(kon, "man", "män"))

#Säkerställ att inte oönskade kolumner kommer med
dfriket_langalosa1624 <- dfriket_langalosa1624[, !names(dfriket_langalosa1624) %in% c("etab","fland")] 
dfregion_kom_langalosa1624 <- dfregion_kom_langalosa1624 [, !names(dfregion_kom_langalosa1624 ) %in% c("etab","fland")]
dfregion_langarlosa1624 <- dfregion_langarlosa1624 [, !names(dfregion_langarlosa1624 ) %in% c("etab","fland")]
dfregion_kom_langalosa60 <- dfregion_kom_langalosa60 [, !names(dfregion_kom_langalosa60 ) %in% c("etab","fland")]
dfregion_langarlosa60 <- dfregion_langarlosa60 [, !names(dfregion_langarlosa60 ) %in% c("etab","fland")]
dfriket_langalosa60 <- dfriket_langalosa60 [, !names(dfriket_langalosa60 ) %in% c("etab","fland")]


##### Lägg ihop Öppet arbetslösa unga i en dataframe 
dflangalosa_alder <- bind_rows(dfriket_langalosa1624,
                         dfregion_kom_langalosa1624,
                         dfregion_langarlosa1624,
                         dfregion_kom_langalosa60,
                         dfregion_langarlosa60,
                         dfriket_langalosa60) %>%
    separate(region,
             into = c("region_kod", "region"), sep = "\\s", extra = "merge") %>%
    pivot_longer(cols = 2:4, names_to = "ar", values_to = "antal") %>%
    filter(!is.na(antal))

#Säkerställ att inte oönskade kolumner kommer med
dfregion_langalosa1664_sv <- dfregion_langalosa1664_sv[, !names(dfregion_langalosa1664_sv) %in% c("etab")] 
dfregion_langalosa1664_ovr_eur <- dfregion_langalosa1664_ovr_eur [, !names(dfregion_langalosa1664_ovr_eur ) %in% c("etab")]
dfregion_langalosa1664_ovr_varlden <- dfregion_langalosa1664_ovr_varlden [, !names(dfregion_langalosa1664_ovr_varlden ) %in% c("etab")]
dfriket_langalosa1664_sv <- dfriket_langalosa1664_sv [, !names(dfriket_langalosa1664_sv ) %in% c("etab")]
dfriket_langalosa1664_ovr_eur <- dfriket_langalosa1664_ovr_eur [, !names(dfriket_langalosa1664_ovr_eur ) %in% c("etab")]
dfriket_langalosa1664_ovr_varlden <- dfriket_langalosa1664_ovr_varlden [, !names(dfriket_langalosa1664_ovr_varlden ) %in% c("etab")]

##### Lägg ihop Öppet arbetslösa fördelat på födelseland i en dataframe 
dflangalosa_fland <- bind_rows(dfregion_langalosa1664_sv,
                           dfregion_langalosa1664_ovr_eur,
                           dfregion_langalosa1664_ovr_varlden,
                           dfriket_langalosa1664_sv,
                           dfriket_langalosa1664_ovr_eur,
                           dfriket_langalosa1664_ovr_varlden) %>%
    separate(region,
             into = c("region_kod", "region"), sep = "\\s", extra = "merge") %>%
    pivot_longer(cols = 2:4, names_to = "ar", values_to = "antal") %>%
    filter(!is.na(antal))

#Säkerställ att inte oönskade kolumner kommer med
dfriket_langalosa_funk <- dfriket_langalosa_funk[, !names(dfriket_langalosa_funk) %in% c("etab","fland")] 
dfregion_kom_langalosa_funk <- dfregion_kom_langalosa_funk [, !names(dfregion_kom_langalosa_funk ) %in% c("etab","fland")]
dfregion_langalosa_funk <- dfregion_langalosa_funk [, !names(dfregion_langalosa_funk ) %in% c("etab","fland")]

dflangalosa_funk <- bind_rows(dfriket_langalosa_funk,
                         dfregion_kom_langalosa_funk,
                         dfregion_langalosa_funk) %>%
    separate(region,
             into = c("region_kod", "region"), sep = "\\s", extra = "merge") %>%
    pivot_longer(cols = 2:4, names_to = "ar", values_to = "antal") %>%
    filter(!is.na(antal))

# Sökvägen på högskoledatorn
sokvag1 <- "C:\\Users\\pmo\\OneDrive - Högskolan Dalarna\\Auto AF\\Uttag\\AF LangArblosa uttag.xlsx"
# Sökvägen på hemmadatorn
sokvag2 <- "C:\\Users\\Administratör\\OneDrive - Region Dalarna\\Auto AF\\Uttag\\AF LangArblosa uttag.xlsx"

# Testa om sökväg på högskoledatorn finns (filen måste finnas), om inte så används
# sökväg för hemmadatorn
if (file.exists(sokvag1)) sokvag <- sokvag1 else sokvag <- sokvag2

writexl::write_xlsx(list(langalosa_alder = dflangalosa_alder,
                         langalosa_fland = dflangalosa_fland,
                         langalosa1664_kon = dflangalosa1664_kon,
                         langalosa_funk = dflangalosa_funk),
                    path = sokvag)

print(paste0("Datainsamlingen tog totalt",TidFormaterat(Sys.time(),StartTid)))