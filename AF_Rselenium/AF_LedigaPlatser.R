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
ValdRegion_df <- RegionKommunMatris(Lanskod)

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

# Nyamnälda platser
retry(fplatser_valj_rapport(1))

# Om det syns ett diagram och inte en tabell - klicka på rutan "Visa som: tabell"
retry(fplatser_valj_tabell())

# Spara nyanmälda platser i riket
dfriket_nyanm_platser <- fplatser_skapa_tab()

# Skapa tabell för alla län
dfallalan_nyanm_platser <- fplatser_extr_data_lan()

# Välj län (välj län som är förvalt)
retry(fplatser_valj_lan(lannr_meny))

# ladda hem nyanm platser per kommun
dfkom_nyanm_platser <- fplatser_extr_data_kommuner(ant_kommuner)

# Avvälj län
retry(fplatser_avvalj_lan())


dfnyanm_platser <- bind_rows(dfallalan_nyanm_platser,
                             dfriket_nyanm_platser,
                             dfkom_nyanm_platser) %>%
    separate(region,
             into = c("region_kod", "region"), sep = "\\s", extra = "merge") %>%
    pivot_longer(cols = 2:4, names_to = "ar", values_to = "antal") %>%
    filter(!is.na(antal))


################ Skriv dataframe till Excelfilen

# Sökvägen på högskoledatorn
sokvag1 <- "C:\\Users\\pmo\\OneDrive - Högskolan Dalarna\\Auto AF\\Uttag\\AF LedigaPlatser uttag.xlsx"
# Sökvägen på hemmadatorn
sokvag2 <- "C:\\Users\\Administratör\\OneDrive - Region Dalarna\\Auto AF\\Uttag\\AF LedigaPlatser uttag.xlsx"

# Testa om sökväg på högskoledatorn finns (filen måste finnas), om inte så används
# sökväg för hemmadatorn
if (file.exists(sokvag1)) sokvag <- sokvag1 else sokvag <- sokvag2


writexl::write_xlsx(list(LedigaPlatser = dfnyanm_platser),
                    path = sokvag)
    