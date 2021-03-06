library(futile.logger)
library(utils)

fplatser_byt_till_sokande <- function(obj_btn = "//*[@id='12']/div[2]/table/tbody/tr/td") {

    web_elem <- Wait_For_Load("xpath", obj_btn)
    web_elem$clickElement()
    
    Sys.sleep(1)
    
    #Säkerställ att det går att välja kön innan vi lämnar denna funktion
    web_elem <- Wait_For_Load("xpath","//*[@id='63']/div[3]/div/div[1]/div/div[1]")
    
}

fsokande_valj_lan <- function(intem_nr = 10, obj_menu = "//*[@id='57']/div[2]/div/div[1]/div[5]/div/div[1]/div[1]"
) {
    print("Start: fsokande_valj_lan")
    fardig <- FALSE
    forsok <- 0
      while(fardig==FALSE & forsok < 20){
        # Hämta element som innehåller länsnamn (om något är valt)
        obj_lanNamn <- '//*[@id="57"]/div[2]/div/div[1]/div[5]/div/div[3]'
        webElem <- remDr$findElement(using = "xpath", value = obj_lanNamn)
        lan_namn <- as.character(webElem$getElementAttribute("title"))
        if (lan_namn == RegList[intem_nr,2]) {
          fardig <- TRUE
        } else {
        forsok <- forsok + 1
        web_elem <- Wait_For_Load("xpath", obj_menu)
        web_elem$clickElement()
    
        web_elem <- Wait_For_Load("xpath", paste0("//*[@id='DS']/div/div/div[1]/div[", intem_nr , "]/div[1]"))
        web_elem$clickElement()
    
        Sys.sleep(2)
            
        #Säkerställ att vecka 1 finns laddad på sidan innan vi går vidare
        web_elem <- Wait_For_Load("xpath",obj_menu)
        if (forsok == 5 | forsok == 10) fsokande_avvalj_lan()
      }
    }
    print("Slut: fsokande_valj_lan")
}

fsokande_avvalj_lan <- function() {
  # Hämta element som innehåller länsnamn (om något är valt)
  obj_lanNamn <- '//*[@id="57"]/div[2]/div/div[1]/div[5]/div/div[3]'
  webElem <- remDr$findElement(using = "xpath", value = obj_lanNamn)
  lan_namn <- as.character(webElem$getElementAttribute("title"))
  
  # Gå vidare och klicka ur länet i gröna fältet om det är något
  # län valt
  if (lan_namn != "") {
    # Hämta element som är det gröna fältet med länsnamnet (om det finns)
    obj_valjbortlan <- '//*[@id="57"]/div[2]/div/div[1]/div[5]/div/div[3]/div[1]'
    webElem_valjbort <- Wait_For_Load("xpath", obj_valjbortlan)
    webElem_valjbort$clickElement()
  }

}

fsokande_valj_kommun <- function(intem_nr = 1, obj_menu = "//*[@id='50']/div[2]/div/div[1]/div[5]/div/div[1]/div[1]") {
    fardig <- FALSE
    while(fardig==FALSE){
      
      # Hämta element som innehåller kommunnamn (om något är valt)
      obj_kommNamn <- "//*[@id='50']/div[2]/div/div[1]/div[5]/div/div[3]"
      webElem <- remDr$findElement(using = "xpath", value = obj_kommNamn)
      komm_namn <- as.character(webElem$getElementAttribute("title"))
      # Prova om kommunnamnet är det som det bör vara
      if (komm_namn == ValdRegion_df[intem_nr,4]) {
        fardig <- TRUE
      } else {
      # Om kommun namnet inte är vad det ska vara så öppnar vi kommun-
      # listrutan och väljer rätt kommun 
      web_elem <- Wait_For_Load("xpath", obj_menu)
      web_elem$clickElement()
    
      web_elem <- Wait_For_Load("xpath", paste0("//*[@id='DS']/div/div/div[1]/div[", intem_nr , "]/div[1]"))
      web_elem$clickElement()
      Sys.sleep(2)
  
      #Säkerställ att sidan är laddad innan vi går vidare
      web_elem <- Wait_For_Load("xpath",obj_menu)

      # Kontrollera att vi har rätt län valt
      # Hämta element som innehåller länsnamn (om något är valt)
      obj_lanNamn <- '//*[@id="57"]/div[2]/div/div[1]/div[5]/div/div[3]'
      webElem <- remDr$findElement(using = "xpath", value = obj_lanNamn)
      lan_namn <- as.character(webElem$getElementAttribute("title"))

      # Ibland händer det att något går snett och ett annat län blir valt
      # Vi testar här om vi har rätt län och om inte, så nollställer vi 
      # kontrollen och väljer rätt län igen
      if (lan_namn != ValdRegion_df[intem_nr,2]) fsokande_ValjOm_lan()
      }
    }
}

fsokande_avvalj_kommun <- function() {
  # Hämta element som innehåller länsnamn (om något är valt)
  obj_kommNamn <- "//*[@id='50']/div[2]/div/div[1]/div[5]/div/div[3]"
  webElem <- remDr$findElement(using = "xpath", value = obj_kommNamn)
  komm_namn <- as.character(webElem$getElementAttribute("title"))
  
  # Gå vidare och klicka ur länet i gröna fältet om det är något
  # kommun valt
  if (komm_namn != "") {
    # Hämta element som är det gröna fältet med länsnamnet (om det finns)
    obj_valjbortkomm <- "//*[@id='50']/div[2]/div/div[1]/div[5]/div/div[3]/div[1]"
    webElem_valjbort <- Wait_For_Load("xpath", obj_valjbortkomm)
    webElem_valjbort$clickElement()
  }    
}

fsokande_valj_alder <- function(intem_nr = 1, obj_menu = "//*[@id='58']/div[2]/div/div[1]/div[5]/div/div[1]/div[1]") {
    fardig <- FALSE
    while (fardig==FALSE){
      # Testa om vi har en ålder vald
      obj_str <- '//*[@id="58"]/div[2]/div/div[1]/div[5]/div/div[3]'
      Elem <- remDr$findElement(using = "xpath", value = obj_str)
      elem_namn <- as.character(Elem$getElementAttribute("title"))
      if (elem_namn == fsokande_aldermatris(intem_nr)) {
        fardig <- TRUE
      } else {
        fsokande_avvalj_alder()
              
        #Öppna åldersmenyn
        web_elem <- Wait_For_Load("xpath", obj_menu)
        web_elem$clickElement()
    
        #Välj alternativ i listan i åldersmenyn
        web_elem <- Wait_For_Load("xpath", paste0("//*[@id='DS']/div/div/div[1]/div[", intem_nr , "]/div[1]"))
        web_elem$clickElement()
    
        Sys.sleep(3)
        
        #Säkerställ att vecka 1 finns laddad på sidan innan vi går vidare
        web_elem <- Wait_For_Load("xpath","//*[@id='55']/div[2]/div[1]/div[1]/div[2]/div/div[2]/div[1]")
      }
    }
}

fsokande_avvalj_alder <- function() {
  obj_alder <- '//*[@id="58"]/div[2]/div/div[1]/div[5]/div/div[3]'
  Elem <- remDr$findElement(using = "xpath", value = obj_alder)
  elem_namn <- as.character(Elem$getElementAttribute("title"))
  if (elem_namn != "") {
    # Hämta element som är det gröna fältet med länsnamnet (om det finns)
    obj_valjbort <- '//*[@id="58"]/div[2]/div/div[1]/div[5]/div/div[3]/div[1]'
    webElem_valjbort <- Wait_For_Load("xpath", obj_valjbort)
    webElem_valjbort$clickElement()
  }
}

fsokande_klicka_kon_btn <- function(obj_btn = "//*[@id='63']/div[3]/div/div[1]/div/div[1]") {

    web_elem <- Wait_For_Load("xpath", obj_btn)
    web_elem$clickElement()

    Sys.sleep(1)    
    
    #Säkerställ att vecka 1 finns ladda på sidan innan vi går vidare
    web_elem <- Wait_For_Load("xpath","//*[@id='55']/div[2]/div[1]/div[1]/div[2]/div/div[2]/div[1]")
    
}

fsokande_valj_kon <- function(obj_btn = "//*[@id='63']/div[3]/div/div[1]/div/div[1]") {
  
  while(Kon_valt() == FALSE){
    web_elem <- Wait_For_Load("xpath", obj_btn)
    web_elem$clickElement()
  
  Sys.sleep(1)    
  
  #Säkerställ att vecka 1 finns ladda på sidan innan vi går vidare
  web_elem <- Wait_For_Load("xpath","//*[@id='49']/div[2]/div[1]/div[1]/div[2]/div/div[2]/div[1]")
  }
}

fsokande_avvalj_kon <- function(obj_btn = "//*[@id='63']/div[3]/div/div[1]/div/div[1]") {
  
  while(Kon_valt() == TRUE){
    web_elem <- Wait_For_Load("xpath", obj_btn)
    web_elem$clickElement()
  
  Sys.sleep(1)    
  
  #Säkerställ att vecka 1 finns ladda på sidan innan vi går vidare
  web_elem <- Wait_For_Load("xpath",obj_btn)
  }
}

fsokande_valj_funk_btn <- function(obj_btn = "//*[@id='65']/div[2]/div/div[1]/div/div[1]") {

    web_elem <- Wait_For_Load("xpath", obj_btn)
    web_elem$clickElement()

    Sys.sleep(2)
    
    #Säkerställ att det går att ladda in funk igen på sidan innan vi går vidare
    #web_elem <- Wait_For_Load("xpath",obj_btn)
    Kontrollera_laddstatus()
}

fsokande_valj_funk <- function(obj_btn = "//*[@id='65']/div[2]/div/div[1]/div/div[1]") {
ArFunkValt <- Funk_valt()  
  while(ArFunkValt == FALSE){
    web_elem <- Wait_For_Load("xpath", obj_btn)
    web_elem$clickElement()
    
    Sys.sleep(2)
    
    #Säkerställ att det går att ladda in funk igen på sidan innan vi går vidare
    web_elem <- Wait_For_Load("xpath",obj_btn)
    ArFunkValt <- TRUE
    }
}

fsokande_avvalj_funk <- function(obj_btn = "//*[@id='65']/div[2]/div/div[1]/div/div[1]") {
  ArFunkValt <- Funk_valt()    
  while(ArFunkValt == TRUE){
    web_elem <- Wait_For_Load("xpath", obj_btn)
    web_elem$clickElement()
    
    Sys.sleep(2)
    
    #Säkerställ att det går att ladda in funk igen på sidan innan vi går vidare
    web_elem <- Wait_For_Load("xpath",obj_btn)
    ArFunkValt <- FALSE
  }
}

fsokande_valj_etablering <- function(intem_nr = 1, obj_menu = "//*[@id='64']/div[2]/div/div[1]/div[5]/div/div[1]/div[1]") {
    fardig <- FALSE
    while(fardig==FALSE){
      #Öppna etableringsmenyn
      web_elem <- Wait_For_Load("xpath", obj_menu)
      web_elem$clickElement()
  
      #Välj alternativ i etableringsmenyn
      web_elem <- Wait_For_Load("xpath", paste0("//*[@id='DS']/div/div/div[1]/div[", intem_nr , "]/div[1]"))
      web_elem$clickElement()
      
      Sys.sleep(3)
  
      #Säkerställ att vecka 1 finns ladda på sidan innan vi går vidare
      web_elem <- Wait_For_Load("xpath",obj_menu)
      
      # Testa om vi har valt något i etablering
      obj_str <- '//*[@id="64"]/div[2]/div/div[1]/div[5]/div/div[3]'
      Elem <- remDr$findElement(using = "xpath", value = obj_str)
      elem_namn <- as.character(Elem$getElementAttribute("title"))
      if (elem_namn != "") fardig <- TRUE
    }
}

fsokande_avvalj_etablering <- function() {
  # Testa om vi har valt något i etablering
  obj_str <- '//*[@id="64"]/div[2]/div/div[1]/div[5]/div/div[3]'
  Elem <- remDr$findElement(using = "xpath", value = obj_str)
  elem_namn <- as.character(Elem$getElementAttribute("title"))
  if (elem_namn != "") {
    # Hämta element som är det gröna fältet med länsnamnet (om det finns)
    obj_valjbort <- '//*[@id="64"]/div[2]/div/div[1]/div[5]/div/div[3]/div[1]'
    webElem_valjbort <- Wait_For_Load("xpath", obj_valjbort)
    webElem_valjbort$clickElement()
  }  
}

fsokande_valj_fodelseland <- function(intem_nr = 1, obj_menu = "//*[@id='48']/div[2]/div/div[1]/div[5]/div/div[1]/div[1]") {
    fardig <- FALSE
    while(fardig==FALSE){
      # Testa om vi har valt något i födelseland
      obj_str <- '//*[@id="48"]/div[2]/div/div[1]/div[5]/div/div[3]'
      Elem <- remDr$findElement(using = "xpath", value = obj_str)
      elem_namn <- as.character(Elem$getElementAttribute("title"))
      if (elem_namn == fsokande_fodelselandmatris(intem_nr)){ 
        fardig <- TRUE 
      }else {
        fsokande_avvalj_fodelseland()
        
        #Öppna födelselandsmenyn
        web_elem <- Wait_For_Load("xpath", obj_menu)
        web_elem$clickElement()
    
        #Välj alternativ i födelselandsmenyn
        web_elem <- Wait_For_Load("xpath", paste0("//*[@id='DS']/div/div/div[1]/div[", intem_nr , "]/div[1]"))
        web_elem$clickElement()
        
        Sys.sleep(3)
    
        #Säkerställ att vecka 1 finns ladda på sidan innan vi går vidare
        web_elem <- Wait_For_Load("xpath","//*[@id='55']/div[2]/div[1]/div[1]/div[2]/div/div[2]/div[1]")
      }
  }
}

fsokande_avvalj_fodelseland <- function() {
  # Testa om vi har valt något i etablering
  obj_str <- '//*[@id="48"]/div[2]/div/div[1]/div[5]/div/div[3]'
  Elem <- remDr$findElement(using = "xpath", value = obj_str)
  elem_namn <- as.character(Elem$getElementAttribute("title"))
  if (elem_namn != "") {
    # Hämta element som är det gröna fältet med länsnamnet (om det finns)
    obj_valjbort <- '//*[@id="48"]/div[2]/div/div[1]/div[5]/div/div[3]/div[1]'
    webElem_valjbort <- Wait_For_Load("xpath", obj_valjbort)
    webElem_valjbort$clickElement()
  }  
}

fsokande_byt_till_platser <- function(obj_btn = "//*[@id='41']/div[2]/table/tbody/tr/td") {

    web_elem <- Wait_For_Load("xpath", obj_btn)
    web_elem$clickElement()

    Sys.sleep(1)
    
    #Säkerställ att det går att öppna Rapport-menyn innan vi går vidare = sidan laddat klart
    web_elem <- Wait_For_Load("xpath","//*[@id='25']/div[2]/div/div[1]/div[5]/div/div[2]/div[2]/div/div/div")
}

fsokande_valj_rapport <- function(intem_nr = 3, obj_menu = "//*[@id='52']/div[2]/div/div[1]/div[5]/div/div[1]/div[1]"
) {
    
    #Öppna Rapport-menyn
    web_elem <- Wait_For_Load("xpath", obj_menu)
    web_elem$clickElement()

    #Välj rapport i Rapport-menyn
    web_elem <- Wait_For_Load("xpath", paste0("//*[@id='DS']/div/div/div[1]/div[", intem_nr , "]/div[1]"))
    web_elem$clickElement()
    
    Sys.sleep(1)

    #Säkerställ att det går att välja tabell innan vi lämnar funktionen
    web_elem <- Wait_For_Load("xpath", "//*[@id='51']/div[2]/div/div[1]/div[2]/div[1]")
}

fsokande_avvalj_rapport <- function() {
    retry(fsokande_valj_rapport(2), maxErrors = 10, sleep = 2)
    #Sys.sleep(1)
    retry(fsokande_valj_rapport(1), maxErrors = 10, sleep = 2)
    #Sys.sleep(1)
}


fsokande_valj_tabell <- function() {

    web_elem <- Wait_For_Load("xpath", "//*[@id='51']/div[2]/div/div[1]/div[2]/div[1]")
    web_elem$clickElement()

    Sys.sleep(1)
    
    #Säkerställ att det går att välja "Exportera till Excel" - bara att det går, vi väljer det inte
    web_elem <- Wait_For_Load("xpath", "//*[@id='73']/div[2]/table/tbody/tr/td")
    
}

fscrolldown <- function(id_nr = 49) {
    # identifiera nedåtpilen i scrollbaren
# gamla sättet    webElem <- remDr$findElement(using = 'xpath', paste0("//*[@id=", "'", id_nr, "'", "]/div[2]/div[1]/div[4]/span"))
        print("Start: fscrolldown")
        webElem <- Wait_For_Load('xpath', paste0("//*[@id=", "'", id_nr, "'", "]/div[2]/div[1]/div[4]/span"))
        remDr$mouseMoveToLocation(webElement = webElem)
        
        Sys.sleep(1)

    # klicka 50 ggr på nedåtpilen för att gå till botten av resultatlistan
    for (ant_klick in 1:50){
        remDr$click()
    }
    Sys.sleep(1)
    print("Slut: fscrolldown")
}

fplatser_valj_lan <- function(intem_nr = 10, obj_menu = "//*[@id='14']/div[2]/div/div[1]/div[5]/div/div[1]/div[1]") {

    # print("Start: fplatser_valj_lan")
    fardig <- FALSE
    forsok <- 0
    while(fardig==FALSE & forsok < 20){
      # Hämta element som innehåller länsnamn (om något är valt)
      obj_lanNamn <- '//*[@id="14"]/div[2]/div/div[1]/div[5]/div/div[3]'
      webElem <- remDr$findElement(using = "xpath", value = obj_lanNamn)
      lan_namn <- as.character(webElem$getElementAttribute("title"))
      if (lan_namn == RegList[intem_nr,2]) {
        fardig <- TRUE
      } else {
        forsok <- forsok + 1
        web_elem <- Wait_For_Load("xpath", obj_menu)
        web_elem$clickElement()
        
        web_elem <- Wait_For_Load("xpath", paste0("//*[@id='DS']/div/div/div[1]/div[", intem_nr , "]/div[1]"))
        web_elem$clickElement()
        
        Sys.sleep(2)
        
        #Säkerställ att vecka 1 finns laddad på sidan innan vi går vidare
        web_elem <- Wait_For_Load("xpath",obj_menu)
        if (forsok == 5 | forsok == 10) fplatser_avvalj_lan()
      }
    }
  
    # print("Slut: fplatser_valj_lan")    
}

fplatser_avvalj_lan <- function() {

  # Hämta element som innehåller länsnamn (om något är valt)
  obj_lanNamn <- '//*[@id="14"]/div[2]/div/div[1]/div[5]/div/div[3]'
  webElem <- remDr$findElement(using = "xpath", value = obj_lanNamn)
  lan_namn <- as.character(webElem$getElementAttribute("title"))
  
  # Gå vidare och klicka ur länet i gröna fältet om det är något
  # län valt
  if (lan_namn != "") {
    # Hämta element som är det gröna fältet med länsnamnet (om det finns)
    obj_valjbortlan <- '//*[@id="14"]/div[2]/div/div[1]/div[5]/div/div[3]/div[1]'
    webElem_valjbort <- Wait_For_Load("xpath", obj_valjbortlan)
    webElem_valjbort$clickElement()
  }  
  
}

fplatser_valj_kommun <- function(intem_nr = 3, obj_menu = "//*[@id='22']/div[2]/div/div[1]/div[5]/div/div[1]/div[1]") {

    fardig <- FALSE
    while(fardig==FALSE){
      # Hämta element som innehåller kommunnamn (om något är valt)
      obj_kommNamn <- "//*[@id='22']/div[2]/div/div[1]/div[5]/div/div[3]"
      webElem <- remDr$findElement(using = "xpath", value = obj_kommNamn)
      komm_namn <- as.character(webElem$getElementAttribute("title"))
      # Prova om kommunnamnet är det som det bör vara
      if (komm_namn == ValdRegion_df[intem_nr,4]) {
        fardig <- TRUE
      } else {
        
        # Om kommun namnet inte är vad det ska vara så öppnar vi kommun-
        # listrutan och väljer rätt kommun 
        web_elem <- Wait_For_Load("xpath", obj_menu)
        web_elem$clickElement()
        
        web_elem <- Wait_For_Load("xpath", paste0("//*[@id='DS']/div/div/div[1]/div[", intem_nr , "]/div[1]"))
        web_elem$clickElement()
        
        Sys.sleep(2)
        
        #Säkerställ att sidan är laddad innan vi går vidare
        web_elem <- Wait_For_Load("xpath",obj_menu)
        
        # Kontrollera att vi har rätt län valt
        # Hämta element som innehåller länsnamn (om något är valt)
        obj_lanNamn <- '//*[@id="14"]/div[2]/div/div[1]/div[5]/div/div[3]'
        webElem <- remDr$findElement(using = "xpath", value = obj_lanNamn)
        lan_namn <- as.character(webElem$getElementAttribute("title"))
        # Ibland händer det att något går snett och ett annat län blir valt
        # Vi testar här om vi har rätt län och om inte, så nollställer vi 
        # kontrollen och väljer rätt län igen
        if (lan_namn != ValdRegion_df[intem_nr,2]) fplatser_Nollstall_lan()
      }
    }
}

fplatser_avvalj_kommun <- function() {
  # Hämta element som innehåller länsnamn (om något är valt)
  obj_kommNamn <- "//*[@id='22']/div[2]/div/div[1]/div[5]/div/div[3]"
  webElem <- remDr$findElement(using = "xpath", value = obj_kommNamn)
  komm_namn <- as.character(webElem$getElementAttribute("title"))
  
  # Gå vidare och klicka ur länet i gröna fältet om det är något
  # kommun valt
  if (komm_namn != "") {
    # Hämta element som är det gröna fältet med länsnamnet (om det finns)
    obj_valjbortkomm <- "//*[@id='22']/div[2]/div/div[1]/div[5]/div/div[3]/div[1]"
    webElem_valjbort <- Wait_For_Load("xpath", obj_valjbortkomm)
    webElem_valjbort$clickElement()
  }
}

fplatser_valj_rapport <- function(intem_nr = 3, obj_menu = "//*[@id='26']/div[2]/div/div[1]/div[5]/div/div[1]/div[1]"
) {

    #Öppna Rapport-menyn
    web_elem <- Wait_For_Load("xpath", obj_menu)
    web_elem$clickElement()

    #Välj rapport i Rapport-menyn
    web_elem <- Wait_For_Load("xpath", paste0("//*[@id='DS']/div/div/div[1]/div[", intem_nr , "]/div[1]"))
    web_elem$clickElement()
    
    Sys.sleep(1)

    #Säkerställ att det går att välja Tabell innan vi lämnar funktionen - dvs. att sidan laddat klart
    web_elem <- Wait_For_Load("xpath","//*[@id='20']/div[2]/div/div[1]/div[2]/div[1]")
    
}

fplatser_avvalj_rapport <- function() {
    retry(fplatser_valj_rapport(2), maxErrors = 10, sleep = 2)
    #Sys.sleep(1)
    retry(fplatser_valj_rapport(1), maxErrors = 10, sleep = 2)
    #Sys.sleep(1)
}

fplatser_valj_tabell <- function() {

    web_elem <- Wait_For_Load("xpath", "//*[@id='21']/div[2]/div/div[1]/div[2]/div[1]")
    web_elem$clickElement()
    
    Sys.sleep(1)
    
    #Säkerställ att det går att ladda in Exportera till Excel innan vi lämnar funktionen - dvs. att sidan laddat klart
    web_elem <- Wait_For_Load("xpath","//*[@id='32']/div[2]/table/tbody/tr/td")
}


frensa_res <- function(varden, platser = FALSE){
    print("Start: frensa_res")
    varden <- varden[varden != "Resize column"]
    varden <- str_remove(varden, "\\s")
    varden <- as.numeric(varden)
    if (length(varden) < 160 ) {
        ant_kol <-  3
    } else {
        ant_kol <- 6
    }
    varden_m <- matrix(varden, ncol = ant_kol, byrow = TRUE)
    varden_tbl <- as_tibble(varden_m)

    # Här hårdkodas kolumnrubriker. De borde hämtas från resultattabellen istället
    if (ncol(varden_tbl) == 3) {
        names(varden_tbl) <- Hamta_kolumn_namn(kon = FALSE, platser)
    } else {
        names(varden_tbl) <- Hamta_kolumn_namn(kon = TRUE)
    }
    varden_tbl$vecka <- 1:nrow(varden_tbl)
    varden_tbl <- relocate(varden_tbl, vecka, .before = 1)
    print("Slut: frensa_res")
    return(varden_tbl)
}

fsokande_extr_data <- function(x, kon = FALSE){
    #print(paste0("Start: fsokande_extr_data ", x))
    if (kon) {
        xp <- paste0("//*[@id='49']/div[2]/div[1]/div[1]/div[5]/div/div[", x, "]")
    } else {
        xp <- paste0("//*[@id='55']/div[2]/div[1]/div[1]/div[5]/div/div[", x, "]")
    }
    
    tmp <- NULL
    tmp <- Wait_For_Load("xpath", xp, 3)
    
    if(is.null(tmp)) return(0) else return(tmp$getElementAttribute("title"))
}


fsokande_skapa_tab <- function(kon = FALSE) {
    print("Start: fsokande_skapa_tab")
    Sys.sleep(3)
    if (kon){
        ant_kol <- 6
        fscrolldown(id_nr = 49)
    } else {
        ant_kol <- 3
        fscrolldown(id_nr = 55)
    }

    varden <- ""
    Tidmatt <- Sys.time()
    print(paste0("Start loop - fsokande_extr_data ", Tidmatt))
    for (i in 1:(veckonr * ant_kol)) {
        varden[i] <- fsokande_extr_data(i, kon = kon)
    }
    print(paste0("Slut loop - fsokande_extr_data ", Sys.time()-Tidmatt))
    varden <- unlist(varden)
    df <- frensa_res(varden)

    elem_etab <- Wait_For_Load("xpath", "//*[@id='64']/div[2]/div/div[1]/div[5]/div/div[3]")
    if (!is.null(elem_etab)) namn_etab <- as.character(elem_etab$getElementAttribute("title") ) else namn_etab <- ""

    elem_fland <- Wait_For_Load("xpath", "//*[@id='48']/div[2]/div/div[1]/div[5]/div/div[3]")
    if (!is.null(elem_fland)) namn_fland <- as.character(elem_fland$getElementAttribute("title") ) else namn_fland <- ""

    elem_alder <- Wait_For_Load("xpath", "//*[@id='58']/div[2]/div/div[1]/div[5]/div/div[3]")
    if (!is.null(elem_alder)) namn_alder <- as.character(elem_alder$getElementAttribute("title") ) else namn_alder <- ""

    elem_lannamn <- Wait_For_Load("xpath", "//*[@id='57']/div[2]/div/div[1]/div[5]/div/div[3]")
    if (!is.null(elem_lannamn)) namn_lan <- as.character(elem_lannamn$getElementAttribute("title") ) else namn_lan <- ""

    elem_komnamn <- Wait_For_Load("xpath", "//*[@id='50']/div[2]/div/div[1]/div[5]/div/div[3]")
    if (!is.null(elem_komnamn)) namn_kommun <- as.character(elem_komnamn$getElementAttribute("title") ) else namn_kommun <- ""

    if (namn_etab != "") df$etab <- namn_etab
    if (namn_fland != "") df$fland <- namn_fland
    if (namn_alder != "") df$age <- namn_alder
    if (namn_alder == "") df$age <- "16 - 64"
    if (namn_lan != "") df$region <- namn_lan
    if (namn_kommun != "") df$region <- namn_kommun
    if (namn_kommun == "" & namn_lan == "") df$region <- "0 Riket"
    
    print("Slut: fsokande_skapa_tab")
    return(df)
}

fplatser_extr_data <- function(x){
    xp <- paste0("//*[@id='19']/div[2]/div[1]/div[1]/div[5]/div/div[", x, "]")
    
    tmp <- NULL
    tmp <- Wait_For_Load("xpath", xp)
    if(is.null(tmp)) return(0) else return(tmp$getElementAttribute("title"))
    
}

fplatser_skapa_tab <- function() {

    fscrolldown(id_nr = 19)

    Sys.sleep(1)

    varden <- ""
    for (i in 1:(veckonr * 3)) {
        varden[i] <- fplatser_extr_data(i)
    }
    varden <- unlist(varden)
    df <- frensa_res(varden, platser = TRUE)

    elem_lannamn <- Wait_For_Load("xpath", "//*[@id='14']/div[2]/div/div[1]/div[5]/div/div[3]")
    namn_lan <- as.character(elem_lannamn$getElementAttribute("title") )

    elem_komnamn <- Wait_For_Load("xpath", "//*[@id='22']/div[2]/div/div[1]/div[5]/div/div[3]")

    namn_kommun <- as.character(elem_komnamn$getElementAttribute("title") )


    if (namn_lan != "") df$region <- namn_lan
    if (namn_kommun != "") df$region <- namn_kommun
    if (namn_kommun == "" & namn_lan == "") df$region <- "0 Riket"
    return(df)
}

fsokande_extr_data_kommuner <- function(ant_kom = 33, veckonr = 54, kon = FALSE){


    lkom_df <- list()

    # Loopa igenom regionens kommuner
    for (i in 1:ant_kom){
        print(paste0("Kommun nr: ", i))
        
        #Ny testfunktion
        retry(fsokande_valj_kommun(intem_nr = i),maxErrors = 10, sleep =  2)
      
        #Gammal och fungerande 
        #fsokande_valj_kommun(intem_nr = i)

#        Testar att släcka ned denna då vi kör med Wait and Load-funktionen nu        
#        Sys.sleep(4)

        # Scrollar till botten av resultatlistan
        # Behövs väl ej - scrollar i fsokand_skapa_tab
#        if (kon){
#            fscrolldown(id_nr = 49)
#        } else {
#            fscrolldown(id_nr = 55)
#        }

        df <- fsokande_skapa_tab(kon = kon)

        lkom_df[[i]] <- df

    }

    df <- bind_rows(lkom_df)
    fsokande_avvalj_kommun()
    return(df)
}

fplatser_extr_data_kommuner <- function(ant_kom = 33, veckonr = 54){

    lkom_df <- list()

    # Loopa igenom regionens kommuner
    for (i in 1:ant_kom){
    
        retry(fplatser_valj_kommun(intem_nr = i), maxErrors = 10, sleep = 2)
        
#        Sys.sleep(4)

        # Scrollar till botten av resultatlistan
        #fscrolldown(id_nr = 19)


        ### Identifierar kommunnamnet och sparar det som komnamn
        elem_komnamn <- Wait_For_Load("xpath", "//*[@id='22']/div[2]/div/div[1]/div[5]/div/div[3]")

        komnamn <- as.character(elem_komnamn$getElementAttribute("title") )

        df <- fplatser_skapa_tab()
        df$region <- komnamn

        lkom_df[[i]] <- df

    }

    df <- bind_rows(lkom_df)

    fplatser_avvalj_kommun()

    return(df)
}

fplatser_extr_data_lan <- function() {
    fplatser_avvalj_lan()
    list_dflan <- list()
    for (i in 1:21) {
        retry(fplatser_valj_lan(i))
#        Sys.sleep(2)
        dflan <- fplatser_skapa_tab()
        list_dflan[[i]] <- dflan
    }

    dflan <- bind_rows(list_dflan)

    retry(fplatser_avvalj_lan())
    return(dflan)
}

fsokande_extr_data_lan <- function(kon = FALSE) {
    print("Start: fsokande_extr_data_lan")
    retry(fsokande_avvalj_lan())
    list_dflan <- list()
    for (i in 1:21) {
        print(paste0("Län nr: ", i))
        retry(fsokande_valj_lan(i))
#        Sys.sleep(2)
        dflan <- fsokande_skapa_tab(kon = kon)
        list_dflan[[i]] <- dflan
    }

    dflan <- bind_rows(list_dflan)

    retry(fsokande_avvalj_lan())

    return(dflan)
    print("Slut: fsokande_extr_data_lan")
}


Wait_For_Load <- function(using_str = "xpath", value_str, Wait_time = 10) {
    
    start_time <- Sys.time()
    webElem <- NULL
    while(is.null(webElem)){
        webElem <- tryCatch({
            suppressMessages({
            remDr$findElement(using = using_str, value = value_str)})},    
                            error = function(e) { NULL })
        now_time <- Sys.time()
        #testnamn <- as.character(webElem$getElementAttribute("title") )
        #print(testnamn)
        if(now_time - start_time > Wait_time) return(NULL)
    }
    #Om man vill testa hur snabbt det går att hämta in data
    #print(now_time - start_time)
    return(webElem)
}

retry <- function(expr, isError=function(x) "try-error" %in% class(x), maxErrors=5, sleep=0) {
  attempts = 0
  retval <- NULL
  retval = try(eval(expr))
  while (isError(retval)) {
    attempts = attempts + 1
    if (attempts >= maxErrors) {
      msg = sprintf("retry: too many retries [[%s]]", capture.output(str(retval)))
      flog.fatal(msg)
      stop(msg)
    } else {
      msg = sprintf("retry: error in attempt %i/%i [[%s]]", attempts, maxErrors, 
                    capture.output(str(retval)))
      flog.error(msg)
      warning(msg)
    }
    if (sleep > 0) Sys.sleep(sleep)
    retval = try(eval(expr))
  }
  if (is.null(retval)) retval <- "Inga fel, funktion klar." else retval <- "Funktion klar."
  return(retval)
}

Hamta_kolumn_namn <- function(kon = FALSE, platser = FALSE){
  kolumnrubriker <- ""
  if (kon){
    ant_kolumner <- 6
    kol_id <- 49
  } else {
    if (platser == FALSE) {
      ant_kolumner <- 3
      kol_id <- 55
    } else {
      ant_kolumner <- 3
      kol_id <- 19
    }
  }
  for (kol in 1:ant_kolumner){
    elem_namn <- Wait_For_Load("xpath", paste0('//*[@id="', kol_id, '"]/div[2]/div[1]/div[1]/div[4]/div/div[', kol, ']'))
    KolNamn <- as.character(elem_namn$getElementAttribute("title") )
    kolumnrubriker[kol] <- KolNamn
  }
  return(kolumnrubriker)
}

Kon_valt <- function(){
  elem_rubr <- Wait_For_Load("xpath", '//*[@id="55"]/div[2]/div[1]/div[1]/div[4]/div/div[1]')
  rubr_visas <- elem_rubr$isElementDisplayed()
  if (rubr_visas == "FALSE") return(TRUE) else return(FALSE)
}

Funk_valt <- function(){
  kon <- Kon_valt()
  varden_innan <- NULL
  varden_efter <- NULL
  for (korning in 1:2){
      varden <- NULL
        for (i in 1:9) {
          varden[i] <- fsokande_extr_data(i, kon = kon)
          if (varden[i]=="-") varden[i] <- NULL
        }
        varden <- unlist(varden)
        varden <- str_remove(varden, "\\s")
        varden <- as.numeric(varden)
        varden <- sum(varden)
      
        # Lägg värdena i varsin vektor som vi jämför efteråt
        if (korning == 1) varden_innan <- varden else varden_efter <- varden
        # Klicka på funktionsnedsättning - en gång per varv, andra
        # gången för att återställa valet till vad det var innan
        retry(fsokande_valj_funk_btn(), maxErrors = 5, sleep = 2)
        Sys.sleep(3)
  }
  # Vi behöver bearbeta de båda vektorerna innan vi kan jämföra dem
  
  # Här kollar vi vilken vektor som har störst summa, om varden_innan
  # är störst är Funktion_valt = FALSE annars TRUE
  if (varden_innan > varden_efter) return(FALSE) else return(TRUE) 
}

Kontrollera_laddstatus <- function(){
  kon <- Kon_valt()
  if (kon) {
    xp <- paste0("//*[@id='49']/div[2]/div[1]/div[1]/div[5]/div/div[1]")
  } else {
    xp <- paste0("//*[@id='55']/div[2]/div[1]/div[1]/div[5]/div/div[1]")
  }
  Wait_For_Load("xpath", xp)

}

fsokande_sakerstall_laddat_klart <- function() {
  KontrollNamn <- "Nej"
  elem_namn <- NULL
  # Testa om vi har valt något i etablering - ändra i så fall KontrollNamn
  obj_str <- '//*[@id="64"]/div[2]/div/div[1]/div[5]/div/div[3]'
  Elem <- remDr$findElement(using = "xpath", value = obj_str)
  elem_namn <- as.character(Elem$getElementAttribute("title"))
  if (elem_namn == "Nej") KontrollNamn = "Etableringsuppdraget"
  
  while (elem_namn != KontrollNamn) {
    # Välj tredje alternativet i etablerings-menyn
    # Öppna etableringsmenyn
    web_elem <- Wait_For_Load("xpath", "//*[@id='64']/div[2]/div/div[1]/div[5]/div/div[1]/div[1]")
    web_elem$clickElement()
    
    # Välj alternativ i etableringsmenyn
    web_elem <- Wait_For_Load("xpath", "//*[@id='DS']/div/div/div[1]/div[3]/div[1]")
    web_elem$clickElement()
    
    Sys.sleep(1)
    
    # Hämta värde som är valt i etableringsmenyn
    obj_str <- '//*[@id="64"]/div[2]/div/div[1]/div[5]/div/div[3]'
    Elem <- remDr$findElement(using = "xpath", value = obj_str)
    elem_namn <- as.character(Elem$getElementAttribute("title"))
  }
  # När vi har fått den text vi vill ha, stäng av etablering
  obj_valjbort <- '//*[@id="64"]/div[2]/div/div[1]/div[5]/div/div[3]/div[1]'
  webElem_valjbort <- Wait_For_Load("xpath", obj_valjbort)
  webElem_valjbort$clickElement()
}

fsokande_fodelselandmatris <- function(intem_nr = 1){
  if (intem_nr == 1) return("Sverige")
  if (intem_nr == 2) return("Europa utom Sverige")
  if (intem_nr == 3) return("Utomeuropeiska länder")
} 

fsokande_aldermatris <- function(intem_nr = 1){
  if (intem_nr == 1) return("- 24")
  if (intem_nr == 2) return("25 - 29")
  if (intem_nr == 3) return("30 - 39")
  if (intem_nr == 4) return("40 - 49")
  if (intem_nr == 5) return("50 - 59")
  if (intem_nr == 6) return("60 -")
} 

RegionKommunMatris <- function(lanskod = 20){
  # AktMapp <- dirname(rstudioapi::getActiveDocumentContext()$path)
  # AktFil <- paste0(AktMapp, "/", "RegionerKommuner.csv")
  Retur_df <- read.csv("RegionerKommuner.csv", header = TRUE, sep = ";")
  Retur_df <- subset(Retur_df, Retur_df$Regionkod == lanskod)
  return(Retur_df)
}

RegionLista <- function(){
  #AktMapp <- dirname(rstudioapi::getActiveDocumentContext()$path)
  #AktFil <- paste0(AktMapp, "/", "RegionerKommuner.csv")
  Retur_df <- read.csv("RegionerKommuner.csv", header = TRUE, sep = ";")
  Retur_df <- Retur_df %>% distinct(Regionkod, .keep_all = TRUE)
  Retur_df <- subset(Retur_df, select = -c(Kommunkod,KommunFull))
  return(Retur_df)
}

fsokande_ValjOm_lan <- function(){
  print("Välj om län")

  # välj nu valt län
  fsokande_valj_lan(ValdRegion_df[1,5])
  
  # Igen - både hängslen och livrem i att vänta på att det laddar klart
  Sys.sleep(2)
  
}

fsokande_Nollstall_lan <- function(){
  print("Nollställ län")
  # Välj ett annat län än det användaren valt
  if (ValdRegion_df[1,5] < 19) fsokande_valj_lan(ValdRegion_df[1,5]+3)
    else fsokande_valj_lan(ValdRegion_df[1,5]-3)
  #Vänta in laddning - både hängslen och livrem
  Sys.sleep(1)
  fsokande_sakerstall_laddat_klart()

  # välj nu valt län
  fsokande_valj_lan(ValdRegion_df[1,5])
  
  # Igen - både hängslen och livrem i att vänta på att det laddar klart
  Sys.sleep(1)
  fsokande_sakerstall_laddat_klart()
  
}

fplatser_Nollstall_lan <- function(){
  # Välj ett annat län än det användaren valt
  if (ValdRegion_df[1,5] < 19) fplatser_valj_lan(ValdRegion_df[1,5]+3)
  else fplatser_valj_lan(ValdRegion_df[1,5]-3)
  #Vänta in laddning - både hängslen och livrem
  Sys.sleep(1)
  obj_lanNamn <- '//*[@id="14"]/div[2]/div/div[1]/div[5]/div/div[3]'
  Wait_For_Load(value_str = obj_lanNamn)
  
  # välj nu valt län
  fplatser_valj_lan(ValdRegion_df[1,5])
  
  # Igen - både hängslen och livrem i att vänta på att det laddar klart
  Sys.sleep(1)
  obj_lanNamn <- '//*[@id="14"]/div[2]/div/div[1]/div[5]/div/div[3]'
  Wait_For_Load(value_str = obj_lanNamn)
  
}

TidFormaterat <- function(fim, ini){
  dif=as.numeric(difftime(fim, ini, units='min'))
  return(paste0(sprintf('%2d', as.integer(dif)), " minuter och "
                ,sprintf('%2.0f', (dif-as.integer(dif))*60), " sekunder."))
}