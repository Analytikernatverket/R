# -----------------------------------------------------------------------------
# Den här funktionen tar en dataframe eller ett lazy_tbl-objekt från dbplyr som input
# (ett lazy_tbl ojekt är ett tabell från dbplyr innan man kört "collect()").
# Funktionen har fyra parametrar:
# tab = den tabell som man vill lägga till namn i klartext på.
# enbart_namn = om den är TRUE joinas bara de två första kolumnerna i nyckelfilen
# urval_cols = ibland vill man inte lägga på namn på alla variabler utan endast 
# ett urval. Då kan man specificera vilka kolumner som man vill ha namn i klartext på.
# Exempelvis urval_cols = c("Kon", "Kommun") innebär att namn i klartext enbart
# läggs till för variablerna "Kon" och "Kommun".
#
# Christian Lindell, Region Skåne 2024-08-22
# -----------------------------------------------------------------------------

fjoin_klartext <- function(tab, enbart_namn = FALSE, urval_cols = NULL, path = "../../nycklar/", 
                           shiftlagesokanslig = TRUE, 
                           trimma_data = TRUE) {
  
  suppressMessages({
    library(tidyverse, quietly = TRUE)
    library(DBI, quietly = TRUE)
  })
  
  # Om det specifiverats vilka kolumer man vill lägga till namn i klartext på i
  # parametern "enbart namn" så sparas dessa i variabeln namn_tab. Annars sparas alla 
  # variabelnam
  if (is.null(urval_cols)) {
    namn_tab <- colnames(tab)
  } else {
    namn_tab <- urval_cols[urval_cols %in%  colnames(tab)]
  }
  
    # # SCB verkar inte kunna bestämma sig för om Kon ska vara en integer eller en character.
  # # För säkerhets skull görs den om till en character.
  # if ("Kon" %in% colnames(tab)) tab <- mutate(tab, Kon = as.character(Kon))
  # 
  # ny test-grej från Peter - vi spara kolumnernas datatyp för det dataset vi skickar in i funktionen
  datatyper <- map_chr(tab, ~class(.x)[1])
  
  # därefter konverterar vi alla kolumner till character
  tab <- tab %>% 
    mutate(across(everything(), as.character))
  
  # vi trimmar data i alla kolumner så att vi tar bort mellanslag som är före eller efter värdet
  if (trimma_data) tab <- tab %>% mutate(across(where(is.character), str_trim))
  
  # om man valt shiftlagesokanslig så görs alla kolumnnamn om till gemener men originalkolumnnamnen sparas
  # så att vi kan byta tillbaka till dem när vi gjort klart alla kopplingar
  if (shiftlagesokanslig) {
    orig_kol_namn <- names(tab)
    names(tab) <- tolower(names(tab))
  } 
  
  # Lista filnamen på all nyckelfiler i katalogen "nycklar".
  files <- list.files(path, pattern = "xlsx")
  
  # Sortera ut de excelfiler vars namn matchar de variabler som finns i tab
  if (shiftlagesokanslig) {
    files_finns <- files[tolower(files) %in% paste0(tolower(namn_tab), ".xlsx")]
  } else {
    files_finns <- files[files %in% paste0(namn_tab, ".xlsx")]
  }
  # Loopa igenom alla nyckelfiler, läs in dem och joina dem med med tab
  for (i in 1:length(files_finns)) {
    
    dfnamn_nyckel <- readxl::read_excel(paste0(path, files_finns[i]))
    
    # ny test-grej från Peter - vi konverterar alla kolumner i inlästa nycklar till character
    # då har både skickat dataset och nycklar alltid character och datatypen kommer aldrig att vara ett problem
    dfnamn_nyckel <- dfnamn_nyckel %>%
      mutate(across(everything(), as.character))
    
    # Om enbart_namn angivits till TRUE så joinas bara de två första kolumnerna i nyckelfilen
    if (enbart_namn) dfnamn_nyckel <- dfnamn_nyckel[, 1:2]
    
    # om shiftlägesokänslig valts så blir kolumnnamnet i första kolumnen (den som kopplas) enbart gemener
    if (shiftlagesokanslig) names(dfnamn_nyckel)[1] <- tolower(names(dfnamn_nyckel)[1])
    
    # # Om en nyckel felatigt angivets som ett flyttal ändras det till integer
    # if (is.numeric(dfnamn_nyckel[[1]])) dfnamn_nyckel[1] <- as.integer(dfnamn_nyckel[[1]])
    
    # Oavsett om det är ett lazy_tbl-objekt från dbplyr eller en datafram så 
    # anger jag copy = TRUE i join-funktionen. Det verkar inte ställa till problem även om det är en dataframe
    suppressMessages({
      tab <- tab |>
        left_join(dfnamn_nyckel, copy = TRUE)
    })
    
  }
  
  names(tab)[1:length(orig_kol_namn)] <- orig_kol_namn
  # här konverterar jag tillbaka det skickade dataset till de ursprungliga datatyperna, nya kolumner får alltid 
  # character då de är text
  tab <- tab %>% 
    mutate(across(everything(), ~ type.convert(as.character(.), as.is = TRUE ))) %>% 
    mutate(across(names(datatyper), ~ {
      target_class <- datatyper[[cur_column()]]
      if (target_class == "date") as.Date(.) else
        if (target_class == "integer") as.integer(.) else
          if (target_class == "numeric") as.numeric(.) else
            if (target_class == "character") as.character(.) else .
    }))
  
  return(tab)
  
}

# TESTKOD - Exempel (Även om det är en vanlig dataframe som returneras så verkar 
# det inte bli fel om man kör collect() på den.)
# df <- fklartext(tab) |> collect()
# df <- fklartext(tab, enbart_namn = TRUE) |> collect()
# df <- fklartext(tab, urval_cols = c("Kon", "Kommun"), enbart_namn = TRUE) |> collect()







