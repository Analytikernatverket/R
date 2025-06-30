# ==============================================================
#
# Konfigurera R-studio och paketen usethis, git2r och gh så att 
# de fungerar med Github. Denna guide är skapad av Peter Möller,
# Region Dalarna. Det är inte säkert att alla steg behövs men 
# jag tror att det är så. 
# 
# För att det ska fungera måste man använda Keyring-paketet som 
# hanterar lösenord på ett säkert sätt. 
#
# ==============================================================
if (!require("pacman")) install.packages("pacman")
p_load(usethis,
       credentials,
       keyring,
       gh)

# ================= fyll i dina personliga uppgifter nedan =================
github_anvandarnamn <- "Användarnamn Github"
github_epost <- "n.n@regiondalarna.se"
github_losenord <- "lösenord"
github_namn_pa_anvandare <- "För- och efternamn"
# ==========================================================================

# ================= Normalt sett ska man inte behöva ändra något nedan utan bara ==================
# ================= köra det stegvis och följa instruktionerna i kommentarerna   ==================

# 1. ställ in användarnamn och mejl för ditt Github-konto
usethis::use_git_config(user.name = github_namn_pa_anvandare, user.email = github_epost)

# 2. Skapa token här med koden nedan eller gå in på din Github-användare
# och välj Settings - <> Developer settings och därefter Tokens (Classic)
# Se till att gist, repo, user, workflow är ikryssat som scopes

# öppna webbsidan för att 
usethis::create_github_token() 
# 2b Man kan lägga in token med hjälp av funktionen nedan, i så fall
#    ska inte Sys.setenv(GITHUB_PAT... behövas och hela steg 5 kan hoppas över
#gitcreds::gitcreds_set()             

# 3. Lägg token som skapades i steg 2 här i denna variabel
skapad_github_token <- "token"

# 4. Lägg in den i systemet nedan 
credentials::set_github_pat(skapad_github_token)

# 5. Slutligen lägger vi in den som environment-variabel
#    Detta steg behövs inte om man kör steg 2b enligt uppgift (jag har inte testat)
Sys.setenv(GITHUB_PAT_GITHUB_COM = paste0("protocol:https:host:github.com:username:", github_anvandarnamn, ":password:", skapad_github_token))
Sys.setenv(GITHUB_PAT = skapad_github_token)

# 6. Kontrollera att token är korrekt inställd
#    bara kontroll, ej nödvändigt och behövs inte om man kör steg 2b istället för steg 5
print(Sys.getenv("GITHUB_PAT_GITHUB_COM"))
print(Sys.getenv("GITHUB_PAT"))

# 7. Kör gh_whoami för att verifiera att vi har rätt scopes kopplad till vår token
gh::gh_whoami()

# 8. Starta om R och kör sedan koden nedan för att kolla att det funkar
usethis::git_sitrep()

# 9. För att func_API.R-funktionerna ska fungera så är det bra att
#    lägga in din token och inloggningsuppgifter i keyring

# det är sammanlagt tre olika service i keyring som behövs läggas in
key_set_with_value(service = "github_token",
                   username = github_anvandarnamn,
                   password = skapad_github_token)

key_set_with_value(service = "git2r",
                   username = github_anvandarnamn,
                   password = github_epost)

key_set_with_value(service = "github",
                   username = github_anvandarnamn,
                   password = github_losenord)