#' Scrape and Enhance Quandoo Restaurant Reviews
#'
#' This function is designed for scraping restaurant reviews from Quandoo and augmenting them with additional insights.
#' It retrieves user-generated content and enriches this data with valuable information such as calculated review dates,
#' estimated gender of the reviewers, and detected review languages. Gender estimation is performed using the `gender::gender()`
#' function. Language detection is accomplished with `cld2::detect_language()`.
#' Load the example dataset with 1444 reviews for a restaurant in zurich:`load(review_example)` (?review_example)
#'
#'
#'
#' @param url The URL to the Quandoo restaurant page from which the reviews are to be retrieved.
#'
#' @return A df with the following structure:
#' (name|review_count|date|sex|prob_male|rating|description|description_lang)
#' @export
#' @examples review_df <- QuandooScraping()
#' @examples review_df <- QuandooScraping(url="https://www.quandoo.ch/en/place/yens-restaurant-11833")
#' @importFrom lubridate %m-%

QuandooScraping <- function(url="https://www.quandoo.ch/en/place/restaurant-sporrer-61089"){


  url = sub("(?:/speisekarte|/bilder|/bewertungen|#content).*", "", url)
  url = sub("(?:/ruokalista|/kuvat|/arvostelut|#content).*", "", url) # finisch
  url = sub("(?:/menu|/immagini|/recensioni|#content).*", "", url) # italienisch
  url = sub("(?:/menu|/photos|/reviews|#content).*", "", url) # englisch
  url = sub("(?:/menu|/gorseller|/degerlendirme|#content).*", "", url) # türkisch
  url = sub("(?:/speisekarte|/bilder|/bewertungen|#content).*", "", url)

  # Testen ob quandoo element von url
  if(!grepl("quandoo", url)) {
    cat("This function is only designed for Quandoo. Check the url.")

    # Vorzeitiges Beenden der Funktion
    return(NULL)
  }

  # Ergänzen falls vergessen
  if(startsWith(url,"www.")){
    url <- paste0("https://", url)
    cat("url was completed with https://")
  }



  webpage = try(rvest::read_html(paste0(url, "/bewertungen?reviewPage=1#content")), silent = TRUE)

  if(inherits(webpage, "try-error")) {
    cat("Error reading the URL: ", url, "\n")
    return(NULL) # Beendet die Funktion vorzeitig
  }

  review_containers <- rvest::html_nodes(webpage, '[data-name="shared-review"]')



  if(length(review_containers)==0){
    cat("The restaurant has no or less than 4 ratings. Search for a restaurant with 4 or more reviews")
    return(NULL)
  }



  # Leere Liste, um alle Reviews zu speichern
  all_descriptions <- list()
  all_ratings <- list()
  all_persons <- list()

  # Anfangswert für die Seitenzahl setzen
  page_number <- 1

  # Variable, um zu überprüfen, ob es noch Seiten gibt
  has_more_pages <- TRUE

  while(has_more_pages) {
    # Erzeugen der spezifischen URL für die aktuelle Seite
    page_url <- paste0(url, "/bewertungen?reviewPage=", page_number,"#content")

    # Versuch, die Webseite zu lesen
    webpage <- rvest::read_html(page_url,silent=TRUE)

    # Alle Review-Container auswählen
    review_containers <- rvest::html_nodes(webpage, '[data-name="shared-review"]')



    review_nodes <- rvest::html_nodes(review_containers, 'p')
    review_descriptions <- rvest::html_text(review_nodes)


    review_rating_nodes <- rvest::html_nodes(review_containers, '[class^="ii3neg-0"]')
    review_rating <- rvest::html_text(review_rating_nodes)

    review_person_nodes <- rvest::html_nodes(review_containers, ".nm9kpb-0.iluhHB")
    review_person <- rvest::html_text(review_person_nodes)



    # Hinzufügen der extrahierten Reviews zur Gesamtliste, wenn es Reviews auf der Seite gibt
    if(length(review_descriptions) > 0) {
      all_descriptions[[page_number]] <- review_descriptions
      all_persons[[page_number]] <- review_person
      all_ratings[[page_number]] <- review_rating
      page_number <- page_number + 1
    } else {
      # Keine Reviews gefunden, Annahme, dass es keine weiteren Seiten gibt
      has_more_pages <- FALSE
    }

  }


  # Kombinieren aller Reviews in einen einzigen Vektor
  all_descriptions <- unlist(all_descriptions)
  all_persons <- unlist(all_persons)
  all_ratings <- unlist(all_ratings)
  length(all_descriptions)



  #### All_persons verarbeiten ####

  # Funktion, die auf jedes Listenelement angewendet wird
  process_persons <- function(all_persons) {

    all_persons <- gsub("a year ago", "1 year ago", all_persons)
    all_persons <- gsub("a day ago", "1 day ago", all_persons)
    all_persons <- gsub("a month ago", "1 month ago", all_persons)
    all_persons <- gsub("an hour ago", "1 hour ago", all_persons)
    all_persons <- gsub("a week ago", "1 week ago", all_persons)

    # Aufteilung des Strings bei " · "
    split_parts <- strsplit(all_persons, " · ")

    # Definition des Suchmusters
    pattern <- "\\d+\\s+(hour|hours|week|weeks|day|days|month|months|year|years) ago\\b"

    # Extraktion des Datums und der Person
    date <- stringr::str_extract(split_parts[[1]][1], pattern)



    # Extraktion der Zahl und der Zeiteinheit aus dem String
    matches <- regmatches(date, regexec("(\\d+)\\s*(hour|day|week|month|year)s? ago", date))[[1]]

    # Extraktion der Zahl und Konvertierung in numerisch
    anzahl <- as.numeric(matches[2])

    # Extraktion der Zeiteinheit
    zeiteinheit <- stringr::str_squish(matches[3])

    # Aktuelles Datum
    aktuelles_datum <- Sys.Date()

    # Berechnung des Datums basierend auf der Zeiteinheit
    if (zeiteinheit == "year"| zeiteinheit == "years") {
      datum_vor_n <- aktuelles_datum %m-% lubridate::years(anzahl)
    } else if (zeiteinheit == "month" | zeiteinheit == "months") {
      datum_vor_n <- aktuelles_datum %m-% months(anzahl)
    } else if (zeiteinheit == "day" | zeiteinheit == "days") {
      datum_vor_n <- aktuelles_datum %m-% lubridate::days(anzahl)
    } else if (zeiteinheit == "hour" | zeiteinheit == "hours"){
      datum_vor_n <- aktuelles_datum %m-% lubridate::hours(anzahl)
    } else {
      stop("Unbekannte Zeiteinheit")
    }



    # Formatierung des Datums, um nur Monat und Jahr anzuzeigen
    formatiertes_datum <- format(datum_vor_n, "%m/%Y")


    name <- stringr::str_replace_all(split_parts[[1]][1], pattern, "")
    sex_object <-  gender::gender(trimws(gsub("[a-zA-Z]\\.", "", tolower(name))))
    sex <- sex_object$gender
    if (length(sex) == 0 && is.logical(sex)){
      sex <- NA
      prob_male <- NA
    } else {
      sex = sex
      prob_male <- sex_object$proportion_male
    }




    # Extraktion der Bewertungsanzahl
    review_count <- as.numeric(gsub("[^0-9]", "", split_parts[[1]][2]))

    # Rückgabe als Liste
    return(list(name = name,date = formatiertes_datum,  review_count = review_count,sex=sex,prob_male=prob_male))
  }

  # Anwendung der Funktion auf jedes Element der Liste all_persons
  processed_persons <- lapply(all_persons, process_persons)



  #### all_ratings verarbeiten ####

  process_rating <- function(rating) {
    as.numeric(stringr::str_extract(rating, "\\d+(?=/6)"))
  }


  processed_rating <- lapply(all_ratings, process_rating)


  #### all_descriptions bearbeiten ####

  process_description <- function(description) {
    return(gsub("\n", " ", description))
  }

  processed_description <- lapply(all_descriptions, process_description)

  description_language <- lapply(processed_description,cld2::detect_language)

  #### Df für Rückgabe zusammensetzen ####

  df_rating_description <- data.frame(rating = unlist(processed_rating),
                                      description = unlist(processed_description),
                                      description_lang = unlist(description_language))


  df_person <- do.call(rbind, lapply(processed_persons, function(x) {
    # x ist hier eine Unterliste
    return(data.frame(name=x$name, review_count=x$review_count, date=x$date, sex=x$sex,prob_male=x$prob_male, stringsAsFactors=FALSE))
  }))


  df = cbind(df_person,df_rating_description)
  df$sex = as.factor(df$sex)
  df$date = as.Date(paste0("01/", df$date), "%d/%m/%Y")
  df$description_lang = as.factor(df$description_lang)

  return(df)
}

