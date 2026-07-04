
#' @title SKFaktor laden
#' 
#' @description
#' Laden der SK-Faktoren für die Zeitraummethode zu einem `Jahr`
#' und `Monat`.
#' 
#'
#' @param path character, Ablageort der parquet-Daten der SK-Faktoren
#'  (default: "I:/Daten/SKFaktor/")
#' @param Jahr integer-Vektor mit den Jahren, die geholt werden sollen 
#' (default: NULL, d.h. alle Jahre). Wenn nur ein Wert angegeben wird,
#' so wird die Spalte `Jahr` nicht im Ergebnis ausgegeben.
#' @param Monat integer-Vektor mit den Monaten, die geholt werden sollen
#' (default: NULL, d.h. alle Monate). Wenn nur ein wert angegeben wird,
#' so wird die Spalte `Monat`nicht im Ergebnis ausgegeben.
#' @param Zeilen Angabe der Zeilen, die ausgewählt werden wollen.
#' (defautl: alle Zeilen); Möglichkeit der Einschränkung über `bquote()`
#' @param Spalten character-Vektor mit den Spalten, die geholt werden sollen
#' (default: NULL, d.h. alle Spalten, ggf. abzüglich `Jahr` und `Monat`, 
#' siehe oben) 
#'
#' @export
#' @md
#' @encoding UTF-8
#'
#' @examples
#' \dontrun{
#' sk24.2024 = loadSKFaktor(Jahr = 2024L,
#'                          Monat = 24L)
#' }
loadSKFaktor = function(path = "I:/Daten/SKFaktor/",
                        Jahr = NULL,
                        Monat = NULL,
                        Zeilen = bquote(TRUE),
                        Spalten = NULL) {
  
  # prüfen, ob Pfad existiert
  stopifnot("Datei oder Verzeichnis welches in path angegeben wurde existiert nicht" = 
              file.exists(path))
  
  
  #' @details
  #' Die Angabe von `Jahr` und `Monat` wird als Cross-Join behandelt, d.h.
  #' bei Angabe von 
  #' 
  #' * Jahr = c(2021L, 2022L, 2023L)
  #' * Monat = 24L
  #' 
  #' werden die Werte für 24.2021, 24.2022 und 24.2023 geholt.
  #' 
  #' Für weitere Verfeinerungen kann auf den Parameter `Zeilen` zurückgegriffen
  #' werden.
  #' 
  
  # Dataset öffnen
  ds = arrow::open_dataset(sources = path)
  
  # Spalten bestimmen
  Spalten = Spalten %||% ds$schema$names
  
  ZeilenIntern = Zeilen
  
  # Jahr auswählen
  if (is.integer(Jahr)) {
    ZeilenIntern = bquote(.(ZeilenIntern) &
                            Jahr %in% .(Jahr))
    
    if (length(Jahr) == 1) {
      Spalten = setdiff(Spalten, "Jahr")
    }
  } else {
    stopifnot("Jahr muss integer oder NULL sein" = is.null(Jahr))
  }
  
  # Monat auswählen
  if (is.integer(Monat)) {
    ZeilenIntern = bquote(.(ZeilenIntern) &
                            Monat %in% .(Monat))
    
    if (length(Monat) == 1) {
      Spalten = setdiff(Spalten, "Monat")
    }
  } else {
    stopifnot("Monat muss integer oder NULL sein" = is.null(Monat))
  }
  
  # Daten holen
  ds |>
    dplyr::filter(eval(ZeilenIntern)) |>
    dplyr::select(dplyr::any_of(Spalten)) |>
    dplyr::collect() |>
    setDT() -> daten
    
  #' @returns
  #' Ein `data.table` mit den gewünschten Zeilen und Spalten.
  #' 
  return(daten)
  
}


# Prüfungen:
# 
# Test auf Fehlermeldungen ----
#  * Existenz von path
#  * Jahr als integer oder NULL
#  * Monat als integer oder NULL
#  
# Test auf Funktionalität ----
# Anlegen eines Test-Verzeichnisses
#  * Standard-Ausgabe mit einem Monat und einem Jahr
#  * Standard-Ausgabe mit mehreren Jahren
#  * Spalten einschränken
#  
