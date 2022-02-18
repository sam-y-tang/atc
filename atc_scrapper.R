################################################################################
# Anatomical Therapeutic Chemcical Classificaiton System
#
# -- Level 1: main -- one letter
# -- level 2: therapeutic subgroup -- two digits
# -- level 3: therapeutic/pharmacological subgroup -- one letter
# -- level 4: chemical/therapeutic/pharmacological subgroup -- one letter
# -- level 5: chemical substance -- two digits

################################################################################
###                    WHO Collaborating Centre Scrapper                     ###
# For a given main ATC group (or subgroup) download code, name, and metadata for
# the code and children
#
# @param code a ATC CODE
# @return a data.frame
whocc_scrapper <- function(code) {
  code <- toupper(code)
  stopifnot(nchar(code) %in% c(1, 3, 4, 5, 7))
  stopifnot(
    substr(code, 1, 1) %in% c("A", "B", "C", "D", "G", "H", "J",
                              "L", "M", "N", "P", "R", "S", "V")
  )

  target_url <-
    paste0("https://www.whocc.no/atc_ddd_index/?code=", code,
           "&showdescription=no")

  message(paste("Scrapping", target_url))
  site <- readLines(con = target_url)

  if (any(grepl("Invalid code", site))) {
    rtn <- data.frame("ATC.code" = code,
                      "Name" = "INVALID CODE",
                      "DDD" = NA_real_,
                      "U" = NA_character_,
                      "Adm.R" = NA_character_,
                      "Note" = NA_character_)
    return(rtn)
  } else {
    # pattern to get ATC label for the given code
    pattern <- paste0(".+<a.*code=", code, "&showdescription=no\">(.+)</a>.+")
    label <- sub(pattern, "\\1", grep(pattern, site, value = TRUE))
    rtn <- data.frame("ATC.code" = code,
                      "Name" = label,
                      "DDD" = NA_real_,
                      "U" = NA_character_,
                      "Adm.R" = NA_character_,
                      "Note" = NA_character_)

    if (nchar(code) %in% c(1)) {
      child_pattern <-
        paste0(".+<a.*code=(", code, "[0-9][0-9])&.*>([A-Z]+.+)</a>.+")
    } else if (nchar(code) %in% c(3, 4))  {
      child_pattern <-
        paste0(".+<a.*code=(", code, "[A-Z])&.*>([A-Z]+.+)</a>.+")
    } else if (nchar(code) == 5) {
      if (any(grepl("<table", site))) {
        tab <- paste(site[seq(grep("<table[^>]+>", site),
                              grep("</table>", site), by = 1)],
                     collapse = "")
        rows <- length(gregexpr("<\\/tr>", tab)[[1]])
        cols <- length(gregexpr("<\\/td>", tab)[[1]]) / rows
        m <- gregexec("<td[^>]*>(.*?)</\\s*td>", tab)
        cells <- regmatches(tab, m)[[1]][2, ]
        cells <- gsub("&nbsp;", "", cells)
        cells <- sub("<a[^>]+>(.*?)</a>", "\\1", cells)
        cells <-
          matrix(
                 cells[-seq(cols, 1, by = -1)]
                 , byrow = TRUE
                 , dimnames = list(NULL,
                                   make.names(cells[seq(1, cols, by = 1)]))
                 , nrow = rows - 1
                 , ncol = cols
          )
        cells <- as.data.frame(cells)
        cells["DDD"][cells["DDD"] == ""] <- NA_character_
        cells["U"][cells["U"] == ""] <- NA_character_
        cells["Adm.R"][cells["Adm.R"] == ""] <- NA_character_
        cells["Note"][cells["Note"] == ""] <- NA_character_
        while (length(i <- which(cells["ATC.code"] == ""))) {
          cells[i[1], "ATC.code"] <- cells[i[1] - 1, "ATC.code"]
          cells[i[1], "Name"] <- cells[i[1] - 1, "Name"]
        }
        return(rbind(rtn, cells))
      } else {
        return(NULL)
      }
    } else {
      stop("unexpected nchar(code)")
    }

    child_codes <-
      sub(child_pattern, "\\1", grep(child_pattern, site, value = TRUE))
  }
 do.call(rbind, c(list(rtn), lapply(child_codes, whocc_scrapper)))
}


################################################################################
###                               ATC Scrapper                               ###
#
# The structure of the webpage requires a call to whocc_scrapper for each main
# ATC code of interest.  atc_scrapper is a wrapper for this.
#
atc_scrapper <- function(codes = "ALL") {
  if (length(codes) == 1 & all(codes == "ALL")) {
    codes <- c("A", "B", "C", "D", "G", "H", "J",
               "L", "M", "N", "P", "R", "S", "V")
  }

  do.call(rbind, lapply(codes, whocc_scrapper))
}

################################################################################
###                               Download ATC                               ###
# download the atc codes
atc_codes <- atc_scrapper()
# write to a .csv
write.csv(atc_codes, file = "atccodes.csv", row.names = FALSE)

################################################################################
###                           List of Abbrevations                           ###
#
# Unit (U)                        | Route of administration (Adm.R)
# --------                        | -------------------------------
# g = gram                        | Implant = Implant
# mg   milligram                  | Inhal =  Inhalation
# mcg = microgram                 | Instill =  Instillation
# U = unit                        | N = nasal
# TU = thousand units             | O = oral
# MU = million units              | P = parenteral
# mmol = millimole                | R = rectal
# ml = milliliter (e.g. eyedrops) | SL =  sublingual/buccal/oromucosal
#                                 | TD = transdermal
#                                 | V = vaginal
#
################################################################################
###                               End of File                                ###
################################################################################

