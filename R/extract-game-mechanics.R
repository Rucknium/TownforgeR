#' Construct influence effects matrix data from Townforge C++ file
#'
#' Description
#'
#' @param source.dir directory of Townforge source
#' @param ... TODO
#'
#' @details Construct influence effects matrix
#'
#' @export
tf_get_influence_effects <- function(source.dir = "https://git.townforge.net/townforge/townforge/raw/branch/cc", ...) {
  
  source.dir <- gsub("/+$", "", source.dir)
  
  cc_influence.v <- readLines(paste0(source.dir, "/src/cc/cc_influence.cpp"))
  
  start.influence_rules <- grep("static const Rules influence_rules[NUM_ROLES]", cc_influence.v, fixed = TRUE)
  dimnames.influence_rules <- cc_influence.v[ - (1:start.influence_rules) ]
  dimnames.influence_rules <- dimnames.influence_rules[nchar(gsub(" ", "", dimnames.influence_rules)) > 20][1]
  dimnames.influence_rules <- gsub(" *// +", "", dimnames.influence_rules)
  dimnames.influence_rules <- gsub(" +", " ", dimnames.influence_rules)
  dimnames.influence_rules <- unlist(strsplit(dimnames.influence_rules, " "))
  dimnames.influence_rules <- c("EMPTY", dimnames.influence_rules)
  
  cc_influence.v <- cc_influence.v[gsub(" ", "", cc_influence.v) != "" & 
                                     ! grepl("^ *//", cc_influence.v)]
  # Remove all empty lines. //// removes the "//" comments
  
  start.influence_rules <- grep("static const Rules influence_rules[NUM_ROLES]", cc_influence.v, fixed = TRUE)
  
  end.influence_rules <- which(gsub(" ", "", cc_influence.v) == "};")
  end.influence_rules <- min(end.influence_rules[end.influence_rules > start.influence_rules])
  cc_influence.v <- cc_influence.v[start.influence_rules:end.influence_rules][-1]
  cc_influence.v <- cc_influence.v[nchar(cc_influence.v) > 20]
  
  cc_influence.txt <- tempfile()
  cat(file = cc_influence.txt, cc_influence.v, sep = "\n")
  cc_infl.mat <- suppressMessages(suppressWarnings(readr::read_fwf(cc_influence.txt, readr::fwf_empty(cc_influence.txt))))
  unlink(cc_influence.txt)
  cc_infl.mat <- as.matrix(cc_infl.mat)
  # attr(cc_infl.mat, "spec") <- NULL # get rid of tibble attributes
  cc_infl.mat <- cc_infl.mat[, (-1) * c(1, ncol(cc_infl.mat))]
  cc_infl.mat <- apply(cc_infl.mat, 2, FUN = function(x) {
    gsub("[{},]", "", x)
  })
  cc_infl.mat <- rbind("0", cc_infl.mat)
  # Need an extra first row for EMPTY
  
  dimnames(cc_infl.mat) <- list(dimnames.influence_rules, dimnames.influence_rules)
  
  extract.influence <- function(x, type, dimnames) {
    ret <- apply(x, 2, FUN = function(y) {
      y <- stringr::str_extract(y, paste0(type, "[(][0-9]+[)]"))
      as.numeric(gsub("[()]", "", stringr::str_extract(y, "[(][0-9]+[)]")))
    })
    dimnames(ret) <- list(dimnames, dimnames)
    ret
  }
  
  bonus.infl.mat <- extract.influence(cc_infl.mat, "BONUS", dimnames.influence_rules)
  need.infl.mat <- extract.influence(cc_infl.mat, "NEED", dimnames.influence_rules)
  penalty.infl.mat <- extract.influence(cc_infl.mat, "PENALTY", dimnames.influence_rules)
  
  list(bonus = bonus.infl.mat, need = need.infl.mat, penalty = penalty.infl.mat)
  
}









#' Construct minimum flag size data from Townforge C++ file
#'
#' Description
#'
#' @param source.dir directory of Townforge source
#' @param ... TODO
#'
#' @details Construct minimum flag size data
#'
#' @export
#'
tf_get_min_flag_size_data <- function(source.dir = "https://git.townforge.net/townforge/townforge/raw/branch/cc", ...) {
  
  source.dir <- gsub("/+$", "", source.dir)
  
  cc.v <- readLines(paste0(source.dir, "/src/cc/cc.cpp"))
  
  cc.v <- cc.v[grep("get_min_size_for_building", cc.v)[1]:length(cc.v)]
  
  cc.v <- cc.v[grepl("case ROLE_[A-Z0-9]+: return scale[(]", cc.v)]
  cc.v <- cc.v[ ! grepl("ROAD", cc.v)]
  # Remove road since it is too complicated
  building.role <- stringr::str_extract(cc.v, "ROLE_[A-Z0-9]+")
  
  cc.v <- gsub("(case ROLE_[A-Z0-9]+: return scale[(])(.+)([)];)", "\\2", cc.v)
  cc.df <- as.data.frame(apply(do.call(rbind, strsplit(cc.v, ",")), MARGIN = 2, FUN = as.numeric))
  colnames(cc.df) <- c("lowest", "highest")
  cc.df$building.role <- building.role
  min.size.scale.df <- cc.df[, c("building.role", "lowest", "highest")]
  
  min.size.scale.df
}





#' Gather English wallet mnemonic seed words list
#'
#' Description
#'
#' @param source.url directory of Townforge source
#' @param ... TODO
#'
#' @details Construct influence effects matrix
#'
#' @export
tf_get_seed_words_list <- function(source.url = "https://raw.githubusercontent.com/monero-project/monero/master/src/mnemonics/english.h", ...) {
  seed.words <- readLines(source.url)
  seed.words.begin.ind <- grep("static constexpr const char", seed.words) + 2
  seed.words.end.ind <- grep("set_words", seed.words) - 2
  seed.words <- seed.words[seed.words.begin.ind:seed.words.end.ind]
  seed.words <- gsub("[^a-z]", "", seed.words)
  seed.words
}
  

