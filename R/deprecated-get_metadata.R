
#' @title Get Metadata
#'
#' @description This functions stores and returns metadata on each competition.
#'
#' @param competition_ids Each competition has a unique competition_id to
#' identify it. If left as NA, the default, all competitions will be returned.
#' From here you can inspect all competition_ids for future use.
#' @param latest_european_season e.g. "2019_2020", stick to that format
#' @param output_format "list" or "tibble" default is a list
#'
#' @return A list containing the metadata on that competition.
#'
#' Each element of the output list will be named with the
#' competition_id so you can easily access the required part. Each sub-element
#' is structured as follows:
#'
#' \itemize{
#'   \item \code{competition_id}, as described above
#'   \item \code{competition_region}, typically the country the competition is
#'   played in or the region e.g. Europe, World, etc
#'   \item \code{competition_name}, full name of the competition
#'   \item \code{competition_alias}, some competitions change names and are
#'   still refered to as their old name
#'   \item \code{competition_type}, one of league, cup, friendly
#'   \item \code{competition_confederation}, uefa, concaf, etc
#'   \item \code{competition_tier}, if a league type system then this is the
#'   position of the league within the structure
#'   \item \code{competition_promoted_into}, if a league type system then this
#'   is the competition_id of the league that teams are promoted into
#'   \item \code{competition_relegated_into}, if a league type system then this
#'   is the competition_id of the league that teams are relegated into
#' }
#'
#' \cr
#' There are some other special sub-elements
#'\itemize{
#'   \item \code{football_data}, contains metadata for download results and odds
#'   data from www.football-data.co.uk
#' }
#'
#'\cr
#' Within \code{football_data} there are several sub2-elements:
#'\itemize{
#'   \item \code{url_suffix}, each league on the website has a unique code which
#'   is used to construct its url
#'   \item \code{start_season, end_season}, see \code{\link{sequence_seasons}}
#'   for season format details
#'   \item \code{data_type}, either main_league or extra_league. The website had
#'   a set of main leagues for years which are stored as individual csv files.
#'   Extra leagues were added at some point in time but are stored in a single
#'   file per sheet.
#'
#' }
#'
#' More element details will be added to metadata as data sources and coverage
#' expands.
#'
#' @seealso \link[panenkar]{sequence_seasons}
#'
#' 
#'
#' @examples
#' metadata <- get_metadata()
#' metadata_subset <- get_metadata(c("eng_pl", "eng_champ"))

get_metadata <- function(competition_ids = NA,
                         latest_european_season = "2020_2021",
                         output_format = "list"){
  
  covid_delayed_season_id <- "2019_2020"
  
  ## England -------------------------------------------------------------------
  
  eng_pl <- list(
    competition_id = "eng_pl",
    competition_region = "england",
    competition_name = "england_premier_league",
    competition_alias = "england_premiership",
    competition_type = "league",
    competition_scope = "domestic",
    competition_confederation = "uefa",
    competition_tier = 1,
    competition_promoted_into = NA,
    competition_relegated_into = "eng_champ",
    football_data = list(
      url_suffix = "E0",
      url_full = NA,
      start_season = "1993_1994",
      end_season = latest_european_season,
      data_type = "main_league"
      
    )
  )
  
  eng_champ <- eng_pl
  eng_champ$competition_id <- "eng_champ"
  eng_champ$competition_name <- "england_championship"
  eng_champ$competition_alias <- "england_division_1"
  eng_champ$competition_tier <- 2
  eng_champ$competition_promoted_into <- "eng_pl"
  eng_champ$competition_relegated_into <- "eng_l1"
  eng_champ$football_data$url_suffix <- "E1"
  
  eng_l1 <- eng_pl
  eng_l1$competition_id <- "eng_l1"
  eng_l1$competition_name <- "england_league_1"
  eng_l1$competition_alias <- "england_division_2"
  eng_l1$competition_tier <- 3
  eng_l1$competition_promoted_into <- "eng_champ"
  eng_l1$competition_relegated_into <- "eng_l2"
  eng_l1$football_data$url_suffix <- "E2"
  
  eng_l2 <- eng_pl
  eng_l2$competition_id <- "eng_l2"
  eng_l2$competition_name <- "england_league_2"
  eng_l1$competition_alias <- "england_division_3"
  eng_l2$competition_tier <- 4
  eng_l2$competition_promoted_into <- "eng_l1"
  eng_l2$competition_relegated_into <- "eng_nl"
  eng_l2$football_data$url_suffix <- "E3"
  
  eng_nl <- eng_pl
  eng_nl$competition_id <- "eng_nl"
  eng_nl$competition_name <- "england_national_league"
  eng_nl$competition_alias <- "england_conference"
  eng_nl$competition_tier <- 5
  eng_nl$competition_promoted_into <- "eng_l2"
  eng_nl$competition_relegated_into <- NA
  eng_nl$football_data$url_suffix <- "EC"
  
  ## Scotland ------------------------------------------------------------------
  
  sco_prem <- eng_pl
  sco_prem$competition_region <- "scotland"
  sco_prem$competition_id <- "sco_prem"
  sco_prem$competition_name <- "scotland_premiership"
  sco_prem$competition_alias <- "scotland_premier_league"
  sco_prem$competition_relegated_into <- "sco_champ"
  sco_prem$football_data$url_suffix <- "SC0"
  sco_prem$football_data$start_season <- "1994_1995"
  
  sco_champ <- sco_prem
  sco_champ$competition_id <- "sco_champ"
  sco_champ$competition_name <- "scotland_championship"
  sco_champ$competition_alias <- "scotland_division_1"
  sco_champ$competition_tier <- 2
  sco_champ$competition_promoted_into <- "sco_prem"
  sco_champ$competition_relegated_into <- "sco_l1"
  sco_champ$football_data$url_suffix <- "SC1"
  sco_champ$football_data$end_season <- covid_delayed_season_id
  
  sco_l1 <- sco_prem
  sco_l1$competition_id <- "sco_l1"
  sco_l1$competition_name <- "scotland_league_1"
  sco_l1$competition_alias <- "scotland_division_2"
  sco_l1$competition_tier <- 3
  sco_l1$competition_promoted_into <- "sco_champ"
  sco_l1$competition_relegated_into <- "sco_l2"
  sco_l1$football_data$url_suffix <- "SC2"
  sco_l1$football_data$start_season <- "1997_1998"
  sco_l1$football_data$end_season <- covid_delayed_season_id
  
  sco_l2 <- sco_prem
  sco_l2$competition_id <- "sco_l2"
  sco_l2$competition_name <- "scotland_league_2"
  sco_l2$competition_alias <- "scotland_division_3"
  sco_l2$competition_tier <- 4
  sco_l2$competition_promoted_into <- "sco_l1"
  sco_l2$competition_relegated_into <- NA
  sco_l2$football_data$url_suffix <- "SC3"
  sco_l2$football_data$start_season <- "1997_1998"
  sco_l2$football_data$end_season <- covid_delayed_season_id
  
  ## Germany -------------------------------------------------------------------
  
  ger_b1 <- eng_pl
  ger_b1$competition_id <- "ger_b1"
  ger_b1$competition_region <- "germany"
  ger_b1$competition_alias <- NA
  ger_b1$competition_name <- "germany_bundesliga"
  ger_b1$competition_relegated_into <- "ger_b2"
  ger_b1$football_data$url_suffix <- "D1"
  
  ger_b2 <- ger_b1
  ger_b2$competition_id <- "ger_b2"
  ger_b2$competition_region <- "germany"
  ger_b2$competition_name <- "germany_bundesliga_2"
  ger_b2$competition_promoted_into <- "ger_b1"
  ger_b2$competition_tier <- 2
  ger_b2$football_data$url_suffix <- "D2"
  
  ## Italy ---------------------------------------------------------------------
  
  ita_sa <- eng_pl
  ita_sa$competition_id <- "ita_sa"
  ita_sa$competition_region <- "italy"
  ita_sa$competition_alias <- NA
  ita_sa$competition_name <- "italy_serie_a"
  ita_sa$competition_relegated_into <- "ita_sb"
  ita_sa$football_data$url_suffix <- "I1"
  
  ita_sb <- ita_sa
  ita_sb$competition_id <- "ita_sb"
  ita_sb$competition_name <- "italy_serie_b"
  ita_sb$competition_promoted_into <- "ita_sb"
  ita_sb$competition_tier <- 2
  ita_sb$football_data$url_suffix <- "I2"
  ita_sb$football_data$start_season <- "1997_1998"
  
  ## Spain ---------------------------------------------------------------------
  
  spa_ll1 <- eng_pl
  spa_ll1$competition_id <- "spa_ll1"
  spa_ll1$competition_region <- "spain"
  spa_ll1$competition_alias <- NA
  spa_ll1$competition_name <- "spain_la_liga"
  spa_ll1$competition_relegated_into <- "spa_ll2"
  spa_ll1$football_data$url_suffix <- "SP1"
  
  spa_ll2 <- ita_sa
  spa_ll2$competition_id <- "spa_ll2"
  spa_ll2$competition_name <- "spain_la_liga_segunda"
  spa_ll2$competition_promoted_into <- "spa_ll1"
  spa_ll2$competition_tier <- 2
  spa_ll2$football_data$url_suffix <- "SP2"
  spa_ll2$football_data$start_season <- "1996_1997"
  
  ## France --------------------------------------------------------------------
  
  fra_l1 <- eng_pl
  fra_l1$competition_id <- "fra_l1"
  fra_l1$competition_region <- "france"
  fra_l1$competition_alias <- NA
  fra_l1$competition_name <- "france_ligue_1"
  fra_l1$competition_relegated_into <- "fra_l2"
  fra_l1$football_data$url_suffix <- "F1"
  
  fra_l2 <- fra_l1
  fra_l2$competition_id <- "fra_l2"
  fra_l2$competition_name <- "france_ligue_2"
  fra_l2$competition_promoted_into <- "fra_l1"
  fra_l2$competition_tier <- 2
  fra_l2$football_data$url_suffix <- "F1"
  fra_l2$football_data$start_season <- "1996_1997"
  
  ## Netherlands ---------------------------------------------------------------
  
  net_ere <- eng_pl
  net_ere$competition_id <- "net_ere"
  net_ere$competition_region <- "netherlands"
  net_ere$competition_alias <- NA
  net_ere$competition_name <- "netherlands_eredivise"
  net_ere$competition_relegated_into <- NA
  net_ere$football_data$url_suffix <- "N1"
  
  ## Belgium -------------------------------------------------------------------
  
  bel_jl <- net_ere
  bel_jl$competition_id <- "bel_jl"
  bel_jl$competition_region <- "belgium"
  bel_jl$competition_alias <- NA
  bel_jl$competition_name <- "belgium_jupiler_league"
  bel_jl$competition_relegated_into <- NA
  bel_jl$football_data$url_suffix <- "B1"
  bel_jl$football_data$start_season <- "1995_1996"
  
  ## Portugal ------------------------------------------------------------------
  
  por_pl <- net_ere
  por_pl$competition_id <- "por_pl"
  por_pl$competition_region <- "portugal"
  por_pl$competition_alias <- NA
  por_pl$competition_name <- "portugal_primeira_liga"
  por_pl$competition_relegated_into <- NA
  por_pl$football_data$url_suffix <- "P1"
  por_pl$football_data$start_season <- "1994_1995"
  
  ## Turkey --------------------------------------------------------------------
  
  tur_sl <- por_pl
  tur_sl$competition_id <- "tur_sl"
  tur_sl$competition_region <- "turkey"
  tur_sl$competition_alias <- NA
  tur_sl$competition_name <- "turkey_superliga"
  tur_sl$competition_relegated_into <- NA
  tur_sl$football_data$url_suffix <- "T1"
  tur_sl$football_data$start_season <- "1994_1995"
  
  ## Greece --------------------------------------------------------------------
  
  gre_sl <- por_pl
  gre_sl$competition_id <- "gre_sl"
  gre_sl$competition_region <- "greece"
  gre_sl$competition_alias <- NA
  gre_sl$competition_name <- "greece_superleague"
  gre_sl$competition_relegated_into <- NA
  gre_sl$football_data$url_suffix <- "G1"
  gre_sl$football_data$start_season <- "1994_1995"
  
  ## Argentina -----------------------------------------------------------------
  
  arg_pd <- por_pl
  arg_pd$competition_id <- "arg_pd"
  arg_pd$competition_region <- "argentina"
  arg_pd$competition_alias <- NA
  arg_pd$competition_name <- "argentina_primera_division"
  arg_pd$competition_confederation <- "conmebol"
  arg_pd$competition_relegated_into <- NA
  arg_pd$football_data$url_suffix <-  NA
  arg_pd$football_data$start_season <- NA
  arg_pd$football_data$end_season <- NA
  arg_pd$football_data$data_type <- "extra_league"
  arg_pd$football_data$url_full <- "https://www.football-data.co.uk/new/ARG.csv"
  
  ## Austria -------------------------------------------------------------------
  
  aus_bl <- arg_pd
  aus_bl$competition_id <- "aus_bl"
  aus_bl$competition_region <- "austria"
  aus_bl$competition_alias <- NA
  aus_bl$competition_name <- "austria_bundesliga"
  aus_bl$competition_confederation <- "uefa"
  aus_bl$football_data$data_type <- "extra_league"
  aus_bl$football_data$url_full <- "https://www.football-data.co.uk/new/AUT.csv"
  
  ## Brazil --------------------------------------------------------------------
  
  bra_sa <- arg_pd
  bra_sa$competition_id <- "bra_sa"
  bra_sa$competition_region <- "brazil"
  bra_sa$competition_alias <- NA
  bra_sa$competition_name <- "brazil_serie_a"
  bra_sa$football_data$data_type <- "extra_league"
  bra_sa$football_data$url_full <- "https://www.football-data.co.uk/new/BRA.csv"
  
  ## China ---------------------------------------------------------------------
  
  chi_sl <- arg_pd
  chi_sl$competition_id <- "chi_sl"
  chi_sl$competition_region <- "china"
  chi_sl$competition_alias <- NA
  chi_sl$competition_name <- "china_superleague"
  chi_sl$competition_confederation <- "afc"
  chi_sl$football_data$data_type <- "extra_league"
  chi_sl$football_data$url_full <- "https://www.football-data.co.uk/new/CHN.csv"
  
  ## Denmark -------------------------------------------------------------------
  
  den_sl <- aus_bl
  den_sl$competition_id <- "den_sl"
  den_sl$competition_region <- "denmark"
  den_sl$competition_alias <- NA
  den_sl$competition_name <- "denmark_superleague"
  den_sl$football_data$data_type <- "extra_league"
  den_sl$football_data$url_full <- "https://www.football-data.co.uk/new/DNK.csv"
  
  ## Finland -------------------------------------------------------------------
  
  fin_vl <- aus_bl
  fin_vl$competition_id <- "fin_vl"
  fin_vl$competition_region <- "finland"
  fin_vl$competition_alias <- NA
  fin_vl$competition_name <- "finland_veikkausliiga"
  fin_vl$football_data$data_type <- "extra_league"
  fin_vl$football_data$url_full <- "https://www.football-data.co.uk/new/FIN.csv"
  
  ## Ireland -------------------------------------------------------------------
  
  ire_pd <- aus_bl
  ire_pd$competition_id <- "ire_pd"
  ire_pd$competition_region <- "ireland"
  ire_pd$competition_alias <- NA
  ire_pd$competition_name <- "ireland_premier_division"
  ire_pd$football_data$data_type <- "extra_league"
  ire_pd$football_data$url_full <- "https://www.football-data.co.uk/new/IRL.csv"
  
  ## Japan ---------------------------------------------------------------------
  
  jap_jl <- chi_sl
  jap_jl$competition_id <- "jap_jl"
  jap_jl$competition_region <- "japan"
  jap_jl$competition_alias <- NA
  jap_jl$competition_name <- "japan_jleague"
  jap_jl$football_data$data_type <- "extra_league"
  jap_jl$football_data$url_full <- "https://www.football-data.co.uk/new/JPN.csv"
  
  ## Mexico --------------------------------------------------------------------
  
  mex_lm <- aus_bl
  mex_lm$competition_id <- "mex_lm"
  mex_lm$competition_region <- "mexico"
  mex_lm$competition_alias <- NA
  mex_lm$competition_name <- "mexico_liga_mexico"
  mex_lm$football_data$data_type <- "extra_league"
  mex_lm$football_data$url_full <- "https://www.football-data.co.uk/new/MEX.csv"
  mex_lm$competition_confederation <- "concacaf"
  
  ## Norway --------------------------------------------------------------------
  
  nor_es <- aus_bl
  nor_es$competition_id <- "nor_es"
  nor_es$competition_region <- "norway"
  nor_es$competition_alias <- NA
  nor_es$competition_name <- "norway_eliteserien"
  nor_es$football_data$data_type <- "extra_league"
  nor_es$football_data$url_full <- "https://www.football-data.co.uk/new/NOR.csv"
  
  ## Poland --------------------------------------------------------------------
  
  pol_ek <- aus_bl
  pol_ek$competition_id <- "pol_ek"
  pol_ek$competition_region <- "poland"
  pol_ek$competition_alias <- NA
  pol_ek$competition_name <- "poland_ekstraklasa"
  pol_ek$football_data$data_type <- "extra_league"
  pol_ek$football_data$url_full <- "https://www.football-data.co.uk/new/POL.csv"
  
  ## Romania -------------------------------------------------------------------
  
  rom_l1 <- aus_bl
  rom_l1$competition_id <- "rom_l1"
  rom_l1$competition_region <- "romania"
  rom_l1$competition_alias <- NA
  rom_l1$competition_name <- "romania_league_1"
  rom_l1$football_data$data_type <- "extra_league"
  rom_l1$football_data$url_full <- "https://www.football-data.co.uk/new/ROU.csv"
  
  ## Russia --------------------------------------------------------------------
  
  rus_pl <- aus_bl
  rus_pl$competition_id <- "rus_pl"
  rus_pl$competition_region <- "russia"
  rus_pl$competition_alias <- NA
  rus_pl$competition_name <- "russia_premier_league"
  rus_pl$football_data$data_type <- "extra_league"
  rus_pl$football_data$url_full <- "https://www.football-data.co.uk/new/RUS.csv"
  
  ## Sweden --------------------------------------------------------------------
  
  swe_as <- aus_bl
  swe_as$competition_id <- "swe_as"
  swe_as$competition_region <- "sweden"
  swe_as$competition_alias <- NA
  swe_as$competition_name <- "sweden_allsvenskan"
  swe_as$football_data$data_type <- "extra_league"
  swe_as$football_data$url_full <- "https://www.football-data.co.uk/new/SWE.csv"
  
  ## Switzerland ---------------------------------------------------------------
  
  swi_sl <- aus_bl
  swi_sl$competition_id <- "swi_sl"
  swi_sl$competition_region <- "switzerland"
  swi_sl$competition_alias <- NA
  swi_sl$competition_name <- "switzerland_superleague"
  swi_sl$football_data$data_type <- "extra_league"
  swi_sl$football_data$url_full <- "https://www.football-data.co.uk/new/SWE.csv"
  
  ## USA -----------------------------------------------------------------------
  
  usa_mls <- mex_lm
  usa_mls$competition_id <- "usa_mls"
  usa_mls$competition_region <- "usa"
  usa_mls$competition_alias <- NA
  usa_mls$competition_name <- "usa_major_league_soccer"
  usa_mls$football_data$data_type <- "extra_league"
  usa_mls$football_data$url_full <- "https://www.football-data.co.uk/new/USA.csv"
  
  ## Output --------------------------------------------------------------------
  
  metadata <- list(eng_pl = eng_pl,
                   eng_champ = eng_champ,
                   eng_l1 = eng_l1,
                   eng_l2 = eng_l2,
                   sco_prem = sco_prem,
                   sco_champ = sco_champ,
                   sco_l1 = sco_l1,
                   sco_l2 = sco_l2,
                   ger_b1 = ger_b1,
                   ger_b2 = ger_b2,
                   ita_sa = ita_sa,
                   ita_sb = ita_sb,
                   spa_ll1 = spa_ll1,
                   spa_ll2 = spa_ll2,
                   fra_l1 = fra_l1,
                   fra_l2 = fra_l2,
                   net_ere = net_ere,
                   bel_jl = bel_jl,
                   por_pl = por_pl,
                   tur_sl = tur_sl,
                   gre_sl = gre_sl,
                   arg_pd = arg_pd,
                   aus_bl = aus_bl,
                   bra_sa = bra_sa,
                   chi_sl = chi_sl,
                   den_sl = den_sl,
                   fin_vl = fin_vl,
                   ire_pd = ire_pd,
                   jap_jl = jap_jl,
                   mex_lm = mex_lm,
                   nor_es = nor_es,
                   pol_ek = pol_ek,
                   rom_l1 = rom_l1,
                   rus_pl = rus_pl,
                   swe_as = swe_as,
                   swi_sl = swi_sl,
                   usa_mls = usa_mls)
  
  competition_ids <- unique(competition_ids)
  
  if(!is.na(competition_ids)){
    bad_ids <- competition_ids[!(competition_ids %in% names(metadata))]
    
    if(length(bad_ids) > 0){
      
      paste0("bad competition_ids supplied: ",
             paste(bad_ids, collapse = ", "),
             ".\n Must be one of: ",
             paste(names(metadata), collapse = ", ")) %>%
        stop()
      
    }
  }
  
  
  if(!is.na(competition_ids)){
    metadata <- metadata[[competition_ids]]
  }
  
  if(output_format == "tibble"){
    metadata <- as_tibble_metadata(metadata)
  } else if(output_format != "list"){
    warning("bad output_format supplied, defaulting to list")
  }
  
  return(metadata)
}

#' @title As Tibble Metadata
#'
#' @description Helper function - turns the metadata list into a tibble
#'
#' @param metadata see ?get_metadata
#'
#' @examples
#' metadata_tibble <- get_metadata() %>% as_tibble_metadata()

as_tibble_metadata <- function(metadata){
  map_dfr(metadata, as_tibble_metadata_competition)
}


#' @title As Tibble Metadata Competition
#'
#' @description Helper - turns a competition element from metadata into a tibble
#' which can then be mapped into a tibble with map_dfr
#'
#' @param metadata_competition list element of metadata
#'
#' @examples
#' metadata_tibble <- get_metadata()[[1]] %>% as_tibble_metadata()

as_tibble_metadata_competition <- function(metadata_competition){
  
  metadata_tibble <- metadata_competition %>%
    as_tibble() %>%
    unnest(cols = football_data) %>%
    mutate(wide_names = paste0("football_data.",
                               names(metadata_competition$football_data))) %>%
    pivot_wider(names_from = wide_names, values_from = football_data)
}
