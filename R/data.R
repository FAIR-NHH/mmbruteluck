#' Data From mmbruteluck - Lab Experiment
#'
#' A dataset from the lab experiment in the appendix
#' of the paper "Choice and personal responsibility: What is a
#' morally relevant choice?" by Alexander W. Cappelen, Sebastian
#' Fest, Erik Ø. Sørensen, and Bertil Tungodden.
#'
#' @format A data frame with 422 observations and 15 variables:
#' \describe{
#' \item{pid}{Non-informative identifier of individual participant}
#' \item{location}{Location of experiment:
#' \itemize{\item 1. University of Bergen
#' \item 2. NHH Norwegian School of Economics
#' }}
#' \item{sesid}{Id of session (not chronological)}
#' \item{T}{Treatment:
#'  \itemize{
#'  \item 1. No Choice
#'  \item 2. Nominal Choice
#'  \item 3. Forced Choice} }
#' \item{lotteryposition}{Lotterychoice or allocation (character):
#' \itemize{
#' \item G: Green ball
#' \item S: Safe alternative
#' \item Y: Yellow ball }}
#' \item{transfer}{Transfer from Lucky to Unlucky (0-800, in NOK).}
#' \item{payment}{Final payment received, showup fee inclusive (100-900, in NOK).}
#' \item{kull}{Program of study.
#' \itemize{
#' \item 1. Bachelor
#' \item 2. Master
#' \item 9. Other program} }
#' \item{sex}{Self-reported sex of participant:
#' \itemize{
#' \item 1. Male
#' \item 2. Female}}
#' \item{age}{Age (in years).}
#' \item{polparty}{Vote for in election?
#' \itemize{
#' \item 1. SV
#' \item 2. Ap
#' \item 3. Sp
#' \item 4. KrF
#' \item 5. V
#' \item 6. H
#' \item 7. FrP }}
#' \item{cr}{Cognitive reflection score (0-3).}
#' \item{cr1}{Cognitive reflection 1: Ball and a bat..., 1 if correct.}
#' \item{cr2}{Cognitive reflection 2: 5m/5/5min, what about 100m..., 1 if correct.}
#' \item{cr3}{Cognitive reflection 3: 48 days to cover pond..., 1 if correct.}
#' }
#' @source Experiment conducted by the authors of the paper.
"bldata"
