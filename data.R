#' Data From **mmbruteluck** - Lab Experiment
#'
#' A dataset from the lab experiment in the appendix
#' of the paper "Choice and personalresponsibility: What is a
#' morally relevant choice?" by Alexander W. Cappelen, Sebastian
#' Fest, Erik Ø. Sørensen, and Bertil Tungodden.
#'
#' @format A data frame with 422 observations and 15 variables.
#' \describe{
#' \item{pid}{Non-informative identifier of individual participant}
#' \item{location}{Experiment location (U of Bergen or NHH). 1: UoB, 2: NHH.}
#' \item{sesid}{Id of session (not chronological)}
#' \item{T}{Treatment. 1: No Choice, 2: Nominal Choice, 3: Forced Choice}
#' \item{lotteryposition}{Lotterychoice or allocation (character). G: Green ball, S: Safe alternative, Y: Yellow ball }
#' \item{transfer}{Transfer from Lucky to Unlucky (0-800, in NOK).}
#' \item{payment}{Final payment received, showup fee inclusive (100-900, in NOK).}
#' \item{kull}{Program of study. 1: Bachelor, 2: Master, 9: Other program.}
#' \item{sex}{Gender of participant.  \describe{
#' \item{1}{male}
#' \item{2}{female}
#' }}
#' \item{age}{Age (in years)}
#' \item{polparty}{Party Voted in election? \describe{
#' \item{1}{SV}
#' \item{2}{Ap}
#' \item{3}{Sp}
#' \item{4}{KrF}
#' \item{5}{V}
#' \item{6}{H}
#' \item{7}{FrP}
#' }}
#' \item{cr}{Cognitive reflection score (0-3).}
#' \item{cr1}{Cognitive reflection 1: Ball and a bat..., 1 if correct.}
#' \item{cr2}{Cognitive reflection 2: 5m/5/5min, what about 100m..., 1 if correct.}
#' \item{cr3}{Cognitive reflection 3: 48 days to cover pond..., 1 if correct.}
#' }
#' @source Experiment conducted by the authors of the paper.
"bldata"

