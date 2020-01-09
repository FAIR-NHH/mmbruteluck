#' Data from mmbruteluck - Lab Experiment
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
#' \itemize{
#'  \item 1. University of Bergen
#'  \item 2. NHH Norwegian School of Economics }}
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


#' Data from mmbruteluck - Online Experiment, spectator decisions
#'
#' A dataset from the online experiment in the paper
#' "Choice and personal responsibility: What is a
#' morally relevant choice?" by Alexander W. Cappelen, Sebastian
#' Fest, Erik Ø. Sørensen, and Bertil Tungodden.
#'
#' @format A data frame with 7124 observations and 22 variables:
#' \describe{
#' \item{Respondent_Serial}{Non-informative identifier of individual participant}
#' \item{comp}{Did the person complete the experiment (1: yes, 2: no)}
#' \item{treatment}{Treatment name (characters)}
#' \item{treatment_kantar}{Treatment names as from kantar (characters)}
#' \item{treatmentgroup}{Grouping of treatments into "Base", "Forced Choice" and "Nominal Choice")}
#' \item{y}{Transfer to the unlucky (numeric)}
#' \item{workp}{Was there a work requirement in this treatment? (logical)}
#' \item{org_strictp}{Indicator for treatment mapping 1:1 to lab experiment (logical)}
#' \item{org_weakp}{Indicator for treatment being in lab experiment, but possibly without a work requirement (logical)}
#' \item{cr1}{First cognitive reflection question (0/1)}
#' \item{cr2}{Second cognitive reflection question (0/1)}
#' \item{cr3}{Third cognitive reflection question (0/1)}
#' \item{crt}{Cognitive reflection score (0-3)}
#' \item{understanding1}{First understanding question, characters ("Ja" (yes), "Nei" (no), "Vet ikke" (don't know))}
#' \item{understanding2}{Second understanding question, characters}
#' \item{age}{Age in years (numeric)}
#' \item{gender}{Gender, characters ("Kvinne": female, "Mann": male)}
#' \item{education}{Education (characters)
#'   \describe{
#'   \item{Grunnskole}{Primary school}
#'   \item{Fagutdanning}{Vocational}
#'   \item{Videregaaende}{High school}
#'   \item{Universitet/hoyskole I}{Some university / college}
#'   \item{Universitet/hoyskole II}{University / college}
#'   }
#'   }
#' \item{indincome}{Individual income bracket (characters)}
#' \item{occupation}{Occupation (characters)}
#' \item{stortingsvalg}{Vote in last election
#' \describe{
#'  \item{R}{"Rødt" ("Red", far left)}
#'  \item{SV}{"Sosialistisk Venstreparti" ("socialist left")}
#'  \item{Ap}{"Arbeiderpartiet" ("Labour")}
#'  \item{MDG}{"Miljøpartiet De Grønne" ("Environmental Green Party")}
#'  \item{Sp}{"Senterpartiet" ("Center party" (farm and rural party))}
#'  \item{KrF}{"Kristelig Folkeparti" ("Christian democrats", centrist)}
#'  \item{Kp}{"Kystpartiet" ("Coast party")}
#'  \item{V}{"Venstre" ("Left", European liberal party, centrist )}
#'  \item{H}{"Høyre" ("Right", conservative party)}
#'  \item{Frp}{"Fremskrittspartiet" ("Progress party", right-populist party)}
#'  \item{Andre partier og lister}{"Some other party"}
#'  \item{Vil ikke oppgi parti"}{"Don't want to report"}
#'  \item{Stemte ikke}{"Did not vote"}
#'  \item{Hadde ikke stemmerett"}{"Did not have a right to vote"}
#'  \item{Husker ikke/vet ikke"}{"Don't remember/don't know"}
#' }}
#' \item{leftp}{Code for not voting to the right (logical, recode of "stortingsvalg")}
#' }
#' @source Experiment conducted by the authors of the paper.
"bl_kantar"

#' Data from mmbruteluck - Online Experiment, mTurk workers
#'
#' A dataset from the online experiment in the paper
#' "Choice and personal responsibility: What is a
#' morally relevant choice?" by Alexander W. Cappelen, Sebastian
#' Fest, Erik Ø. Sørensen, and Bertil Tungodden.
#'
#' @format A data frame with 2437 observations and 17 variables:
#' \describe{
#' \item{id}{Non-informative identifier of individual participant}
#' \item{female}{Indicator for mTurker being female (0/1)}
#' \item{age}{Age of mTurker in years (numeric) }
#' \item{education}{Education category
#' \describe{
#'  \item{1.}{Less than High School}
#'  \item{2.}{High School/GED  }
#'  \item{3.}{Some College  }
#'  \item{4.}{2 year College Degree }
#'  \item{5.}{4 year College Degree }
#'  \item{6.}{Masters Degree }
#'  \item{7.}{Masters Degree  }
#'  \item{8.}{Professional Degree (JD, MD)}
#' }
#' }
#' \item{political}{Political identification.Numeric, [-2, - 2],
#' with -2 being "very liberal" and 2 "very conservative.}
#' \item{treatment}{Treatment arm (characters)}
#' \item{safe}{Did the mTurker choose the safe alternative? (0/1), 1 if they
#' chose the safe alternative}
#' \item{bonus_ingame}{bonus in game (0.5USD) - 0.25 USD if worker chose
#' costly safe alternative in that treatment}
#' \item{bonus_lottery}{Outcome of lottery choice prior to redistribution (numeric, in USD)}
#' \item{bonus_final}{The sum of bonus_ingame and the post redistribution income from lottery (numeric, in USD)}
#' \item{repeater}{Has the worker attempted to start the survey more than once?
#' (0/1), 1 if they did so.}
#' \item{lottery_or_green}{Indicator for whether the worker chose the lottery in Forced Choice treatments or the
#' green ball in Nominal Choice treatments (0/1), 1 if they dis so.}
#' \item{duration_survey}{Duration of HIT completion (numeric, in seconds)}
#' \item{treatment_str}{Treatment (characters)}
#' \item{bonus_startup}{Bonus account at startup (numeric, 0.5 USD)}
#' \item{winner}{Indicator for whether the worker is a lottery winner (0/1).}
#' \item{loser}{Indicator for whether the worker is a lottery loser (0/1).}
#' }
#' @source Experiment conducted by the authors of the paper.
"mturk_workers"
