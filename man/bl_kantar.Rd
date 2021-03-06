% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{bl_kantar}
\alias{bl_kantar}
\title{Data from mmbruteluck - Online Experiment, spectator decisions}
\format{
A data frame with 7124 observations and 22 variables:
\describe{
\item{Respondent_Serial}{Non-informative identifier of individual participant}
\item{comp}{Did the person complete the experiment (1: yes, 2: no)}
\item{treatment}{Treatment name (characters)}
\item{treatment_kantar}{Treatment names as from kantar (characters)}
\item{treatmentgroup}{Grouping of treatments into "Base", "Forced Choice" and "Nominal Choice")}
\item{y}{Transfer to the unlucky (numeric)}
\item{workp}{Was there a work requirement in this treatment? (logical)}
\item{org_strictp}{Indicator for treatment mapping 1:1 to lab experiment (logical)}
\item{org_weakp}{Indicator for treatment being in lab experiment, but possibly without a work requirement (logical)}
\item{cr1}{First cognitive reflection question (0/1)}
\item{cr2}{Second cognitive reflection question (0/1)}
\item{cr3}{Third cognitive reflection question (0/1)}
\item{crt}{Cognitive reflection score (0-3)}
\item{understanding1}{First understanding question, characters ("Ja" (yes), "Nei" (no), "Vet ikke" (don't know))}
\item{understanding2}{Second understanding question, characters}
\item{age}{Age in years (numeric)}
\item{gender}{Gender, characters ("Kvinne": female, "Mann": male)}
\item{education}{Education (characters)
  \describe{
  \item{Grunnskole}{Primary school}
  \item{Fagutdanning}{Vocational}
  \item{Videregaaende}{High school}
  \item{Universitet/hoyskole I}{Some university / college}
  \item{Universitet/hoyskole II}{University / college}
  }
  }
\item{indincome}{Individual income bracket (characters)}
\item{occupation}{Occupation (characters)}
\item{stortingsvalg}{Vote in last election
\describe{
 \item{R}{"Rødt" ("Red", far left)}
 \item{SV}{"Sosialistisk Venstreparti" ("socialist left")}
 \item{Ap}{"Arbeiderpartiet" ("Labour")}
 \item{MDG}{"Miljøpartiet De Grønne" ("Environmental Green Party")}
 \item{Sp}{"Senterpartiet" ("Center party" (farm and rural party))}
 \item{KrF}{"Kristelig Folkeparti" ("Christian democrats", centrist)}
 \item{Kp}{"Kystpartiet" ("Coast party")}
 \item{V}{"Venstre" ("Left", European liberal party, centrist )}
 \item{H}{"Høyre" ("Right", conservative party)}
 \item{Frp}{"Fremskrittspartiet" ("Progress party", right-populist party)}
 \item{Andre partier og lister}{"Some other party"}
 \item{Vil ikke oppgi parti"}{"Don't want to report"}
 \item{Stemte ikke}{"Did not vote"}
 \item{Hadde ikke stemmerett"}{"Did not have a right to vote"}
 \item{Husker ikke/vet ikke"}{"Don't remember/don't know"}
}}
\item{leftp}{Code for not voting to the right (logical, recode of "stortingsvalg")}
}
}
\source{
Experiment conducted by the authors of the paper.
}
\usage{
bl_kantar
}
\description{
A dataset from the online experiment in the paper
"Choice and personal responsibility: What is a
morally relevant choice?" by Alexander W. Cappelen, Sebastian
Fest, Erik Ø. Sørensen, and Bertil Tungodden.
}
\keyword{datasets}
