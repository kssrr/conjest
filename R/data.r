#' Conjoint Experiment - Identity-Based Trust in Bosnia and Herzegovina
#'
#' **Note:** Still needs a reference!
#' 
#' A dataset containing responses from a conjoint survey experiment on 
#' identity-based trust conducted in Bosnia and Herzegovina. Respondents had to 
#' choose between two profiles which person/profile they would rather trust in
#' specific situations. Respondents were given presented with two hypothetical
#' situations:
#' 
#' * **Seek-help scenario:** *Suppose you are walking down a busy street in the
#'  afternoon. Someone approaches you and asks if they could borrow your phone to 
#'  make a call. I will now show you descriptions of different people. If you were 
#'  in such a situation, who would you rather hand your phone to: 
#'  person A or person B?*
#'  
#' * **Offer-help scenario:** *Suppose you are traveling on a train in the 
#'  afternoon and you need to use the restroom. You have heavy luggage with you
#'  that is difficult to carry. I will now show you descriptions of different people. 
#'  If you were in such a situation, who would you rather ask to look after your 
#'  belongings: person A or person B?*
#'  
#' For each scenario, respondents had to complete three choice tasks. 
#' They were always presented with two randomly generated profiles, and 
#' asked to choose which person they would prefer to trust in the given situation,
#' and to rate how much they trusted each person.
#' 
#' The design was fully randomized. Attributes and levels were:
#' * **Age:** 23, 49, 67
#' * **Class:** Lower, Middle, Upper
#' * **Education:** None, Highschool, University
#' * **Ethnicity:** Bosniak, Croat, Serb
#' * **Sex:** Female, Male
#' 
#' @format A data frame with 23,960 rows and 20 columns:
#' \describe{
#'  \item{uuid}{Respondent identifier}
#'  \item{scenario}{Situation/scenario; 1 = Seek-help scenario, 2 = Offer-help scenario}
#'  \item{round}{Iteration of the scenario (1-3)}
#'  \item{profile}{Whether the profile was presented as "A" or "B"}
#'  \item{rating}{Trust rating assigned to the profile by the respondent (1-7)}
#'  \item{ethnicity}{Ethnicity shown on the profile}
#'  \item{group}{Whether the profile was the same ethnic group as the respondent or not ("Ingroup" or "Outgroup")}
#'  \item{age}{Age shown on the profile}
#'  \item{class}{Class shown on the profile}
#'  \item{education}{Education level shown on the profile}
#'  \item{sex}{Sex shown on the profile (Male or Female)}
#'  \item{choice}{Profile the respondent selected in that round ("A" or "B")}
#'  \item{selected}{Main outcome: whether the given profile was selected}
#'  \item{prc_id}{Identifier of the sampling location}
#'  \item{resp_age}{Age of the respondent}
#'  \item{resp_sex}{Sex of the respondent}
#'  \item{resp_ethn}{Ethnicity of the respondent}
#'  \item{resp_educ}{Highest education level attained by respondent}
#'  \item{stratum}{Sampling stratum}
#'  \item{weight}{Design weight}
#' }
