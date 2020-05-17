#' Form 1 from a real credentialing dataset
#'
#' This is a subset of Form 1 from a real credentialing dataset provided by Cizek and Wollack (2017). 
#' It has 1,636 test takers and their response to 30 items. The dataset also includes the 
#' unique individual IDs in the first column and the unique center IDs in the second column. 
#' Column 3 to Column 32 includes the dichotomous item responses to 30 items.
#'
#' @format A data frame with 1,636 rows and 32 columns.
#' \describe{
#'   \item{EID}{unique person ID variable}
#'   \item{cent_id}{unique test center ID variable}
#'   \item{iraw.A1}{dichotomous item score for item A1}
#'   \item{iraw.A2}{dichotomous item score for item A2}
#'   \item{iraw.A3}{dichotomous item score for item A3}
#'   \item{iraw.A4}{dichotomous item score for item A4}
#'   \item{iraw.A5}{dichotomous item score for item A5}
#'   \item{iraw.A6}{dichotomous item score for item A6}
#'   \item{iraw.A7}{dichotomous item score for item A7}
#'   \item{iraw.A8}{dichotomous item score for item A8}
#'   \item{iraw.A9}{dichotomous item score for item A9}
#'   \item{iraw.A10}{dichotomous item score for item A10}
#'   \item{iraw.A11}{dichotomous item score for item A11}
#'   \item{iraw.A12}{dichotomous item score for item A12}
#'   \item{iraw.A13}{dichotomous item score for item A13}
#'   \item{iraw.A14}{dichotomous item score for item A14}
#'   \item{iraw.A15}{dichotomous item score for item A15}
#'   \item{iraw.A16}{dichotomous item score for item A16}
#'   \item{iraw.A17}{dichotomous item score for item A17}
#'   \item{iraw.A18}{dichotomous item score for item A18}
#'   \item{iraw.A19}{dichotomous item score for item A19}
#'   \item{iraw.A20}{dichotomous item score for item A20}
#'   \item{iraw.A21}{dichotomous item score for item A21}
#'   \item{iraw.A22}{dichotomous item score for item A22}
#'   \item{iraw.A23}{dichotomous item score for item A23}
#'   \item{iraw.A24}{dichotomous item score for item A24}
#'   \item{iraw.A25}{dichotomous item score for item A25}
#'   \item{iraw.A26}{dichotomous item score for item A26}
#'   \item{iraw.A27}{dichotomous item score for item A27}
#'   \item{iraw.A28}{dichotomous item score for item A28}
#'   \item{iraw.A29}{dichotomous item score for item A29}
#'   \item{iraw.A30}{dichotomous item score for item A30}
#' }
#' 
#' @source Cizek, G. J., & Wollack, J. A. (Eds.). (2017). Handbook of quantitative methods 
#' for detecting cheating on tests. New York, NY: Routledge.

"form1"

#' Form 2 of a real credentialing dataset
#'
#' This is a subset of Form 2 from a real credentialing dataset provided by Cizek and Wollack (2017). 
#' It has 1,607 test takers and their response to 30 items. The dataset also includes the 
#' unique individual IDs in the first column and the unique center IDs in the second column. 
#' Column 3 to Column 32 includes the nominal item responses to 30 items.
#'
#' @format A data frame with 1,607 rows and 32 columns.
#' \describe{
#'   \item{EID}{unique person ID variable}
#'   \item{cent_id}{unique test center ID variable}
#'   \item{iresp.B1}{nominal item score for item B1}
#'   \item{iresp.B2}{nominal item score for item B2}
#'   \item{iresp.B3}{nominal item score for item B3}
#'   \item{iresp.B4}{nominal item score for item B4}
#'   \item{iresp.B5}{nominal item score for item B5}
#'   \item{iresp.B6}{nominal item score for item B6}
#'   \item{iresp.B7}{nominal item score for item B7}
#'   \item{iresp.B8}{nominal item score for item B8}
#'   \item{iresp.B9}{nominal item score for item B9}
#'   \item{iresp.B10}{nominal item score for item B10}
#'   \item{iresp.B11}{nominal item score for item B11}
#'   \item{iresp.B12}{nominal item score for item B12}
#'   \item{iresp.B13}{nominal item score for item B13}
#'   \item{iresp.B14}{nominal item score for item B14}
#'   \item{iresp.B15}{nominal item score for item B15}
#'   \item{iresp.B16}{nominal item score for item B16}
#'   \item{iresp.B17}{nominal item score for item B17}
#'   \item{iresp.B18}{nominal item score for item B18}
#'   \item{iresp.B19}{nominal item score for item B19}
#'   \item{iresp.B20}{nominal item score for item B20}
#'   \item{iresp.B21}{nominal item score for item B21}
#'   \item{iresp.B22}{nominal item score for item B22}
#'   \item{iresp.B23}{nominal item score for item B23}
#'   \item{iresp.B24}{nominal item score for item B24}
#'   \item{iresp.B25}{nominal item score for item B25}
#'   \item{iresp.B26}{nominal item score for item B26}
#'   \item{iresp.B27}{nominal item score for item B27}
#'   \item{iresp.B28}{nominal item score for item B28}
#'   \item{iresp.B29}{nominal item score for item B29}
#'   \item{iresp.B30}{nominal item score for item B30}
#' }
#' 
#' @source Cizek, G. J., & Wollack, J. A. (Eds.). (2017). Handbook of quantitative methods 
#' for detecting cheating on tests. New York, NY: Routledge.

"form2"



#'Exam responses from a mastery test for a graduate class
#'
#'This data set involves exam responses from a postgraduate class in business 
#'studies. Cheating was suspected by one of the exam proctors. When confronted 
#'with evidence, two students confessed that they had cheated by sharing answers 
#'during the administration of the test.
#'
#'Key response vector is:
#'
#'3343233133 1221411113 4224232323 4323341411 4244232422 3232113313
#'
#' @format A data frame with 500 rows and 61 columns.
#' \describe{
#'   \item{ID}{unique person ID variable}
#'   \item{P1}{nominal item score for P1}
#'   \item{P2}{nominal item score for P2}
#'   \item{P3}{nominal item score for P3}
#'   \item{P4}{nominal item score for P4}
#'   \item{P5}{nominal item score for P5}
#'   \item{P6}{nominal item score for P6}
#'   \item{P7}{nominal item score for P7}
#'   \item{P8}{nominal item score for P8}
#'   \item{P9}{nominal item score for P9}
#'   \item{P10}{nominal item score for P10}
#'   \item{P11}{nominal item score for P11}
#'   \item{P12}{nominal item score for P12}
#'   \item{P13}{nominal item score for P13}
#'   \item{P14}{nominal item score for P14}
#'   \item{P15}{nominal item score for P15}
#'   \item{P16}{nominal item score for P16}
#'   \item{P17}{nominal item score for P17}
#'   \item{P18}{nominal item score for P18}
#'   \item{P19}{nominal item score for P19}
#'   \item{P20}{nominal item score for P20}
#'   \item{P21}{nominal item score for P21}
#'   \item{P22}{nominal item score for P22}
#'   \item{P23}{nominal item score for P23}
#'   \item{P24}{nominal item score for P24}
#'   \item{P25}{nominal item score for P25}
#'   \item{P26}{nominal item score for P26}
#'   \item{P27}{nominal item score for P27}
#'   \item{P28}{nominal item score for P28}
#'   \item{P29}{nominal item score for P29}
#'   \item{P30}{nominal item score for P30}
#'   \item{P31}{nominal item score for P31}
#'   \item{P32}{nominal item score for P32}
#'   \item{P33}{nominal item score for P33}
#'   \item{P34}{nominal item score for P34}
#'   \item{P35}{nominal item score for P35}
#'   \item{P36}{nominal item score for P36}
#'   \item{P37}{nominal item score for P37}
#'   \item{P38}{nominal item score for P38}
#'   \item{P39}{nominal item score for P39}
#'   \item{P40}{nominal item score for P40}
#'   \item{P41}{nominal item score for P41}
#'   \item{P42}{nominal item score for P42}
#'   \item{P43}{nominal item score for P43}
#'   \item{P44}{nominal item score for P44}
#'   \item{P45}{nominal item score for P45}
#'   \item{P46}{nominal item score for P46}
#'   \item{P47}{nominal item score for P47}
#'   \item{P48}{nominal item score for P48}
#'   \item{P49}{nominal item score for P49}
#'   \item{P50}{nominal item score for P50}
#'   \item{P51}{nominal item score for P51}
#'   \item{P52}{nominal item score for P52}
#'   \item{P53}{nominal item score for P53}
#'   \item{P54}{nominal item score for P54}
#'   \item{P55}{nominal item score for P55}
#'   \item{P56}{nominal item score for P56}
#'   \item{P57}{nominal item score for P57}
#'   \item{P58}{nominal item score for P58}
#'   \item{P59}{nominal item score for P59}
#'   \item{P60}{nominal item score for P60}
#' }
#' 
#' @source \url{http://lertap5.com/HTMLHelp/Lrtp59HTML/index.html?negocios.htm}

"negocios"

