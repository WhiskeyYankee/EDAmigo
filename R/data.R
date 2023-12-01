#' finance
#'
#' A subset of Lending Club data which documents customer loan information
#'
#' @format
#' A data frame with 42,537 rows and 54 columns:
#' \describe{
#'   \item{id, member_id}{loan and member identifiers}
#'   \item{loan_amnt}{listed amount of the loan applied for by the borrower}
#'   \item{funded_amnt}{total amount committed to that loan at that point in time}
#'   \item{funded_amnt_inv}{total amount committed by investors for that loan at that point in time}
#'   \item{term}{number of payments on the loan,either 36 or 60 months}
#'   \item{int_rate}{interest rate on the loan}
#'   \item{installment}{monthly payment owed by the borrower if the loan originates}
#'   \item{grade}{LendingClub assigned loan grade}
#'   \item{sub_grade}{LendingClub assigned loan subgrade}
#'   \item{emp_title}{ job title supplied by the Borrower when applying for the loan}
#'   \item{emp_length}{employment length in years with possible values between 0 and 10 where 0 means less than one year and 10 means ten or more years}
#'   \item{home_ownership}{home ownership status provided by the borrower, values are RENT, OWN, MORTGAGE, OTHER}
#'   \item{annual_inc}{self-reported annual income provided by the borrower during registration.}
#'   \item{verification_status}{indicates if income was verified by LendingClub, not verified, or if the income source was verified}
#'   \item{issue_d}{month which the loan was funded}
#'   \item{loan_status}{current status of the loan}
#'   \item{pymnt_plan}{indicates if a payment plan has been put in place for the loan}
#'   \item{purpose}{a category of purpose provided by the borrower for the loan request}
#'   \item{zip_code}{first 3 numbers of the zip code provided by the borrower in the loan application}
#'   \item{addr_state}{state provided by the borrower in the loan application}
#'   \item{dti}{ratio calculated using borrower total monthly debt payments on the total debt obligations, excluding mortgage and the requested LendingClub loan, divided by the borrower self-reported monthly income}
#'   \item{delinq_2yrs}{number of 30+ days past-due incidences of delinquency in the borrower's credit file for the past 2 years}
#'   \item{earliest_cr_line}{month borrower earliest reported credit line was opened}
#'   \item{inq_last_6mths}{ number of inquiries in past 6 months (excluding auto and mortgage inquiries)}
#'   \item{mths_since_last_delinq}{number of months since borrower last delinquency}
#'   \item{mths_since_last_record}{number of months since the last public record}
#'   \item{open_acc}{number of open credit lines in the borrower's credit file}
#'   \item{pub_rec}{number of derogatory public records}
#'   \item{revol_bal}{total credit revolving balance}
#'   \item{revol_util}{revolving line utilization rate, or the amount of credit the borrower is using relative to all available revolving credit}
#'   \item{total_acc}{total number of credit lines currently in the borrower's credit file}
#'   \item{initial_list_status}{initial listing status of the loan, possible values are w, f}
#'   \item{out_prncp}{remaining outstanding principal for total amount funded}
#'   \item{out_prncp_inv}{remaining outstanding principal for portion of total amount funded by investors}
#'   \item{total_pymnt}{payments received to date for total amount funded}
#'   \item{total_pymnt_inv}{payments received to date for portion of total amount funded by investors}
#'   \item{total_rec_prncp}{principal received to date}
#'   \item{total_rec_int}{interest received to date}
#'   \item{total_rec_late_fee}{late fees received to date}
#'   \item{recoveries}{post charge off gross recovery}
#'   \item{collection_recovery_fee}{post charge off collection fee}
#'   \item{last_pymnt_d}{last month payment was received}
#'   \item{last_pymnt_amnt}{last total payment amount received}
#'   \item{next_pymnt_d}{next scheduled payment date}
#'   \item{last_credit_pull_d}{most recent month LendingClub pulled credit for this loan}
#'   \item{collections_12_mths_ex_med}{number of collections in 12 months excluding medical collections}
#'   \item{policy_code}{publicly available, policy_code=1}
#'   \item{application_type}{indicates whether the loan is an individual application or a joint application with two co-borrowers}
#'   \item{acc_now_delinq}{number of accounts on which the borrower is now delinquent}
#'   \item{chargeoff_within_12_mths}{number of charge-offs within 12 months}
#'   \item{delinq_amnt}{past-due amount owed for the accounts on which the borrower is now delinquent}
#'   \item{pub_rec_bankruptcies}{number of public record bankruptcies}
#'   \item{tax_liens}{number of tax liens}
#'   ...
#' }
#' @source <https://www.lendingclub.com/company/media-center.>
#'
#'
"finance"

#' clean_finance
#'
#'Cleaned finance, to be deleted.
#'
#' @format
#' A data frame with 7,240 rows and 60 columns:
#' \describe{
#'   \item{id, member_id}{loan and member identifiers}
#'   \item{loan_amnt}{listed amount of the loan applied for by the borrower}
#'   \item{funded_amnt}{total amount committed to that loan at that point in time}
#'   \item{funded_amnt_inv}{total amount committed by investors for that loan at that point in time}
#'   ...
#' }
#' @source <https://data-nifc.opendata.arcgis.com/datasets/nifc::wildland-fire-incident-locations/about.>
#'
#'
"clean_finance"

#' fires
#'
#' A subset of the National Interagency Fire Center Wildland Fire Incident Locations data set, from Columbus,OH and Denver, CO which are on similar latitudes.
#'
#' @format
#' A data frame with 623 rows and 30 columns:
#' \describe{
#'   \item{Lat}{latitude of fire center}
#'   \item{Long}{longitude of fire center}
#'   \item{Region}{D = Denver,CO;  C = Columbus, OH}
#'   \item{UniqueFireIdentifier}{unique identification code for each fire}
#'   \item{ContainmentDateTime}{date and time of fire containment}
#'   \item{ControlDateTime}{date and time of control of fire}
#'   \item{IncidentSize}{number of acres of incident}
#'   \item{DispatchCenterID}{identification code of the dispatching center}
#'   \item{FireCause}{overall category of the cause of the fire: human, natural, undetermined, unknown}
#'   \item{FireCauseGeneral}{general cause of fire within the specified overall cateogory}
#'   \item{FireCauseSpecific}{specific cause of the fire}
#'   \item{FireDepartmentID}{unique identifier of the responding fire department}
#'   \item{FireDiscoveryDateTime}{date and time of fire discovery}
#'   \item{FireOutDateTime}{date and time of fire out}
#'   \item{IncidentName}{name of the fire incident, if applicable}
#'   \item{IncidentShortDescription}{description of the fire incident}
#'   \item{IncidentTypeCategory}{type of fire, wf = wildfire; rx = prescribed fire}
#'   \item{InitialResponseAcres}{an estimate of acres burning at the time of initial response, when the IC arrives and performs initial size up.The estimate should include number of acres within the current perimeter of a specific, individual incident, including unburned and unburnable islands}
#'   \item{InitialResponseDateTime}{date and time of initial response}
#'   \item{IsMultiJurisdictional}{indicates if the incident covers multiple jurisdictions}
#'   \item{IsReimbursable}{indicates the cost of an incident may be another agency’s responsibility}
#'   \item{LocalIncidentIdentifier}{number or code that uniquely identifies an incident for a particular local fire management organization within a particular calendar year}
#'   \item{POOCounty}{county name identifying the county or equivalent entity at point of origin designated at the time of collection}
#'   \item{POODispatchCenterID}{unique identifier for the dispatch center that intersects with the incident point of origin}
#'   \item{POOFips}{code which uniquely identifies counties and county equivalents.  The first two digits are the FIPS State code and the last three are the county code within the state}
#'   \item{POOJurisdictionalUnit}{NWCG Unit Identifier to identify the unit with jurisdiction for the land where the point of origin falls.}
#'   \item{POOLandownerCategory}{more specific classification of land ownership within land owner kinds identifying the deeded owner at the point of origin at the time of the incident}
#'   \item{POOLandownerKind}{broad classification of land ownership identifying the deeded owner at the point of origin at the time of the incident}
#'   \item{POOProtectingUnit}{NWCG Unit responsible for providing direct incident management and services to a an incident pursuant to its jurisdictional responsibility or as specified by law, contract or agreement}
#'   \item{POOState}{state alpha code identifying the state or equivalent entity at point of origin}
#'   ...
#' }
#' @source <https://data-nifc.opendata.arcgis.com/datasets/nifc::wildland-fire-incident-locations/about.>
#'
#'
"fires"



#' cleaned_fires
#'
#' Cleaned version of fires. To be deleted.
#'
#' @format
#' A data frame with 623 rows and 30 columns:
#' \describe{
#'   \item{Lat}{latitude of fire center}
#'   \item{Long}{longitude of fire center}
#'   \item{Region}{D = Denver,CO;  C = Columbus, OH}
#'   \item{UniqueFireIdentifier}{unique identification code for each fire}
#'   \item{ContainmentDateTime}{date and time of fire containment}
#'   \item{ControlDateTime}{date and time of control of fire}
#'   \item{IncidentSize}{number of acres of incident}
#'   \item{DispatchCenterID}{identification code of the dispatching center}
#'   \item{FireCause}{overall category of the cause of the fire: human, natural, undetermined, unknown}
#'   \item{FireCauseGeneral}{general cause of fire within the specified overall cateogory}
#'   \item{FireCauseSpecific}{specific cause of the fire}
#'   \item{FireDepartmentID}{unique identifier of the responding fire department}
#'   \item{FireDiscoveryDateTime}{date and time of fire discovery}
#'   \item{FireOutDateTime}{date and time of fire out}
#'   \item{IncidentName}{name of the fire incident, if applicable}
#'   \item{IncidentShortDescription}{description of the fire incident}
#'   \item{IncidentTypeCategory}{type of fire, wf = wildfire; rx = prescribed fire}
#'   \item{InitialResponseAcres}{an estimate of acres burning at the time of initial response, when the IC arrives and performs initial size up.The estimate should include number of acres within the current perimeter of a specific, individual incident, including unburned and unburnable islands}
#'   \item{InitialResponseDateTime}{date and time of initial response}
#'   \item{IsMultiJurisdictional}{indicates if the incident covers multiple jurisdictions}
#'   \item{IsReimbursable}{indicates the cost of an incident may be another agency’s responsibility}
#'   \item{LocalIncidentIdentifier}{number or code that uniquely identifies an incident for a particular local fire management organization within a particular calendar year}
#'   \item{POOCounty}{county name identifying the county or equivalent entity at point of origin designated at the time of collection}
#'   \item{POODispatchCenterID}{unique identifier for the dispatch center that intersects with the incident point of origin}
#'   \item{POOFips}{code which uniquely identifies counties and county equivalents.  The first two digits are the FIPS State code and the last three are the county code within the state}
#'   \item{POOJurisdictionalUnit}{NWCG Unit Identifier to identify the unit with jurisdiction for the land where the point of origin falls.}
#'   \item{POOLandownerCategory}{more specific classification of land ownership within land owner kinds identifying the deeded owner at the point of origin at the time of the incident}
#'   \item{POOLandownerKind}{broad classification of land ownership identifying the deeded owner at the point of origin at the time of the incident}
#'   \item{POOProtectingUnit}{NWCG Unit responsible for providing direct incident management and services to a an incident pursuant to its jurisdictional responsibility or as specified by law, contract or agreement}
#'   \item{POOState}{state alpha code identifying the state or equivalent entity at point of origin}
#'   ...
#' }
#' @source <https://data-nifc.opendata.arcgis.com/datasets/nifc::wildland-fire-incident-locations/about.>
#'
#'
"cleaned_fires"
