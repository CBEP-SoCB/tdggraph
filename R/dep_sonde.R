#' Profile data collected by Maine Department of Environmental Protection
#'
#' A dataset containing vertical profile data collected by automated water
#' quality sonde by Maine's Department of Environmental Protection staff.  Data
#' has been filtered down to only include data from one site and one year.
#' Currently, the data is from Site "FR09", just outside the mouth of the Fore
#' River, from 2018.  The site and year may change between versions of this
#' package.
#'
#' Data includes temperature, salinity, dissolved oxygen, pH, chlorophyll A and
#' turbidity. Data was collected by lowering automated water quality sensor
#' apparatus from a boat.  As the device is lowered, it periodically collects
#' data on depth and water quality parameters.  Lack of discrete samples to
#' calibrate optical estimates of chlorophyll A mean the chlorophyll data
#' should be treated as provisional.
#'
#' @docType data
#' @keywords datasets
#' @name dep_sonde
#' @usage data(dep_sonde)
#'
#' @format A data frame with 108 rows and 16 variables:
#' \describe{
#'   \item{site}{Site Code. A unique character string for different locations.
#'               Retained her for documentation purposes, but will include only
#'               one value.}
#'   \item{sample_date}{Date of sample collection (as and R Date).}
#'   \item{month}{Month of sample collection, as a factor. Uses three letter
#'                codes.}
#'   \item{time}{Time of sample collection, as an R `hms` object.}
#'   \item{depth}{Depth of readings, in meters.}
#'   \item{temp}{Water temperature (C).}
#'   \item{salinity}{Salinity in "Practical Salinity Units" (roughly PPT).}
#'   \item{ph}{pH value, measured with a electrochemical pH meter, thus
#'             on the "NBS" scale.  This value can differ from the "Total" pH
#'             scale often used in ocean acidification research. See, for
#'             example, Pimenta and Greer. 2018.  Guidelines for measuring
#'             changes in pH and associated carbonate chemistry in coastal
#'             environments of the Eastern United States.  Atlantic Ecology
#'             Division, National Health and Environmental Effects Research
#'             Laboratory, US Environmental Protection Agency, Narragansett, RI.
#'             EPA/600/R-17/483 available at
#'             <http://necan.org/sites/default/files/EPA600_R17_483coastalAcidificationMonitoringGuidelinesFinal%20041318%20%282%29_0.pdf>.}
#'   \item{pctsat}{Oxygen percent saturation.}
#'   \item{do}{Dissolved oxygen (mg/l).}
#'   \item{chl_a_sonde}{Chlorophyll a, (mg/l), measured by a fluorometer.
#'       All data is marked with data quality flags as "estimate".  For high
#'       accuracy work, *in situ* flourometric analysis of chlorophyll should be
#'       calibrated after data collection against laboratory chlorophyll values.
#'       These data reflect only the uncorrected field estimates.}
#'   \item{turbidity}{Turbidity measurements or detection limits (NTU).}
#'   \item{turbidity_cens}{True / False.  Was this turbidity measurement below
#'       detection? If so, the value in `turbidity` is the detection limit.}
#'   }
#' @source Maine Department of Environmental Protection, via Casco Bay Estuary
#'     Partnership's (CBEP) 2020 State of Casco Bay Report. Original data
#'     received by CBEP from Angie Brewer, Marine Scientist for Maine DEP in
#'     response to a request for data to be used in the 2020 State of Casco Bay
#'     Report. CBEP staff reviewed, simplified and reorganized the data  while
#'     preparing the report. Additional information on the report and associated
#'     data and data analysis archives are available on the CBEP web site at
#'     <https://cascobayestuary.org>.
"dep_sonde"
