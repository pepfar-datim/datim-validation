runValidation <- function(d,
datasets = NA,
organisationUnit = NA,
datastream = NA,
d2session = dynGet("d2_default_session", inherits = TRUE)) {

#Exact duplicates
d <- getExactDuplicates(d)
#Check orgunits are within users hierarchy
d <- checkOrgunitsInHierarchy(d, userOrgUnit = organisationUnit, d2session = d2session)

#Check data element cadence
 d <- checkDataElementCadence(d, d2_session)

# Check Data element disagg validity

d <- checkDataElementDisaggValidity(d, datasets = datasets, d2session = d2session)

#Check Data element orgunit validity

d <- checkDataElementOrgunitValidity(d, datasets = datasets, d2session = d2session)

#Check mechanism valdity

d <- checkMechanismValidity(d, organisationUnit = organisationUnit, d2session =  d2session)

if (datastream == "MER") {
    #Negative values
    d <- checkNegativeValues(d, d2session = d2session)
    #Value type compliance
    d <- checkValueTypeCompliance(d, d2session = d2session)

}


}