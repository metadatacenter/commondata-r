.onLoad <- function(libname, pkgname) {
  
  # REST APIs
  DCAPI <- "http://api.datacommons.org/"
  DCAPI_STAT_ALL <<- paste0(DCAPI, "stat/all")
  DCAPI_STAT_SET <<- paste0(DCAPI, "stat/set")
  
  # Return payload keywords
  DATA <<- "data"
  PLACE_DATA <<- "placeData"
  STATVAR_DATA <<- "statVarData"
  SOURCE_SERIES <<- "sourceSeries"
  VAL <<- "val"
  VALUE <<- "value"
  STAT <<- "stat"
  DATE <<- "date"
  METADATA <<- "metadata"
  MEASUREMENT_METHOD <<- "measurementMethod"
  IMPORT_NAME <<- "importName"
  PROVENANCE_DOMAIN <<- "provenanceDomain"
  PROVENANCE_URL <<- "provenanceUrl"
  UNIT <<- "unit"
  
  # Statistical variables group
  CENSUS_SEX_CATEGORIES <<- c(
    "Male",
    "Female")
  
  CENSUS_PERSON_AGE_GROUPS <<- c(
    "Upto5Years",
    "5To9Years",
    "10To14Years",
    "15To17Years",
    "18To19Years",
    "20Years",
    "21Years",
    "22To24Years",
    "25To29Years",
    "30To34Years",
    "35To39Years",
    "40To44Years",
    "45To49Years",
    "50To54Years",
    "55To59Years",
    "60To61Years",
    "62To64Years",
    "65To66Years",
    "67To69Years",
    "70To74Years",
    "75To79Years",
    "80To84Years",
    "85OrMoreYears"
  )
  
  CENSUS_PERSON_WITH_RACE_AGE_GROUPS <<- c(
    "Upto5Years",
    "5To9Years",
    "10To14Years",
    "15To17Years",
    "18To19Years",
    "20To24Years",
    "25To29Years",
    "30To34Years",
    "35To44Years",
    "45To54Years",
    "55To64Years",
    "65To74Years",
    "75To84Years",
    "85OrMoreYears"
  )
  
  CENSUS_PERSON_WITH_SCHOOL_EDUCATION_AGE_GROUPS <<- c(
    "3To4Years",
    "5To9Years",
    "10To14Years",
    "15To17Years",
    "18To19Years",
    "20To24Years",
    "25To34Years",
    "35OrMoreYears"
  )
  
  CENSUS_PERSON_WITH_COLLEGE_EDUCATION_AGE_GROUPS <<- c(
    "15To17Years",
    "18To24Years",
    "25To34Years",
    "35OrMoreYears"
  )
  
  CENSUS_PERSON_WITH_INCOME_AGE_GROUPS <<- c(
    "15OrMoreYears",
    "16OrMoreYears",
    "25OrMoreYears"
  )
  
  CENSUS_HOUSEHOLDER_AGE_GROUPS <<- c(
    "Under25Years",
    "25To44Years",
    "45To64Years",
    "65OrMoreYears"
  )
  
  CENSUS_SCHOOL_ENROLLMENT_GROUPS <<- c(
    "PrivateSchool",
    "PublicSchool"
  )
  
  CENSUS_COLLEGE_ENROLLMENT_GROUPS <<- c(
    "PrivateCollegeOrGraduateSchool",
    "PublicCollegeOrGraduateSchool"
  )
  
  CENSUS_RACE_GROUPS <<- c(
    "WhiteAlone",
    "BlackOrAfricanAmericanAlone",
    "AmericanIndianOrAlaskaNativeAlone",
    "AsianAlone",
    "NativeHawaiianOrOtherPacificIslanderAlone",
    "SomeOtherRaceAlone",
    "TwoOrMoreRaces",
    "WhiteAloneNotHispanicOrLatino",
    "HispanicOrLatino"
  )
  
  CENSUS_INCOME_BRACKETS <<- c(
    "10000To14999USDollar",
    "15000To19999USDollar",
    "20000To24999USDollar",
    "25000To29999USDollar",
    "30000To34999USDollar",
    "35000To39999USDollar",
    "40000To44999USDollar",
    "45000To49999USDollar",
    "50000To59999USDollar",
    "60000To74999USDollar",
    "75000To99999USDollar",
    "100000To124999USDollar",
    "125000To149999USDollar",
    "150000To199999USDollar",
    "200000OrMoreUSDollar"
  )
  
  COVID19_INCIDENT_CASES <<- c(
    "ConfirmedCase",
    "ConfirmedOrProbableCase",
    "PatientHospitalized",
    "PatientInICU",
    "PatientOnVentilator",
    "PatientRecovered",
    "PatientDeceased"
  )
  
  # US States
  ALABAMA <<- c("Alabama")
  ALASKA <<- c("Alaska")
  ARIZONA <<- c("Arizona")
  ARKANSAS <<- c("Arkansas")
  CALIFORNIA <<- c("California")
  COLORADO <<- c("Colorado")
  CONNECTICUT <<- c("Connecticut")
  DELAWARE <<- c("Delaware")
  FLORIDA <<- c("Florida")
  GEORGIA <<- c("Georgia")
  HAWAII <<- c("Hawaii")
  IDAHO <<- c("Idaho")
  ILLINOIS <<- c("Illinois")
  INDIANA <<- c("Indiana")
  IOWA <<- c("Iowa")
  KANSAS <<- c("Kansas")
  KENTUCKY <<- c("Kentucky")
  LOUISIANA <<- c("Louisiana")
  MAINE <<- c("Maine")
  MARYLAND <<- c("Maryland")
  MASSACHUSETTS <<- c("Massachusetts")
  MICHIGAN <<- c("Michigan")
  MINNESOTA <<- c("Minnesota")
  MISSISSIPPI <<- c("Mississippi")
  MISSOURI <<- c("Missouri")
  MONTANA <<- c("Montana")
  NEBRASKA <<- c("Nebraska")
  NEVADA <<- c("Nevada")
  NEW_HAMPSHIRE <<- c("New Hampshire")
  NEW_JERSEY <<- c("New Jersey")
  NEW_MEXICO <<- c("New Mexico")
  NEW_YORK <<- c("New York")
  NORTH_CAROLINA <<- c("North Carolina")
  NORTH_DAKOTA <<- c("North Dakota")
  OHIO <<- c("Ohio")
  OKLAHOMA <<- c("Oklahoma")
  OREGON <<- c("Oregon")
  PENNSYLVANIA <<- c("Pennsylvania")
  RHODE_ISLAND <<- c("Rhode Island")
  SOUTH_CAROLINA <<- c("South Carolina")
  SOUTH_DAKOTA <<- c("South Dakota")
  TENNESSEE <<- c("Tennessee")
  TEXAS <<- c("Texas")
  UTAH <<- c("Utah")
  VERMONT <<- c("Vermont")
  VIRGINIA <<- c("Virginia")
  WASHINGTON <<- c("Washington")
  WEST_VIRGINIA <<- c("West Virginia")
  WISCONSIN <<- c("Wisconsin")
  WYOMING <<- c("Wyoming")
  
  US_STATES <<- c(ALABAMA, ALASKA, ARIZONA, ARKANSAS, CALIFORNIA,
                  COLORADO, CONNECTICUT, DELAWARE, FLORIDA, GEORGIA,
                  HAWAII, IDAHO, ILLINOIS, INDIANA, IOWA, 
                  KANSAS, KENTUCKY, LOUISIANA, MAINE, MARYLAND, 
                  MASSACHUSETTS, MICHIGAN, MINNESOTA, MISSISSIPPI, MISSOURI, 
                  MONTANA, NEBRASKA, NEVADA, NEW_HAMPSHIRE, NEW_JERSEY, 
                  NEW_MEXICO, NEW_YORK, NORTH_CAROLINA, NORTH_DAKOTA, OHIO,
                  OKLAHOMA, OREGON, PENNSYLVANIA, RHODE_ISLAND, SOUTH_CAROLINA,
                  SOUTH_DAKOTA, TENNESSEE, TEXAS, UTAH, VERMONT, 
                  VIRGINIA, WASHINGTON, WEST_VIRGINIA, WISCONSIN, WYOMING)
  
  # ZIP codes
  # City level
  ALVISO_CITY_CA <<- c("95002")
  CAMPBELL_CITY_CA <<- c("95008", "95009", "95011")
  COYOTE_CITY_CA <<- c("95013")
  CUPERTINO_CITY_CA <<- c("95014", "95015")
  GILROY_CITY_CA <<- c("95020", "95021")
  HOLY_CITY_CITY_CA <<- c("95026")
  LOS_ALTOS_CITY_CA <<- c("94022", "94023", "94024")
  LOS_GATOS_CITY_CA <<- c("95030", "95031", "95032")
  MILPITAS_CITY_CA <<- c("95035", "95036")
  MORGAN_HILL_CITY_CA <<- c("95037", "95038")
  MOUNT_HAMILTON_CITY_CA <<- c("95140")
  MOUNTAIN_VIEW_CITY_CA <<- c("94035", "94039", "94040", "94041", "94042",
                              "94043")
  NEW_ALMADEN_CITY_CA <<- c("95042")
  PALO_ALTO_CITY_CA <<- c("94301", "94302", "94303", "94304", "94306", "94309")
  REDWOOD_ESTATES_CITY_CA <<- c("95044")
  SAN_JOSE_CITY_CA <<- c("95101", "95103", "95106", "95108", "95109", "95110",
                         "95111", "95112", "95113", "95115", "95116", "95117",
                         "95118", "95119", "95120", "95121", "95122", "95123",
                         "95124", "95125", "95126", "95127", "95128", "95129",
                         "95130", "95131", "95132", "95133", "95134", "95135",
                         "95136", "95138", "95139", "95141", "95148", "95150",
                         "95151", "95152", "95153", "95154", "95155", "95156",
                         "95157", "95158", "95159", "95160", "95161", "95164",
                         "95170", "95172", "95173", "95190", "95191", "95192",
                         "95193", "95194", "95196")
  SAN_MARTIN_CITY_CA <<- c("95046")
  SANTA_CLARA_CITY_CA <<- c("95050", "95051", "95052", "95053", "95054", 
                            "95055", "95056")
  SARATOGA_CITY_CA <<- c("95070", "95071")
  STANFORD_CITY_CA <<- c("94305")
  SUNNYVALE_CITY_CA <<- c("94085", "94086", "94087", "94088", "94089")
  
  # County level
  SANTA_CLARA_COUNTY_CA <<- c(ALVISO_CITY_CA, 
                              CAMPBELL_CITY_CA, 
                              COYOTE_CITY_CA,
                              CUPERTINO_CITY_CA,
                              GILROY_CITY_CA,
                              HOLY_CITY_CITY_CA,
                              LOS_ALTOS_CITY_CA,
                              LOS_GATOS_CITY_CA,
                              MILPITAS_CITY_CA,
                              MORGAN_HILL_CITY_CA,
                              MOUNT_HAMILTON_CITY_CA,
                              MOUNTAIN_VIEW_CITY_CA,
                              NEW_ALMADEN_CITY_CA,
                              PALO_ALTO_CITY_CA,
                              REDWOOD_ESTATES_CITY_CA,
                              SAN_JOSE_CITY_CA,
                              SAN_MARTIN_CITY_CA,
                              SANTA_CLARA_CITY_CA,
                              SARATOGA_CITY_CA,
                              STANFORD_CITY_CA,
                              SUNNYVALE_CITY_CA)
}